#!/usr/bin/env python3
"""Durable localhost mailbox for cooperating agents; Python standard library only."""

import argparse
import datetime
import fcntl
import hmac
import json
import os
from pathlib import Path
import re
import secrets
import signal
import sqlite3
import subprocess
import sys
import threading
import time
import urllib.error
import urllib.parse
import urllib.request
import uuid
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

SERVICE = "fas-agent-relay-v1"
MAX_BODY = 65536
MAX_WAIT = 45
NAME = re.compile(r"[A-Za-z0-9_.-]{1,64}\Z")


def json_bytes(value):
    return (json.dumps(value, ensure_ascii=False) + "\n").encode()


def state_dir(path):
    path = Path(path).expanduser().resolve()
    path.mkdir(mode=0o700, parents=True, exist_ok=True)
    return path


def token_for(path):
    target = path / "token"
    try:
        fd = os.open(target, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
    except FileExistsError:
        return target.read_text().strip()
    with os.fdopen(fd, "w") as stream:
        token = secrets.token_hex(32)
        stream.write(token + "\n")
    return token


def valid_name(value):
    if not isinstance(value, str) or not NAME.fullmatch(value):
        raise ValueError("agent names must be 1–64 letters, digits, dots, underscores or hyphens")
    return value


class Mailbox:
    def __init__(self, path):
        self.condition = threading.Condition()
        self.stopping = False
        self.db = sqlite3.connect(path, check_same_thread=False)
        self.db.row_factory = sqlite3.Row
        self.db.execute("PRAGMA journal_mode=WAL")
        self.db.execute("PRAGMA synchronous=FULL")
        self.db.executescript("""
            CREATE TABLE IF NOT EXISTS messages (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                message_key TEXT NOT NULL UNIQUE,
                sender TEXT NOT NULL,
                recipient TEXT NOT NULL,
                body TEXT NOT NULL,
                reply_to INTEGER,
                created_at TEXT NOT NULL
            );
            CREATE INDEX IF NOT EXISTS recipient_id ON messages(recipient, id);
        """)

    @staticmethod
    def message(row):
        result = dict(row)
        result["from"] = result.pop("sender")
        result["to"] = result.pop("recipient")
        return result

    def send(self, data):
        sender, recipient = valid_name(data.get("from")), valid_name(data.get("to"))
        body = data.get("body")
        if not isinstance(body, str) or not body.strip() or len(body.encode()) > MAX_BODY:
            raise ValueError("body must contain text and be at most 65536 UTF-8 bytes")
        reply = data.get("reply_to")
        if reply is not None and (type(reply) is not int or not 1 <= reply <= 2**63 - 1):
            raise ValueError("reply_to must be a positive message ID")
        key = data.get("message_key", str(uuid.uuid4()))
        if not isinstance(key, str) or not 1 <= len(key) <= 128:
            raise ValueError("message_key must be a string of 1–128 characters")
        with self.condition:
            existing = self.db.execute(
                "SELECT * FROM messages WHERE message_key = ?", (key,)
            ).fetchone()
            if existing:
                if tuple(existing[k] for k in ("sender", "recipient", "body", "reply_to")) != (
                    sender, recipient, body, reply
                ):
                    raise ValueError("message_key already belongs to a different message")
                return self.message(existing)
            if reply is not None and not self.db.execute(
                "SELECT 1 FROM messages WHERE id = ?", (reply,)
            ).fetchone():
                raise ValueError("reply_to does not exist")
            created = datetime.datetime.now(datetime.timezone.utc).isoformat()
            with self.db:
                cursor = self.db.execute(
                    "INSERT INTO messages(message_key, sender, recipient, body, reply_to, created_at) "
                    "VALUES (?, ?, ?, ?, ?, ?)",
                    (key, sender, recipient, body, reply, created),
                )
            row = self.db.execute("SELECT * FROM messages WHERE id = ?", (cursor.lastrowid,)).fetchone()
            self.condition.notify_all()
            return self.message(row)

    def receive(self, recipient, after, wait):
        valid_name(recipient)
        if not 0 <= after <= 2**63 - 1 or not 0 <= wait <= MAX_WAIT:
            raise ValueError("after must be a nonnegative signed 64-bit ID; wait must be between 0 and 45 seconds")
        deadline = time.monotonic() + wait
        with self.condition:
            while True:
                rows = self.db.execute(
                    "SELECT * FROM messages WHERE recipient = ? AND id > ? ORDER BY id LIMIT 100",
                    (recipient, after),
                ).fetchall()
                remaining = deadline - time.monotonic()
                if rows or remaining <= 0 or self.stopping:
                    return [self.message(row) for row in rows]
                self.condition.wait(remaining)

    def stop(self):
        with self.condition:
            self.stopping = True
            self.condition.notify_all()


class Server(ThreadingHTTPServer):
    daemon_threads = False
    allow_reuse_address = True

    def __init__(self, port, mailbox, token):
        self.mailbox, self.token = mailbox, token
        super().__init__(("127.0.0.1", port), Handler)

    def stop(self):
        self.mailbox.stop()
        threading.Thread(target=self.shutdown, daemon=True).start()


class Handler(BaseHTTPRequestHandler):
    def setup(self):
        super().setup()
        self.connection.settimeout(10)

    def log_message(self, *_args):
        pass

    def reply(self, status, value):
        body = json_bytes(value)
        self.send_response(status)
        self.send_header("Content-Type", "application/json; charset=utf-8")
        self.send_header("Content-Length", str(len(body)))
        self.send_header("Cache-Control", "no-store")
        self.end_headers()
        self.wfile.write(body)

    def authenticated(self):
        supplied = self.headers.get("Authorization", "").encode()
        expected = ("Bearer " + self.server.token).encode()
        if hmac.compare_digest(supplied, expected):
            return True
        self.reply(401, {"error": "unauthorized"})
        return False

    def do_GET(self):
        if not self.authenticated():
            return
        parsed = urllib.parse.urlsplit(self.path)
        if parsed.path == "/health":
            self.reply(200, {"service": SERVICE, "pid": os.getpid()})
            return
        if parsed.path != "/messages":
            self.reply(404, {"error": "unknown endpoint"})
            return
        try:
            query = urllib.parse.parse_qs(parsed.query)
            messages = self.server.mailbox.receive(
                query.get("to", [""])[0],
                int(query.get("after", ["0"])[0]),
                float(query.get("wait", ["0"])[0]),
            )
            self.reply(200, {"messages": messages})
        except ValueError as error:
            self.reply(400, {"error": str(error)})

    def do_POST(self):
        if not self.authenticated():
            return
        if self.path == "/shutdown":
            self.reply(200, {"stopping": True})
            self.server.stop()
            return
        if self.path != "/messages":
            self.reply(404, {"error": "unknown endpoint"})
            return
        try:
            if self.headers.get_content_type() != "application/json":
                raise ValueError("Content-Type must be application/json")
            length = int(self.headers.get("Content-Length", "0"))
            if not 0 < length <= MAX_BODY * 6 + 4096:
                raise ValueError("invalid request length")
            data = json.loads(self.rfile.read(length))
            if not isinstance(data, dict):
                raise ValueError("request must be a JSON object")
            self.reply(200, self.server.mailbox.send(data))
        except (ValueError, UnicodeError) as error:
            self.reply(400, {"error": str(error)})


def request(args, endpoint, data=None, timeout=5):
    token = (args.state_dir / "token").read_text().strip()
    req = urllib.request.Request(
        f"http://127.0.0.1:{args.port}{endpoint}",
        data=None if data is None else json_bytes(data),
        headers={"Authorization": "Bearer " + token, "Content-Type": "application/json"},
    )
    # Local traffic must not inherit a user's HTTP proxy configuration.
    opener = urllib.request.build_opener(urllib.request.ProxyHandler({}))
    try:
        with opener.open(req, timeout=timeout) as response:
            return json.load(response)
    except urllib.error.HTTPError as error:
        raise ValueError(json.load(error).get("error", str(error))) from error


def serve(args):
    lock = open(args.state_dir / "server.lock", "a")
    fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
    mailbox = Mailbox(args.state_dir / "messages.sqlite3")
    server = Server(args.port, mailbox, token_for(args.state_dir))
    for sig in (signal.SIGINT, signal.SIGTERM):
        signal.signal(sig, lambda *_: server.stop())
    print(f"{SERVICE} listening on 127.0.0.1:{server.server_port}", flush=True)
    try:
        server.serve_forever(poll_interval=0.1)
    finally:
        mailbox.stop()
        server.server_close()
        mailbox.db.close()
        lock.close()


def start(args):
    token_for(args.state_dir)
    try:
        health = request(args, "/health")
        if health.get("service") != SERVICE:
            raise ValueError("port belongs to a different service")
        print(json.dumps({"already_running": True, **health}))
        return
    except (OSError, urllib.error.URLError):
        pass
    log_path = args.state_dir / "server.log"
    with open(log_path, "ab") as log:
        process = subprocess.Popen(
            [sys.executable, str(Path(__file__).resolve()), "--state-dir", str(args.state_dir),
             "--port", str(args.port), "serve"],
            stdin=subprocess.DEVNULL, stdout=log, stderr=log, start_new_session=True,
        )
    for _ in range(100):
        if process.poll() is not None:
            raise ValueError(f"server failed to start; see {log_path}")
        try:
            health = request(args, "/health", timeout=0.2)
            print(json.dumps({"started": True, "log": str(log_path), **health}))
            return
        except (OSError, urllib.error.URLError):
            time.sleep(0.05)
    raise ValueError(f"server did not become ready; see {log_path}")


def checkpoint(path, after):
    temporary = path.with_name(path.name + "." + uuid.uuid4().hex + ".tmp")
    try:
        with temporary.open("x") as stream:
            stream.write(str(after) + "\n")
            stream.flush()
            os.fsync(stream.fileno())
        os.replace(temporary, path)
    finally:
        temporary.unlink(missing_ok=True)


def listen(args):
    identity = valid_name(args.identity)
    cursor_path = args.state_dir / f"cursor.{identity}"
    # One listener per identity avoids racing checkpoints. Other agents use their own names.
    with open(args.state_dir / f"listener.{identity}.lock", "a") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        after = args.after
        if after is None:
            after = int(cursor_path.read_text()) if cursor_path.exists() else 0
        if after < 0:
            raise ValueError("after must be nonnegative")
        last_message = time.monotonic()
        retry = 0.25
        while True:
            remaining = args.idle_timeout - (time.monotonic() - last_message)
            if args.idle_timeout and remaining <= 0:
                return
            wait = min(MAX_WAIT, remaining) if args.idle_timeout else MAX_WAIT
            query = urllib.parse.urlencode({"to": identity, "after": after, "wait": wait})
            try:
                messages = request(args, "/messages?" + query, timeout=wait + 5)["messages"]
            except (OSError, urllib.error.URLError) as error:
                print(f"relay unavailable; reconnecting: {error}", file=sys.stderr, flush=True)
                time.sleep(min(retry, max(0, remaining)) if args.idle_timeout else retry)
                retry = min(5, retry * 2)
                continue
            retry = 0.25
            for message in messages:
                print(json.dumps(message, ensure_ascii=False), flush=True)
                after = message["id"]
                checkpoint(cursor_path, after)
                last_message = time.monotonic()
            if args.once:
                return


def main():
    default_state = Path(os.environ.get("XDG_STATE_HOME", str(Path.home() / ".local/state")))
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--state-dir", default=str(default_state / "fas-agent-relay"))
    parser.add_argument("--port", type=int, default=8765)
    commands = parser.add_subparsers(dest="command", required=True)
    for command in ("start", "serve", "status", "stop"):
        commands.add_parser(command)
    sender = commands.add_parser("send")
    sender.add_argument("--from", dest="sender", required=True)
    sender.add_argument("--to", dest="recipient", required=True)
    text = sender.add_mutually_exclusive_group(required=True)
    text.add_argument("--body")
    text.add_argument("--file", help="UTF-8 message file, or - for stdin")
    sender.add_argument("--reply-to", type=int)
    sender.add_argument("--key", help="stable idempotency key when retrying a send")
    receiver = commands.add_parser("inbox", help="read history without advancing listener cursor")
    receiver.add_argument("--as", dest="identity", required=True)
    receiver.add_argument("--after", type=int, default=0)
    receiver.add_argument("--wait", type=float, default=0)
    listener = commands.add_parser("listen", help="print incoming JSON lines and save progress")
    listener.add_argument("--as", dest="identity", required=True)
    listener.add_argument("--after", type=int)
    listener.add_argument("--idle-timeout", type=float, default=0, help="exit after idle seconds; 0 waits indefinitely")
    listener.add_argument("--once", action="store_true", help="exit after one long-poll response")
    args = parser.parse_args()
    args.state_dir = state_dir(args.state_dir)
    os.umask(0o077)
    if not 1 <= args.port <= 65535:
        parser.error("port must be between 1 and 65535")
    if args.command == "serve":
        serve(args)
    elif args.command == "start":
        start(args)
    elif args.command in ("status", "stop"):
        print(json.dumps(request(args, "/health" if args.command == "status" else "/shutdown",
                                 None if args.command == "status" else {})))
    elif args.command == "send":
        body = args.body
        if args.file is not None:
            body = sys.stdin.read() if args.file == "-" else Path(args.file).read_text(encoding="utf-8")
        data = {"from": args.sender, "to": args.recipient, "body": body,
                "reply_to": args.reply_to, "message_key": args.key or str(uuid.uuid4())}
        print(json.dumps(request(args, "/messages", data), ensure_ascii=False))
    elif args.command == "inbox":
        query = urllib.parse.urlencode({"to": args.identity, "after": args.after, "wait": args.wait})
        print(json.dumps(request(args, "/messages?" + query, timeout=args.wait + 5), ensure_ascii=False))
    else:
        if args.idle_timeout < 0 or not args.idle_timeout < float("inf"):
            parser.error("idle-timeout must be finite and nonnegative")
        listen(args)


if __name__ == "__main__":
    try:
        main()
    except BrokenPipeError:
        os._exit(1)
    except (OSError, ValueError, urllib.error.URLError) as error:
        print(f"relay: {error}", file=sys.stderr)
        sys.exit(1)
