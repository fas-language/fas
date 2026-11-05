#!/usr/bin/env python3
"""Protocol, persistence and real subprocess listener checks."""

import concurrent.futures
import fcntl
import json
from pathlib import Path
import socket
import subprocess
import sys
import tempfile
import threading
import time
import unittest
import urllib.error
import urllib.request

import relay


SCRIPT = str(Path(relay.__file__).resolve())


class RelayTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.path = Path(self.temp.name)
        self.token = relay.token_for(self.path)
        self.launch()

    def launch(self, port=0):
        self.mailbox = relay.Mailbox(self.path / "messages.sqlite3")
        self.server = relay.Server(port, self.mailbox, self.token)
        self.thread = threading.Thread(target=self.server.serve_forever, kwargs={"poll_interval": 0.01})
        self.thread.start()
        self.port = self.server.server_port

    def halt(self):
        self.mailbox.stop()
        self.server.shutdown()
        self.thread.join(2)
        self.server.server_close()
        self.mailbox.db.close()

    def tearDown(self):
        self.halt()

    def cli(self, *args, **kwargs):
        return subprocess.run(
            [sys.executable, SCRIPT, "--state-dir", str(self.path), "--port", str(self.port), *args],
            text=True, capture_output=True, timeout=8, **kwargs,
        )

    def http(self, endpoint, data=None, token=None):
        args = type("Args", (), {"state_dir": self.path, "port": self.port})
        if token is None:
            return relay.request(args, endpoint, data)
        req = urllib.request.Request(
            f"http://127.0.0.1:{self.port}{endpoint}",
            headers={"Authorization": "Bearer " + token},
        )
        return urllib.request.urlopen(req, timeout=2)

    def send(self, recipient="claude", **extra):
        return self.http("/messages", {"from": "codex", "to": recipient, "body": "hello λ\nsecond line", **extra})

    def test_isolation_paging_replies_and_idempotency(self):
        first = self.send(message_key="stable")
        self.assertEqual(first, self.send(message_key="stable"))
        with self.assertRaisesRegex(ValueError, "different message"):
            self.send(body="different", message_key="stable")
        reply = self.send("codex", reply_to=first["id"])
        for _ in range(105):
            self.send()
        page = self.http("/messages?to=claude")["messages"]
        self.assertEqual(len(page), 100)
        self.assertNotIn(reply["id"], [m["id"] for m in page])
        tail = self.http(f"/messages?to=claude&after={page[-1]['id']}")["messages"]
        self.assertEqual(len(tail), 6)
        self.assertEqual(page[0]["body"], "hello λ\nsecond line")

    def test_long_poll_wakes_for_matching_recipient_and_times_out(self):
        with concurrent.futures.ThreadPoolExecutor() as pool:
            waiting = pool.submit(self.http, "/messages?to=claude&wait=2")
            time.sleep(0.1)
            self.send("someone_else")
            time.sleep(0.1)
            self.assertFalse(waiting.done())
            message = self.send()
            self.assertEqual(waiting.result(timeout=2)["messages"], [message])
        start = time.monotonic()
        self.assertEqual(self.http("/messages?to=empty&wait=0.1"), {"messages": []})
        self.assertGreaterEqual(time.monotonic() - start, 0.08)

    def test_authentication_and_invalid_input(self):
        with self.assertRaises(urllib.error.HTTPError) as raised:
            self.http("/health", token="wrong")
        self.assertEqual(raised.exception.code, 401)
        for changes in ({"to": "../bad"}, {"body": ""}, {"body": "λ" * 40000},
                        {"reply_to": -1}, {"reply_to": 123}, {"body": ["bad"]}):
            with self.subTest(changes=str(changes)[:100]):
                with self.assertRaises(ValueError):
                    self.send(**changes)
        for query in ("wait=nan", "wait=46", "after=-1", "after=nope", "after=99999999999999999999"):
            with self.assertRaises(ValueError):
                self.http("/messages?to=claude&" + query)

    def test_database_restart_preserves_messages(self):
        message = self.send()
        self.halt()
        self.launch()
        self.assertEqual(self.http("/messages?to=claude")["messages"], [message])
        self.assertGreater(self.send()["id"], message["id"])

    def test_listener_checkpoints_timeout_and_history_replay(self):
        self.send()
        result = self.cli("listen", "--as", "claude", "--once", "--idle-timeout", "1")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(json.loads(result.stdout)["id"], 1)
        result = self.cli("listen", "--as", "claude", "--idle-timeout", "0.15")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, "")
        self.assertEqual(len(json.loads(self.cli("inbox", "--as", "claude").stdout)["messages"]), 1)
        message = self.send()
        result = self.cli("listen", "--as", "claude", "--once")
        self.assertEqual(json.loads(result.stdout)["id"], message["id"])

    def test_concurrent_senders_have_unique_ordered_ids(self):
        with concurrent.futures.ThreadPoolExecutor(max_workers=8) as pool:
            sent = list(pool.map(lambda _: self.send(), range(40)))
        ids = [m["id"] for m in self.http("/messages?to=claude")["messages"]]
        self.assertEqual(ids, sorted(m["id"] for m in sent))
        self.assertEqual(len(set(ids)), 40)

    def test_live_listener_reconnects_and_excludes_duplicate_listener(self):
        command = [sys.executable, SCRIPT, "--state-dir", str(self.path), "--port", str(self.port),
                   "listen", "--as", "claude", "--idle-timeout", "2"]
        process = subprocess.Popen(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
        try:
            locked = False
            for _ in range(100):
                lock_path = self.path / "listener.claude.lock"
                if lock_path.exists():
                    with lock_path.open("a") as lock:
                        try:
                            fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
                        except BlockingIOError:
                            locked = True
                if locked:
                    break
                time.sleep(0.01)
            self.assertTrue(locked, "listener did not acquire its lock")
            duplicate = self.cli("listen", "--as", "claude", "--idle-timeout", "0.1")
            self.assertNotEqual(duplicate.returncode, 0)
            port = self.port
            self.halt()
            time.sleep(0.1)
            self.launch(port)
            message = self.send()
            stdout, stderr = process.communicate(timeout=5)
            self.assertEqual(process.returncode, 0, stderr)
            self.assertEqual(json.loads(stdout)["id"], message["id"])
        finally:
            if process.poll() is None:
                process.kill()
                process.communicate(timeout=3)


class DaemonTests(unittest.TestCase):
    def test_start_duplicate_start_stop_and_restart(self):
        with tempfile.TemporaryDirectory() as directory:
            with socket.socket() as sock:
                sock.bind(("127.0.0.1", 0))
                port = sock.getsockname()[1]
            command = [sys.executable, SCRIPT, "--state-dir", directory, "--port", str(port)]

            def cli(*args):
                result = subprocess.run(command + list(args), capture_output=True, text=True, timeout=8)
                self.assertEqual(result.returncode, 0, result.stderr)
                return json.loads(result.stdout)

            try:
                self.assertTrue(cli("start")["started"])
                self.assertTrue(cli("start")["already_running"])
                cli("send", "--from", "codex", "--to", "claude", "--body", "persist me")
                pid = cli("status")["pid"]
                cli("stop")
                # The old server must release its socket and lock before restart.
                for _ in range(100):
                    result = subprocess.run(command + ["status"], capture_output=True, timeout=2)
                    if result.returncode:
                        break
                    time.sleep(0.02)
                time.sleep(0.15)
                self.assertNotEqual(cli("start")["pid"], pid)
                self.assertEqual(cli("inbox", "--as", "claude")["messages"][0]["body"], "persist me")
            finally:
                subprocess.run(command + ["stop"], capture_output=True, timeout=3)


if __name__ == "__main__":
    unittest.main(verbosity=2)
