#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu
export DEBUGINFOD_URLS=

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
START_DIR=$(pwd)
CC=$(printenv CC || printf 'clang-22')
LLVM_OPT=$(printenv LLVM_OPT || printf 'opt-22')
LLVM_LLC=$(printenv LLVM_LLC || printf 'llc-22')
OCAML_FAS=$(printenv OCAML_FAS || printf '%s/_build/default/bin/main.exe' "$ROOT")

resolve_tool_path() {
  case "$1" in
    /*) printf '%s\n' "$1" ;;
    */*) printf '%s/%s\n' "$START_DIR" "$1" ;;
    *)
      resolved=$(command -v "$1" 2>/dev/null || true)
      case "$resolved" in
        /*) printf '%s\n' "$resolved" ;;
        */*) printf '%s/%s\n' "$START_DIR" "$resolved" ;;
        *) printf '%s\n' "$1" ;;
      esac
      ;;
  esac
}

CC=$(resolve_tool_path "$CC")
LLVM_OPT=$(resolve_tool_path "$LLVM_OPT")
LLVM_LLC=$(resolve_tool_path "$LLVM_LLC")
OCAML_FAS=$(resolve_tool_path "$OCAML_FAS")
REAL_CC=$(command -v "$CC")
REAL_OPT=$(command -v "$LLVM_OPT")
REAL_LLC=$(command -v "$LLVM_LLC")

LSP_TMP=$(mktemp -d)
TMPDIR=$LSP_TMP/tmp
mkdir -p "$LSP_TMP/work" "$TMPDIR"
WORK=$(cd "$LSP_TMP/work" && pwd -P)
export TMPDIR
trap 'rm -rf "$LSP_TMP"' EXIT HUP INT TERM

cat >"$WORK/opt-wrap" <<'SH'
#!/bin/sh
exec "$REAL_OPT" "$@"
SH
cat >"$WORK/llc-wrap" <<'SH'
#!/bin/sh
exec "$REAL_LLC" "$@"
SH
cat >"$WORK/cc-wrap" <<'SH'
#!/bin/sh
printf '%s\n' "$*" >>"$CLANG_LOG"
exec "$REAL_CC" "$@"
SH
chmod +x "$WORK/opt-wrap" "$WORK/llc-wrap" "$WORK/cc-wrap"
: >"$WORK/clang.log"

CLANG_LOG=$WORK/clang.log
FAS_OPT=$WORK/opt-wrap
FAS_LLC=$WORK/llc-wrap
FAS_CC=$WORK/cc-wrap
export CLANG_LOG REAL_OPT REAL_LLC REAL_CC FAS_OPT FAS_LLC FAS_CC

python3 -I - "$OCAML_FAS" "$WORK" <<'PY'
import json
import os
import re
import select
import subprocess
import sys
import time
import urllib.parse

exe = sys.argv[1]
work = sys.argv[2]
clang_log = os.environ["CLANG_LOG"]
TIMEOUT = 60
servers = []


def fail(message):
    for server in servers:
        server.proc.kill()
    sys.stderr.write("lsp_protocol: %s\n" % message)
    sys.exit(1)


def expect(condition, message):
    if not condition:
        fail(message)


def raw_frame(body):
    return b"Content-Length: %d\r\n\r\n" % len(body) + body


def frame_of(message):
    return raw_frame(json.dumps(message).encode("utf-8"))


def uri_of(path):
    return "file://" + urllib.parse.quote(path, safe="/")


def write_file(path, text):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w", encoding="utf-8", newline="") as handle:
        handle.write(text)


def utf16_units(text):
    return len(text.encode("utf-16-le")) // 2


def diagnostic(text, token, message):
    prefix = text[: text.index(token)]
    line = prefix.count("\n")
    start = utf16_units(prefix.rsplit("\n", 1)[-1])
    return {
        "range": {
            "start": {"line": line, "character": start},
            "end": {"line": line, "character": start + utf16_units(token)},
        },
        "severity": 1,
        "source": "fas",
        "message": message,
    }


def error_code(reply):
    return reply.get("error", {}).get("code")


class Server:
    def __init__(self, name, trace=False):
        self.name = name
        self.stderr = open(os.path.join(work, name + ".err"), "wb")
        env = dict(os.environ)
        env.pop("FAS_LSP_TRACE", None)
        if trace:
            env["FAS_LSP_TRACE"] = "1"
        self.proc = subprocess.Popen(
            [exe, "lsp"],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=self.stderr,
            env=env,
        )
        self.buffer = b""
        self.next_id = 100
        self.replies = []
        self.publishes = []
        self.logs = []
        servers.append(self)

    def write(self, data):
        self.proc.stdin.write(data)
        self.proc.stdin.flush()

    def read_message(self):
        while True:
            end = self.buffer.find(b"\r\n\r\n")
            if end >= 0:
                length = None
                for line in self.buffer[:end].split(b"\r\n"):
                    key, _, value = line.partition(b":")
                    if key.strip().lower() == b"content-length":
                        length = int(value.strip())
                expect(length is not None, "%s: output without Content-Length" % self.name)
                start = end + 4
                if len(self.buffer) >= start + length:
                    body = self.buffer[start : start + length]
                    self.buffer = self.buffer[start + length :]
                    return json.loads(body.decode("utf-8"))
            ready, _, _ = select.select([self.proc.stdout], [], [], TIMEOUT)
            expect(ready, "%s: no output within %d seconds" % (self.name, TIMEOUT))
            chunk = os.read(self.proc.stdout.fileno(), 65536)
            expect(chunk, "%s: server closed stdout" % self.name)
            self.buffer += chunk

    def pump(self):
        message = self.read_message()
        if "method" in message:
            method = message["method"]
            if method == "textDocument/publishDiagnostics":
                params = message["params"]
                self.publishes.append((params["uri"], params["diagnostics"]))
            elif method == "window/logMessage":
                self.logs.append(message["params"]["message"])
        else:
            self.replies.append(message)

    def wait_reply(self, ident):
        while True:
            for index, reply in enumerate(self.replies):
                if reply.get("id") == ident:
                    return self.replies.pop(index)
            self.pump()

    def next_reply(self):
        while not self.replies:
            self.pump()
        return self.replies.pop(0)

    def frame_request(self, method, params=None):
        self.next_id += 1
        ident = self.next_id
        message = {"jsonrpc": "2.0", "id": ident, "method": method}
        if params is not None:
            message["params"] = params
        return ident, frame_of(message)

    def request(self, method, params=None):
        ident, data = self.frame_request(method, params)
        self.write(data)
        return self.wait_reply(ident)

    def notify(self, method, params=None):
        message = {"jsonrpc": "2.0", "method": method}
        if params is not None:
            message["params"] = params
        self.write(frame_of(message))

    def settle(self):
        reply = self.request("fas/settle")
        expect(error_code(reply) == -32601, "%s: settle got %r" % (self.name, reply))
        expect(not self.replies, "%s: unexpected replies %r" % (self.name, self.replies))
        publishes = self.publishes
        self.publishes = []
        return publishes

    def finish(self, code):
        self.notify("exit")
        status = self.proc.wait(timeout=TIMEOUT)
        expect(status == code, "%s: exit status %d, expected %d" % (self.name, status, code))


def initialize(server):
    reply = server.request(
        "initialize", {"processId": None, "rootUri": None, "capabilities": {}}
    )
    expect("result" in reply, "%s: initialize failed: %r" % (server.name, reply))
    result = reply["result"]
    expect(result["capabilities"]["textDocumentSync"] == 1, "%s: textDocumentSync" % server.name)
    expect(result["serverInfo"]["name"] == "fas", "%s: serverInfo name" % server.name)
    expect(
        re.match(r"^\d+\.\d+\.\d+", result["serverInfo"]["version"]),
        "%s: serverInfo version" % server.name,
    )
    server.notify("initialized", {})


def shutdown(server):
    reply = server.request("shutdown")
    expect("result" in reply and reply["result"] is None, "%s: shutdown %r" % (server.name, reply))


def did_open(server, path, text, version):
    server.notify(
        "textDocument/didOpen",
        {"textDocument": {"uri": uri_of(path), "languageId": "fas", "version": version, "text": text}},
    )


def did_change(server, path, text, version):
    server.notify(
        "textDocument/didChange",
        {"textDocument": {"uri": uri_of(path), "version": version}, "contentChanges": [{"text": text}]},
    )


def did_save(server, path):
    server.notify("textDocument/didSave", {"textDocument": {"uri": uri_of(path)}})


def did_close(server, path):
    server.notify("textDocument/didClose", {"textDocument": {"uri": uri_of(path)}})


def change_frame(path, text, version):
    return frame_of(
        {
            "jsonrpc": "2.0",
            "method": "textDocument/didChange",
            "params": {
                "textDocument": {"uri": uri_of(path), "version": version},
                "contentChanges": [{"text": text}],
            },
        }
    )


def hover_params(path, line, character):
    return {
        "textDocument": {"uri": uri_of(path)},
        "position": {"line": line, "character": character},
    }


def analyses_of(server, path):
    return [
        line
        for line in server.logs
        if line.startswith("analyse ") and line.endswith(os.path.basename(path))
    ]


def expect_published(publishes, path, expected, label):
    found = None
    for uri, diagnostics in publishes:
        if uri == uri_of(path):
            found = diagnostics
    expect(found is not None, "%s: no publishDiagnostics for %s" % (label, path))
    expect(found == expected, "%s: got %r, expected %r" % (label, found, expected))


def lifecycle():
    s = Server("lifecycle")
    reply = s.request("shutdown")
    expect(error_code(reply) == -32002, "request before initialize: %r" % reply)
    initialize(s)
    reply = s.request("fas/missing", {})
    expect(error_code(reply) == -32601, "unknown request: %r" % reply)
    s.write(raw_frame(b'{"jsonrpc":'))
    reply = s.next_reply()
    expect(reply.get("id") is None and error_code(reply) == -32700, "malformed JSON: %r" % reply)
    s.notify("$/cancelRequest", {"id": 5})
    shutdown(s)
    s.finish(0)


def exit_without_shutdown():
    s = Server("exit-error")
    initialize(s)
    s.finish(1)


def framing():
    s = Server("framing")
    initialize(s)
    s.write(
        frame_of({"jsonrpc": "2.0", "id": 31, "method": "fas/one"})
        + frame_of({"jsonrpc": "2.0", "id": 32, "method": "fas/two"})
    )
    third = frame_of({"jsonrpc": "2.0", "id": 33, "method": "fas/three"})
    for piece in (third[:5], third[5:20], third[20:]):
        s.write(piece)
        time.sleep(0.05)
    for ident in (31, 32, 33):
        reply = s.next_reply()
        expect(reply.get("id") == ident and error_code(reply) == -32601, "framing %d: %r" % (ident, reply))
    s.write(b"X-Other: 1\r\n\r\n")
    reply = s.next_reply()
    expect(reply.get("id") is None and error_code(reply) == -32700, "missing Content-Length: %r" % reply)
    s.write(frame_of({"jsonrpc": "2.0", "id": 34, "method": "fas/four"}))
    reply = s.next_reply()
    expect(reply.get("id") == 34 and error_code(reply) == -32601, "after malformed frame: %r" % reply)
    s.notify("$/cancelRequest", {"id": 34})
    expect(s.settle() == [], "cancelRequest produced publishes")
    shutdown(s)
    s.finish(0)


def framing_resync():
    s = Server("framing-resync")
    initialize(s)
    glued = b"Content-Length: abc\r\n\r\n{}" + frame_of({"jsonrpc": "2.0", "id": 41, "method": "fas/one"})
    s.write(glued)
    reply = s.next_reply()
    expect(reply.get("id") is None and error_code(reply) == -32700, "bad length resync: %r" % reply)
    reply = s.next_reply()
    expect(reply.get("id") == 41 and error_code(reply) == -32601, "bad length frame: %r" % reply)

    s.write(b"Content-Length: abc\r\n\r\n")
    reply = s.next_reply()
    expect(reply.get("id") is None and error_code(reply) == -32700, "bad length alone: %r" % reply)
    time.sleep(0.05)
    s.write(b"{}")
    time.sleep(0.05)
    s.write(frame_of({"jsonrpc": "2.0", "id": 42, "method": "fas/two"}))
    reply = s.next_reply()
    expect(reply.get("id") == 42 and error_code(reply) == -32601, "bad body split from frame: %r" % reply)

    split = frame_of({"jsonrpc": "2.0", "id": 43, "method": "fas/three"})
    cut = split.index(b"gth")
    s.write(b"X-Other: 1\r\n\r\n{}" + split[:cut])
    reply = s.next_reply()
    expect(reply.get("id") is None and error_code(reply) == -32700, "split keyword block: %r" % reply)
    time.sleep(0.05)
    s.write(split[cut:])
    reply = s.next_reply()
    expect(reply.get("id") == 43 and error_code(reply) == -32601, "split keyword frame: %r" % reply)

    s.write(frame_of({"jsonrpc": "2.0", "id": 99, "result": None}))
    s.write(frame_of({"jsonrpc": "2.0", "id": 98, "error": {"code": -32000, "message": "client"}}))
    s.write(frame_of({"jsonrpc": "2.0"}))
    s.write(frame_of({"jsonrpc": "2.0", "id": 44, "method": "fas/four"}))
    reply = s.next_reply()
    expect(reply.get("id") is None and error_code(reply) == -32600, "no id and no method: %r" % reply)
    reply = s.next_reply()
    expect(reply.get("id") == 44 and error_code(reply) == -32601, "responses ignored: %r" % reply)
    expect(s.settle() == [], "resync produced publishes")
    shutdown(s)
    s.finish(0)


def diagnostics():
    d = Server("diagnostics")
    initialize(d)
    folder = os.path.join(work, "diagnostics")
    main = os.path.join(folder, "main.fas")
    lib = os.path.join(folder, "lib.fas")
    valid = "fn main() i32 { return 0 } /* é😀 */\n"
    broken = "fn main() i32 { return 0 } /* é😀 */ fn f() i32 { return missing }\n"
    lib_text = "fn helper() i32 { return nope }\n"
    uses_lib = 'use "lib.fas"\nfn main() i32 { return 0 }\n'
    write_file(main, valid)

    did_open(d, main, valid, 1)
    expect_published(d.settle(), main, [], "open valid")

    did_change(d, main, broken, 2)
    expect_published(
        d.settle(),
        main,
        [diagnostic(broken, "missing", "unknown name `missing`")],
        "change to error",
    )

    did_change(d, main, valid, 3)
    did_save(d, main)
    expect_published(d.settle(), main, [], "fix")

    write_file(lib, lib_text)
    did_change(d, main, uses_lib, 4)
    publishes = d.settle()
    lib_message = "unknown name `nope`\nnote: include chain: %s -> %s" % (main, lib)
    expect_published(publishes, lib, [diagnostic(lib_text, "nope", lib_message)], "use file error")
    expect_published(publishes, main, [], "root with use file error")

    did_change(d, main, valid, 5)
    expect_published(d.settle(), lib, [], "use file cleared once no open root imports it")

    did_change(d, main, broken, 6)
    expect_published(d.settle(), main, [diagnostic(broken, "missing", "unknown name `missing`")], "error again")
    did_close(d, main)
    expect_published(d.settle(), main, [], "close clears")
    shutdown(d)
    d.finish(0)


def dependant_roots():
    d = Server("dependant_roots")
    initialize(d)
    folder = os.path.join(work, "dependant_roots")
    root = os.path.join(folder, "a.fas")
    lib = os.path.join(folder, "b.fas")
    root_text = 'use "b.fas"\nfn main() i32 { return helper(); }\n'
    lib_text = "fn helper() i32 { return 7; }\n"
    renamed = "fn support() i32 { return 7; }\n"
    missing = diagnostic(root_text, "helper()", "unknown function `helper`")
    write_file(root, root_text)
    write_file(lib, lib_text)

    did_open(d, root, root_text, 1)
    expect_published(d.settle(), root, [], "open root")

    did_open(d, lib, lib_text, 1)
    publishes = d.settle()
    expect_published(publishes, root, [], "open use file")
    expect_published(publishes, lib, [], "open use file clean")

    did_change(d, lib, renamed, 2)
    expect_published(d.settle(), root, [missing], "rename helper in open use file")

    did_change(d, lib, lib_text, 3)
    expect_published(d.settle(), root, [], "restore helper in open use file")

    write_file(lib, renamed)
    did_close(d, lib)
    publishes = d.settle()
    expect_published(publishes, root, [missing], "close use file reads disk")
    expect_published(publishes, lib, [], "close use file clears its diagnostics")

    did_close(d, root)
    expect_published(d.settle(), root, [], "close root clears")
    shutdown(d)
    d.finish(0)


def clang_calls():
    with open(clang_log, encoding="utf-8") as handle:
        return [line for line in handle if line.strip()]


def header_dumps():
    return [line for line in clang_calls() if "-fdump-record-layouts-complete" in line]


def c_cache():
    open(clang_log, "w", encoding="utf-8").close()
    c = Server("c-cache")
    initialize(c)
    folder = os.path.join(work, "c-cache")

    stdio = os.path.join(folder, "stdio.fas")
    base = 'use "C" <stdio.h>\nfn main() i32 { return 0 }\n'
    write_file(stdio, base)
    did_open(c, stdio, base, 1)
    expect_published(c.settle(), stdio, [], "stdio import opens cleanly")
    expect(len(header_dumps()) == 1, "first stdio import ran %d header dumps" % len(header_dumps()))
    calls = len(clang_calls())
    did_change(c, stdio, base + "/* first edit */\n", 2)
    expect_published(c.settle(), stdio, [], "first comment edit")
    did_change(c, stdio, base + "/* second edit */\n", 3)
    expect_published(c.settle(), stdio, [], "second comment edit")
    expect(len(header_dumps()) == 1, "comment edits re-ran the C header dump")
    expect(len(clang_calls()) == calls, "comment edits spawned %d clang processes" % (len(clang_calls()) - calls))

    header = os.path.join(folder, "local.h")
    write_file(header, "int add(int a, int b);\n")
    local = os.path.join(folder, "local.fas")
    local_text = 'use "C" "local.h"\nfn main() i32 { return 0 }\n'
    write_file(local, local_text)
    before = len(header_dumps())
    did_open(c, local, local_text, 1)
    expect_published(c.settle(), local, [], "local header opens cleanly")
    expect(len(header_dumps()) == before + 1, "local header import ran %d dumps" % (len(header_dumps()) - before))
    did_change(c, local, local_text + "/* edit */\n", 2)
    c.settle()
    expect(len(header_dumps()) == before + 1, "unchanged header re-ran the dump")
    stamp = os.stat(header)
    os.utime(header, (stamp.st_atime + 100, stamp.st_mtime + 100))
    did_change(c, local, local_text + "/* edit */\n/* again */\n", 3)
    expect_published(c.settle(), local, [], "after header touch")
    expect(len(header_dumps()) == before + 2, "touched header did not invalidate the cache")
    shutdown(c)
    c.finish(0)


def burst_text(index):
    body = "fn main() i32 { return 0 }\n"
    if index == 50:
        body += "fn only_in_fifty() i32 { return 50 }\n"
    return body + "// change %d\n" % index


def burst():
    b = Server("burst", trace=True)
    initialize(b)
    folder = os.path.join(work, "burst")
    main = os.path.join(folder, "main.fas")
    write_file(main, burst_text(0))
    did_open(b, main, burst_text(0), 1)
    b.settle()
    b.logs = []
    changes = b"".join(change_frame(main, burst_text(index), index + 1) for index in range(1, 51))
    ident, hover = b.frame_request("textDocument/hover", hover_params(main, 1, 4))
    b.write(changes + hover)
    reply = b.wait_reply(ident)
    expect("only_in_fifty" in json.dumps(reply.get("result")), "burst hover: %r" % reply)
    count = len(analyses_of(b, main))
    expect(count == 1, "50 didChange messages analysed %d times" % count)
    expect_published(b.settle(), main, [], "burst")

    quiet = Server("burst-quiet")
    initialize(quiet)
    did_open(quiet, main, burst_text(0), 1)
    quiet.settle()
    expect(not [line for line in quiet.logs if line.startswith("analyse ")], "analysis traced without FAS_LSP_TRACE")
    shutdown(quiet)
    quiet.finish(0)
    shutdown(b)
    b.finish(0)


def split_frame():
    s = Server("split-frame")
    initialize(s)
    folder = os.path.join(work, "split-frame")
    main = os.path.join(folder, "main.fas")
    base = "fn main() i32 { return 0 }\n"
    write_file(main, base)
    did_open(s, main, base, 1)
    s.settle()
    first = change_frame(main, base + "// first\n", 2)
    second = change_frame(main, base + "fn split_b() i32 { return 2 }\n", 3)
    cut = len(second) // 2
    s.write(first + second[:cut])
    time.sleep(0.05)
    s.write(second[cut:])
    ident, hover = s.frame_request("textDocument/hover", hover_params(main, 1, 4))
    s.write(hover)
    reply = s.wait_reply(ident)
    expect("split_b" in json.dumps(reply.get("result")), "split frame hover: %r" % reply)
    shutdown(s)
    s.finish(0)


def ordered_replies():
    o = Server("ordered")
    initialize(o)
    folder = os.path.join(work, "ordered")
    main = os.path.join(folder, "main.fas")
    alpha = "fn main() i32 { return 0 }\nfn alpha() i32 { return 1 }\n"
    beta = "fn main() i32 { return 0 }\nfn beta() i32 { return 1 }\n"
    write_file(main, alpha)
    did_open(o, main, alpha, 1)
    o.settle()
    before_id, before = o.frame_request("textDocument/hover", hover_params(main, 1, 4))
    after_id, after = o.frame_request("textDocument/hover", hover_params(main, 1, 4))
    o.write(before + change_frame(main, beta, 2) + after)
    first = o.next_reply()
    second = o.next_reply()
    expect(
        first.get("id") == before_id and "alpha" in json.dumps(first.get("result")),
        "hover before change: %r" % first,
    )
    shown = json.dumps(second.get("result"))
    expect(
        second.get("id") == after_id and "beta" in shown and "alpha" not in shown,
        "hover after change: %r" % second,
    )
    shutdown(o)
    o.finish(0)


def close_cases():
    c = Server("close-cases")
    initialize(c)
    folder = os.path.join(work, "close-cases")
    root = os.path.join(folder, "main.fas")
    lib = os.path.join(folder, "lib.fas")
    root_text = 'use "lib.fas"\nfn main() i32 { return 0 }\n'
    lib_text = "fn helper() i32 { return nope }\n"
    lib_message = "unknown name `nope`\nnote: include chain: %s -> %s" % (root, lib)
    write_file(root, root_text)
    write_file(lib, lib_text)
    did_open(c, root, root_text, 1)
    expect_published(c.settle(), root, [], "open root")
    did_open(c, lib, lib_text, 1)
    publishes = c.settle()
    expect_published(publishes, lib, [diagnostic(lib_text, "nope", lib_message)], "open use file with error")
    expect_published(publishes, root, [], "root with open use file error")
    did_close(c, lib)
    publishes = c.settle()
    expect_published(publishes, lib, [diagnostic(lib_text, "nope", lib_message)], "close use file keeps diagnostics")
    expect_published(publishes, root, [], "root after use file close")
    did_close(c, root)
    publishes = c.settle()
    expect_published(publishes, root, [], "close root clears root")
    expect_published(publishes, lib, [], "close root clears use file")
    shutdown(c)
    c.finish(0)


def batch_exit():
    e = Server("batch-exit")
    initialize(e)
    folder = os.path.join(work, "batch-exit")
    main = os.path.join(folder, "main.fas")
    base = "fn main() i32 { return 0 }\nfn alpha() i32 { return 1 }\n"
    gamma = "fn main() i32 { return 0 }\nfn gamma() i32 { return 1 }\n"
    write_file(main, base)
    did_open(e, main, base, 1)
    e.settle()
    ident, hover = e.frame_request("textDocument/hover", hover_params(main, 1, 4))
    cancel = frame_of({"jsonrpc": "2.0", "method": "$/cancelRequest", "params": {"id": ident}})
    shut_id, shut = e.frame_request("shutdown")
    exit_frame = frame_of({"jsonrpc": "2.0", "method": "exit"})
    e.write(
        change_frame(main, gamma, 2)
        + change_frame(main, gamma + "// two\n", 3)
        + hover
        + cancel
        + shut
        + exit_frame
    )
    reply = e.wait_reply(ident)
    expect("gamma" in json.dumps(reply.get("result")), "batch hover: %r" % reply)
    reply = e.wait_reply(shut_id)
    expect("result" in reply and reply["result"] is None, "batch shutdown: %r" % reply)
    status = e.proc.wait(timeout=TIMEOUT)
    expect(status == 0, "batch exit status %d, expected 0" % status)

    x = Server("batch-exit-error")
    initialize(x)
    write_file(main, base)
    did_open(x, main, base, 1)
    x.settle()
    early_id, early = x.frame_request("textDocument/hover", hover_params(main, 1, 4))
    late_id, late = x.frame_request("textDocument/hover", hover_params(main, 1, 4))
    x.write(change_frame(main, gamma, 2) + early + exit_frame + late)
    reply = x.wait_reply(early_id)
    expect("gamma" in json.dumps(reply.get("result")), "batch hover before exit: %r" % reply)
    status = x.proc.wait(timeout=TIMEOUT)
    expect(status == 1, "batch exit without shutdown status %d, expected 1" % status)


lifecycle()
exit_without_shutdown()
framing()
framing_resync()
diagnostics()
dependant_roots()
c_cache()
burst()
split_frame()
ordered_replies()
close_cases()
batch_exit()
PY

echo "lsp_protocol: lifecycle, framing, framing resync, batching, diagnostics, close, and C import cache: ok"
