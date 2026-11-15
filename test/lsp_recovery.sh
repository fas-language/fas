#!/bin/sh
set -eu
export DEBUGINFOD_URLS=

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
START_DIR=$(pwd)
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

OCAML_FAS=$(resolve_tool_path "$OCAML_FAS")

LSP_TMP=$(mktemp -d)
TMPDIR=$LSP_TMP/tmp
mkdir -p "$LSP_TMP/work" "$TMPDIR"
WORK=$(cd "$LSP_TMP/work" && pwd -P)
export TMPDIR
trap 'rm -rf "$LSP_TMP"' EXIT HUP INT TERM

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
TIMEOUT = 60
CAP_SECONDS = 5.0
servers = []

LIB_BROKEN = [
    'use "C" <does_not_exist.h> as c',
    "",
    "struct Shape { width i32 height i32 }",
    "",
    "fn helper() i32 {",
    "    return 1",
    "}",
]

LIB_CLEAN = [""] + LIB_BROKEN[1:]

MAIN = [
    'use "lib.fas"',
    "",
    "const LIMIT i32 = 4",
    "",
    "fn identity[T](value T) T {",
    "    return value",
    "}",
    "",
    "fn main() i32 {",
    "    seven u8 = 7",
    "    tiny u8 = identity(seven)",
    "    local i32 = LIMIT + helper()",
    "    return local",
    "}",
]

HEADER_ERROR = {
    "range": {"start": {"line": 0, "character": 8}, "end": {"line": 0, "character": 9}},
    "severity": 1,
    "source": "fas",
    "message": "C header `does_not_exist.h` not found",
}

TYPED_GOOD = [
    "const LIMIT i32 = 4",
    "",
    "fn f(extra i32) i32 {",
    "    value i32 = LIMIT",
    "    return value",
    "}",
    "",
    "fn g() i32 {",
    "    return LIMIT + f(LIMIT)",
    "}",
]

TYPED_BROKEN = [
    "const LIMIT i32 = 4",
    "",
    "fn f(extra i32) i32 {",
    "    value i32 = LIMIT + missing_name",
    "    return value",
    "}",
    "",
    "fn g() i32 {",
    "    return LIMIT + f(LIMIT)",
    "}",
]

CHAIN_GOOD = [
    "const LIMIT i32 = 4",
    "",
    "struct S { value i32 }",
    "",
    "fn uses_s() i32 {",
    "    held S = {.value = 3}",
    "    return held.value",
    "}",
    "",
    "fn other() i32 {",
    "    return 2",
    "}",
    "",
    "fn main() i32 {",
    "    return other() + LIMIT + uses_s()",
    "}",
]

CHAIN_BROKEN = list(CHAIN_GOOD)
CHAIN_BROKEN[2] = ""

STUB_GOOD = [
    "const LIMIT i32 = 4",
    "",
    "fn helper(value i32) i32 {",
    "    held i32 = value + LIMIT",
    "    return held",
    "}",
    "",
    "fn main() i32 {",
    "    return helper(2)",
    "}",
]

STUB_BROKEN = list(STUB_GOOD)
STUB_BROKEN[3] = "    held i32 = value + missing_name"


def fail(message):
    for server in servers:
        server.proc.kill()
    sys.stderr.write("lsp_recovery: %s\n" % message)
    sys.exit(1)


def expect(condition, message):
    if not condition:
        fail(message)


def join_lines(lines):
    return "\n".join(lines) + "\n"


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


def position_of(text, anchor, offset):
    count = text.count(anchor)
    expect(count == 1, "anchor %r occurs %d times" % (anchor, count))
    prefix = text[: text.index(anchor) + offset]
    return prefix.count("\n"), utf16_units(prefix.rsplit("\n", 1)[-1])


def rng(start_line, start_char, end_line, end_char):
    return {
        "start": {"line": start_line, "character": start_char},
        "end": {"line": end_line, "character": end_char},
    }


def at(path, source, anchor, offset=0):
    line, character = position_of(source, anchor, offset)
    return {
        "textDocument": {"uri": uri_of(path)},
        "position": {"line": line, "character": character},
    }


def published(publishes, path):
    found = None
    for uri, diagnostics in publishes:
        if uri == uri_of(path):
            found = diagnostics
    expect(found is not None, "no publishDiagnostics for %s" % path)
    return found


def all_diagnostics(publishes):
    return [item for _, diagnostics in publishes for item in diagnostics]


def symbol_names(symbols):
    names = []
    for symbol in symbols or []:
        names.append(symbol["name"])
        names.extend(symbol_names(symbol.get("children")))
    return names


def error_code(reply):
    return reply.get("error", {}).get("code")


class Server:
    def __init__(self, name):
        self.name = name
        self.stderr = open(os.path.join(work, name + ".err"), "wb")
        self.proc = subprocess.Popen(
            [exe, "lsp"],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=self.stderr,
        )
        self.buffer = b""
        self.next_id = 100
        self.replies = []
        self.publishes = []
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
            if message["method"] == "textDocument/publishDiagnostics":
                params = message["params"]
                self.publishes.append((params["uri"], params["diagnostics"]))
        else:
            self.replies.append(message)

    def wait_reply(self, ident):
        while True:
            for index, reply in enumerate(self.replies):
                if reply.get("id") == ident:
                    return self.replies.pop(index)
            self.pump()

    def request(self, method, params=None):
        self.next_id += 1
        ident = self.next_id
        message = {"jsonrpc": "2.0", "id": ident, "method": method}
        if params is not None:
            message["params"] = params
        self.write(frame_of(message))
        return self.wait_reply(ident)

    def result(self, method, params=None):
        reply = self.request(method, params)
        expect("result" in reply, "%s: %s failed: %r" % (self.name, method, reply))
        return reply["result"]

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


def start(name):
    server = Server(name)
    reply = server.request(
        "initialize", {"processId": None, "rootUri": None, "capabilities": {}}
    )
    expect("result" in reply, "%s: initialize failed: %r" % (name, reply))
    expect(reply["result"]["capabilities"]["textDocumentSync"] == 1, "%s: sync kind" % name)
    server.notify("initialized", {})
    return server


def close(server):
    reply = server.request("shutdown")
    expect("result" in reply and reply["result"] is None, "%s: shutdown %r" % (server.name, reply))
    server.finish(0)


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


def expect_answered(value, label):
    expect(value is not None, "%s: no answer" % label)


def expect_same(got, expected, label):
    expect(got == expected, "%s: got %r, expected %r" % (label, got, expected))


def hover(server, path, text, anchor, offset=0):
    return server.result("textDocument/hover", at(path, text, anchor, offset))


def definition(server, path, text, anchor, offset=0):
    return server.result("textDocument/definition", at(path, text, anchor, offset))


def answers(server, folder, main_text, lib_text):
    main = os.path.join(folder, "main.fas")
    lib = os.path.join(folder, "lib.fas")
    found = {
        "const": hover(server, main, main_text, "const LIMIT", 6),
        "field": hover(server, lib, lib_text, "width i32", 0),
        "call": hover(server, main, main_text, "helper()", 0),
        "definition": definition(server, main, main_text, "helper()", 0),
        "inlay": server.result(
            "textDocument/inlayHint",
            {"textDocument": {"uri": uri_of(main)}, "range": rng(0, 0, 1000, 0)},
        ),
        "outline": server.result(
            "textDocument/documentSymbol", {"textDocument": {"uri": uri_of(lib)}}
        ),
    }
    for key, value in found.items():
        expect_answered(value, "missing header %s" % key)
    return found


def run_missing_header():
    broken_folder = os.path.join(work, "missing")
    clean_folder = os.path.join(work, "clean")
    main_text = join_lines(MAIN)
    lib_broken = join_lines(LIB_BROKEN)
    lib_clean = join_lines(LIB_CLEAN)
    broken_main = os.path.join(broken_folder, "main.fas")
    broken_lib = os.path.join(broken_folder, "lib.fas")
    clean_main = os.path.join(clean_folder, "main.fas")
    write_file(broken_main, main_text)
    write_file(broken_lib, lib_broken)
    write_file(clean_main, main_text)
    write_file(os.path.join(clean_folder, "lib.fas"), lib_clean)

    broken = start("missing")
    did_open(broken, broken_main, main_text, 1)
    publishes = broken.settle()
    expect_same(all_diagnostics(publishes), [HEADER_ERROR], "missing header diagnostics")
    expect_same(published(publishes, broken_lib), [HEADER_ERROR], "missing header in lib.fas")
    expect_same(published(publishes, broken_main), [], "missing header main.fas")

    clean = start("clean")
    did_open(clean, clean_main, main_text, 1)
    expect_same(all_diagnostics(clean.settle()), [], "clean diagnostics")

    got = answers(broken, broken_folder, main_text, lib_broken)
    expected = answers(clean, clean_folder, main_text, lib_clean)
    expect(
        json.dumps(got, sort_keys=True).replace(broken_folder, "FOLDER")
        == json.dumps(expected, sort_keys=True).replace(clean_folder, "FOLDER"),
        "missing header answers differ from the clean project",
    )
    expect(len(got["inlay"]) > 0, "missing header inlay hints are empty")
    names = symbol_names(got["outline"])
    expect("Shape" in names and "helper" in names, "missing header outline: %r" % names)
    expect(got["field"] is not None and got["const"] is not None, "missing header hover")

    line, character = position_of(lib_broken, "fn helper", 3)
    target = definition(broken, broken_main, main_text, "helper()", 0)
    expect_same(
        target,
        {
            "uri": uri_of(broken_lib),
            "range": rng(line, character, line, character + 6),
        },
        "definition of helper across files",
    )
    close(broken)
    close(clean)


def run_type_error():
    folder = os.path.join(work, "typed")
    main = os.path.join(folder, "main.fas")
    good = join_lines(TYPED_GOOD)
    broken = join_lines(TYPED_BROKEN)
    write_file(main, good)

    server = start("typed")
    did_open(server, main, good, 1)
    expect_same(published(server.settle(), main), [], "typed good diagnostics")

    def probe(text):
        return {
            "return value": hover(server, main, text, "return value", 7),
            "fn f": hover(server, main, text, "fn f", 3),
            "parameter": hover(server, main, text, "extra i32", 0),
            "call": hover(server, main, text, "+ f(LIMIT)", 2),
            "call definition": definition(server, main, text, "+ f(LIMIT)", 2),
            "const": hover(server, main, text, "LIMIT + f(LIMIT)", 0),
        }

    before = probe(good)
    for key, value in before.items():
        expect_answered(value, "typed good %s" % key)
    did_change(server, main, broken, 2)
    diagnostics = published(server.settle(), main)
    expect(
        len(diagnostics) == 1 and diagnostics[0]["range"]["start"]["line"] == 3,
        "typed diagnostics: %r" % diagnostics,
    )
    after = probe(broken)
    for key in ("return value", "fn f", "parameter"):
        expect_same(after[key], before[key], "typed %s from the last good text" % key)
    for key in ("call", "call definition", "const"):
        expect_answered(after[key], "typed %s" % key)
    expect_same(after["call definition"], before["call definition"], "typed call definition")
    did_change(server, main, good, 3)
    expect_same(published(server.settle(), main), [], "typed fixed diagnostics")
    fixed = probe(good)
    for key in ("fn f", "parameter"):
        expect_same(after[key], fixed[key], "typed %s after the fix" % key)
    expect_same(fixed["call definition"], before["call definition"], "typed call definition after the fix")
    close(server)


def run_never_checked():
    folder = os.path.join(work, "fresh")
    main = os.path.join(folder, "main.fas")
    good = join_lines(TYPED_GOOD)
    text = join_lines(TYPED_BROKEN)
    write_file(main, text)

    server = start("fresh")
    did_open(server, main, text, 1)
    diagnostics = published(server.settle(), main)
    expect(
        len(diagnostics) == 1 and diagnostics[0]["range"]["start"]["line"] == 3,
        "fresh diagnostics: %r" % diagnostics,
    )
    expect_answered(hover(server, main, text, "const LIMIT", 6), "fresh const")
    expect_answered(hover(server, main, text, "+ f(LIMIT)", 2), "fresh call to f")
    expect_answered(definition(server, main, text, "+ f(LIMIT)", 2), "fresh call definition")
    expect_answered(hover(server, main, text, "fn g", 3), "fresh g")
    expect_same(hover(server, main, text, "return value", 7), None, "fresh inside f")
    name = hover(server, main, text, "fn f", 3)
    parameter = hover(server, main, text, "extra i32", 0)
    call = definition(server, main, text, "+ f(LIMIT)", 2)
    expect_answered(name, "fresh name of f")
    expect_answered(parameter, "fresh parameter of f")
    did_change(server, main, good, 2)
    expect_same(published(server.settle(), main), [], "fresh fixed diagnostics")
    expect_same(hover(server, main, good, "fn f", 3), name, "fresh name of f after the fix")
    expect_same(hover(server, main, good, "extra i32", 0), parameter, "fresh parameter after the fix")
    expect_same(definition(server, main, good, "+ f(LIMIT)", 2), call, "fresh call definition after the fix")
    close(server)


def run_chain():
    folder = os.path.join(work, "chain")
    main = os.path.join(folder, "main.fas")
    good = join_lines(CHAIN_GOOD)
    broken = join_lines(CHAIN_BROKEN)
    write_file(main, good)

    server = start("chain")
    did_open(server, main, good, 1)
    expect_same(published(server.settle(), main), [], "chain good diagnostics")

    def probe(text):
        return {
            "other": hover(server, main, text, "other() +", 0),
            "other definition": definition(server, main, text, "other() +", 0),
            "const": hover(server, main, text, "LIMIT + uses_s", 0),
            "uses_s name": hover(server, main, text, "fn uses_s", 3),
            "held": hover(server, main, text, "held S", 0),
        }

    before = probe(good)
    for key, value in before.items():
        expect_answered(value, "chain good %s" % key)
    did_change(server, main, broken, 2)
    diagnostics = published(server.settle(), main)
    expect(
        len(diagnostics) == 1 and diagnostics[0]["range"]["start"]["line"] == 5,
        "chain diagnostics: %r" % diagnostics,
    )
    after = probe(broken)
    expect_same(after, before, "chain answers after removing struct S")
    close(server)


def many_broken(count):
    lines = ["const LIMIT i32 = 4", ""]
    for index in range(count):
        lines += ["fn task%d() i32 {" % index, "    return missing_%d" % index, "}", ""]
    lines += ["fn main() i32 {", "    return task0() + LIMIT", "}"]
    return lines


def run_cap(count, recovers):
    folder = os.path.join(work, "cap%d" % count)
    main = os.path.join(folder, "main.fas")
    text = join_lines(many_broken(count))
    write_file(main, text)

    server = start("cap%d" % count)
    started = time.monotonic()
    did_open(server, main, text, 1)
    publishes = server.settle()
    value = hover(server, main, text, "const LIMIT", 6)
    elapsed = time.monotonic() - started
    diagnostics = published(publishes, main)
    expect(
        len(diagnostics) == 1 and diagnostics[0]["range"]["start"]["line"] == 3,
        "cap %d diagnostics: %r" % (count, diagnostics),
    )
    expect(elapsed < CAP_SECONDS, "cap %d took %.2f seconds" % (count, elapsed))
    if recovers:
        expect_answered(value, "cap %d const" % count)
    close(server)


def run_stub_hover():
    folder = os.path.join(work, "stub")
    main = os.path.join(folder, "main.fas")
    good = join_lines(STUB_GOOD)
    broken = join_lines(STUB_BROKEN)
    write_file(main, good)

    server = start("stub")
    did_open(server, main, good, 1)
    expect_same(published(server.settle(), main), [], "stub good diagnostics")

    def probe(text):
        return {
            "call": hover(server, main, text, "helper(2)", 0),
            "name": hover(server, main, text, "fn helper", 3),
            "definition": definition(server, main, text, "helper(2)", 0),
        }

    before = probe(good)
    for key, value in before.items():
        expect_answered(value, "stub good %s" % key)
    did_change(server, main, broken, 2)
    diagnostics = published(server.settle(), main)
    expect(
        len(diagnostics) == 1 and diagnostics[0]["range"]["start"]["line"] == 3,
        "stub diagnostics: %r" % diagnostics,
    )
    after = probe(broken)
    expect_same(after, before, "stubbed helper hover against normal hover")
    close(server)


BRACE_GOOD = [
    "const LIMIT i32 = 4",
    "",
    "fn first() i32 {",
    "    return LIMIT",
    "}",
    "",
    "fn middle() i32 {",
    "    return first() + 1",
    "}",
    "",
    "fn last() i32 {",
    "    return middle() + LIMIT",
    "}",
]

BRACE_BROKEN = list(BRACE_GOOD)
BRACE_BROKEN[8] = ""


def fresh_diagnostics(name, path, text):
    server = start(name)
    did_open(server, path, text, 1)
    diagnostics = published(server.settle(), path)
    close(server)
    return diagnostics


def outline_names(server, path):
    return sorted(
        symbol_names(
            server.result("textDocument/documentSymbol", {"textDocument": {"uri": uri_of(path)}})
        )
    )


def run_brace_gap():
    folder = os.path.join(work, "brace")
    main = os.path.join(folder, "main.fas")
    good = join_lines(BRACE_GOOD)
    broken = join_lines(BRACE_BROKEN)
    write_file(main, good)

    server = start("brace")
    did_open(server, main, good, 1)
    expect_same(published(server.settle(), main), [], "brace good diagnostics")

    def probe(text):
        return {
            "first": hover(server, main, text, "fn first", 3),
            "middle": hover(server, main, text, "fn middle", 3),
            "last": hover(server, main, text, "fn last", 3),
            "middle call": hover(server, main, text, "first() + 1", 0),
            "outline": outline_names(server, main),
        }

    before = probe(good)
    for key, value in before.items():
        expect_answered(value, "brace good %s" % key)
    expect(
        {"first", "middle", "last"} <= set(before["outline"]),
        "brace good outline: %r" % before["outline"],
    )
    did_change(server, main, broken, 2)
    diagnostics = published(server.settle(), main)
    expect_same(
        diagnostics,
        fresh_diagnostics("brace-reference", os.path.join(folder, "reference.fas"), broken),
        "brace diagnostics against the first parse error",
    )
    expect(
        len(diagnostics) == 1 and diagnostics[0]["range"]["start"]["line"] == position_of(broken, "fn last", 0)[0],
        "brace diagnostics: %r" % diagnostics,
    )
    expect_same(probe(broken), before, "brace answers in the broken text")
    did_change(server, main, good, 3)
    expect_same(published(server.settle(), main), [], "brace fixed diagnostics")
    expect_same(probe(good), before, "brace answers after the fix")
    close(server)


USE_GOOD = [
    'use "lib.fas"',
    "",
    "const LIMIT i32 = 4",
    "",
    "fn twice(value i32) i32 {",
    "    return value + value",
    "}",
    "",
    "fn main() i32 {",
    "    return twice(LIMIT)",
    "}",
]

USE_BROKEN = list(USE_GOOD)
USE_BROKEN[0] = 'use "lib.fas'

LIB_PLAIN = "fn helper() i32 {\n    return 1\n}\n"


def run_use_line():
    folder = os.path.join(work, "use_line")
    main = os.path.join(folder, "main.fas")
    good = join_lines(USE_GOOD)
    broken = join_lines(USE_BROKEN)
    write_file(main, good)
    write_file(os.path.join(folder, "lib.fas"), LIB_PLAIN)

    server = start("use_line")
    did_open(server, main, good, 1)
    expect_same(published(server.settle(), main), [], "use good diagnostics")

    def probe(text):
        return {
            "const": hover(server, main, text, "const LIMIT", 6),
            "twice": hover(server, main, text, "fn twice", 3),
            "main": hover(server, main, text, "fn main", 3),
            "call": hover(server, main, text, "twice(LIMIT)", 0),
            "definition": definition(server, main, text, "twice(LIMIT)", 0),
            "outline": outline_names(server, main),
        }

    before = probe(good)
    for key, value in before.items():
        expect_answered(value, "use good %s" % key)
    did_change(server, main, broken, 2)
    diagnostics = published(server.settle(), main)
    expect_same(
        diagnostics,
        fresh_diagnostics("use-reference", os.path.join(folder, "reference.fas"), broken),
        "use diagnostics against the first parse error",
    )
    expect(
        len(diagnostics) == 1 and diagnostics[0]["range"]["start"]["line"] == 0,
        "use diagnostics: %r" % diagnostics,
    )
    expect_same(probe(broken), before, "use answers in the broken text")
    did_change(server, main, good, 3)
    expect_same(published(server.settle(), main), [], "use fixed diagnostics")
    expect_same(probe(good), before, "use answers after the fix")
    close(server)


RAW_GOOD = [
    'use "C" as c <<END',
    "struct point { int x; int y; };",
    "int c_twice(int value) { return value * 2; }",
    "END",
    "",
    "const LIMIT i32 = 4",
    "",
    "fn first() i32 {",
    "    return LIMIT",
    "}",
    "",
    "fn middle() i32 {",
    "    return first() + 1",
    "}",
    "",
    "fn last() i32 {",
    "    return middle() + LIMIT",
    "}",
]

RAW_BROKEN = RAW_GOOD[:4] + ["}"] + RAW_GOOD[4:]


def shift_lines(value, first, delta):
    if value is None:
        return None
    shifted = json.loads(json.dumps(value))
    for position in ("start", "end"):
        point = shifted["range"][position]
        if point["line"] >= first:
            point["line"] += delta
    return shifted


def run_raw_block():
    folder = os.path.join(work, "raw")
    main = os.path.join(folder, "main.fas")
    good = join_lines(RAW_GOOD)
    broken = join_lines(RAW_BROKEN)
    unterminated = join_lines(RAW_GOOD[:3] + RAW_GOOD[4:])
    write_file(main, good)

    server = start("raw")
    did_open(server, main, good, 1)
    expect_same(published(server.settle(), main), [], "raw good diagnostics")

    def probe(text):
        return {
            "const": hover(server, main, text, "const LIMIT", 6),
            "first": hover(server, main, text, "fn first", 3),
            "middle": hover(server, main, text, "fn middle", 3),
            "last": hover(server, main, text, "fn last", 3),
        }

    before = probe(good)
    for key, value in before.items():
        expect_answered(value, "raw good %s" % key)
    did_change(server, main, broken, 2)
    diagnostics = published(server.settle(), main)
    expect_same(
        diagnostics,
        fresh_diagnostics("raw-reference", os.path.join(folder, "reference.fas"), broken),
        "raw diagnostics against the first parse error",
    )
    expect(
        len(diagnostics) == 1 and diagnostics[0]["range"]["start"]["line"] == 4,
        "raw diagnostics: %r" % diagnostics,
    )
    expect_same(
        probe(broken),
        {key: shift_lines(value, 4, 1) for key, value in before.items()},
        "raw answers after the stray brace",
    )
    expect(
        {"first", "middle", "last"} <= set(outline_names(server, main)),
        "raw broken outline: %r" % outline_names(server, main),
    )
    did_change(server, main, unterminated, 3)
    diagnostics = published(server.settle(), main)
    expect(len(diagnostics) == 1, "raw unterminated diagnostics: %r" % diagnostics)
    reply = server.request("textDocument/hover", at(main, unterminated, "fn first", 3))
    expect("result" in reply, "raw unterminated hover got no result: %r" % reply)
    did_change(server, main, good, 4)
    expect_same(published(server.settle(), main), [], "raw fixed diagnostics")
    expect_same(probe(good), before, "raw answers after the fix")
    close(server)


NEST_MAIN = [
    'use "lib.fas"',
    "",
    "const LIMIT i32 = 4",
    "",
    "fn main() i32 {",
    "    return helper() + LIMIT",
    "}",
]

NEST_LIB_GOOD = [
    "struct Shape { width i32 height i32 }",
    "",
    "fn helper() i32 {",
    "    return 1",
    "}",
    "",
    "fn other() i32 {",
    "    return 2",
    "}",
]

NEST_LIB_BROKEN = list(NEST_LIB_GOOD)
NEST_LIB_BROKEN[4] = ""


def run_use_file_gap():
    folder = os.path.join(work, "nested")
    main = os.path.join(folder, "main.fas")
    lib = os.path.join(folder, "lib.fas")
    main_text = join_lines(NEST_MAIN)
    lib_good = join_lines(NEST_LIB_GOOD)
    lib_broken = join_lines(NEST_LIB_BROKEN)
    write_file(main, main_text)
    write_file(lib, lib_good)

    server = start("nested")
    did_open(server, main, main_text, 1)
    did_open(server, lib, lib_good, 1)
    expect_same(all_diagnostics(server.settle()), [], "nested good diagnostics")

    def probe_root(text):
        return {
            "const": hover(server, main, text, "const LIMIT", 6),
            "main": hover(server, main, text, "fn main", 3),
            "outline": outline_names(server, main),
        }

    def probe_lib(text):
        return {
            "shape": hover(server, lib, text, "width i32", 0),
            "helper": hover(server, lib, text, "fn helper", 3),
            "other": hover(server, lib, text, "fn other", 3),
        }

    before_root = probe_root(main_text)
    before_lib = probe_lib(lib_good)
    for key, value in list(before_root.items()) + list(before_lib.items()):
        expect_answered(value, "nested good %s" % key)
    did_change(server, lib, lib_broken, 2)
    publishes = server.settle()
    diagnostics = published(publishes, lib)
    expect(
        len(diagnostics) == 1 and diagnostics[0]["range"]["start"]["line"] == position_of(lib_broken, "fn other", 0)[0],
        "nested lib diagnostics: %r" % diagnostics,
    )
    expect_same(probe_root(main_text), before_root, "nested root answers with broken lib")
    expect_same(probe_lib(lib_broken), before_lib, "nested lib answers with broken lib")
    did_change(server, lib, lib_good, 3)
    expect_same(all_diagnostics(server.settle()), [], "nested fixed diagnostics")
    expect_same(probe_root(main_text), before_root, "nested root answers after the fix")
    expect_same(probe_lib(lib_good), before_lib, "nested lib answers after the fix")
    close(server)


def run_fresh_parse_gap():
    folder = os.path.join(work, "fresh_gap")
    main = os.path.join(folder, "main.fas")
    good = join_lines(BRACE_GOOD)
    broken = join_lines(BRACE_BROKEN)
    write_file(main, broken)

    server = start("fresh_gap")
    did_open(server, main, broken, 1)
    diagnostics = published(server.settle(), main)
    expect(
        len(diagnostics) == 1 and diagnostics[0]["range"]["start"]["line"] == position_of(broken, "fn last", 0)[0],
        "fresh gap diagnostics: %r" % diagnostics,
    )
    expect_answered(hover(server, main, broken, "const LIMIT", 6), "fresh gap const")
    expect_answered(hover(server, main, broken, "fn first", 3), "fresh gap first")
    expect_answered(hover(server, main, broken, "fn last", 3), "fresh gap last")
    middle_header = hover(server, main, broken, "fn middle", 3)
    expect_answered(middle_header, "fresh gap middle header without a good parse")
    expect("fn middle() i32" in middle_header["contents"]["value"], "fresh gap middle header: %r" % middle_header)
    expect(
        {"first", "last"} <= set(outline_names(server, main)),
        "fresh gap outline: %r" % outline_names(server, main),
    )
    did_change(server, main, good, 2)
    expect_same(published(server.settle(), main), [], "fresh gap fixed diagnostics")
    expect_answered(hover(server, main, good, "fn middle", 3), "fresh gap middle after the fix")
    expect_answered(hover(server, main, good, "first() + 1", 0), "fresh gap call after the fix")
    expect(
        {"first", "middle", "last"} <= set(outline_names(server, main)),
        "fresh gap outline after the fix: %r" % outline_names(server, main),
    )
    close(server)


RECOVER_GOOD = [
    "const LIMIT i32 = 4",
    "",
    "fn one() i32 {",
    "    return LIMIT",
    "}",
    "",
    "fn two() i32 {",
    "    return one() + 1",
    "}",
    "",
    "fn three() i32 {",
    "    if two() > LIMIT {",
    "        return two()",
    "    }",
    "    return 0",
    "}",
    "",
    "fn four() i32 {",
    "    return two() + LIMIT",
    "}",
    "",
    "fn five() i32 {",
    "    return one() + LIMIT",
    "}",
]

RECOVER_ITEM_BROKEN = list(RECOVER_GOOD)
RECOVER_ITEM_BROKEN[13] = ""

ITEM_PROBES = [
    ("one", "fn one", 3),
    ("two", "fn two", 3),
    ("four", "fn four", 3),
    ("four call", "two() + LIMIT", 0),
    ("four limit", "two() + LIMIT", 9),
    ("five", "fn five", 3),
    ("five call", "one() + LIMIT", 0),
]


def item_answers(server, path, text):
    return {name: hover(server, path, text, anchor, offset) for name, anchor, offset in ITEM_PROBES}


def run_recover_previous_item():
    folder = os.path.join(work, "recover_previous_item")
    main = os.path.join(folder, "main.fas")
    good = join_lines(RECOVER_GOOD)
    broken = join_lines(RECOVER_ITEM_BROKEN)
    write_file(main, good)

    server = start("recover_previous_item")
    did_open(server, main, good, 1)
    expect_same(published(server.settle(), main), [], "previous item good diagnostics")
    before = item_answers(server, main, good)
    for key, value in before.items():
        expect_answered(value, "previous item good %s" % key)
    three_before = hover(server, main, good, "fn three", 3)
    expect_answered(three_before, "previous item good three")
    did_change(server, main, broken, 2)
    diagnostics = published(server.settle(), main)
    expect(
        len(diagnostics) == 1 and diagnostics[0]["range"]["start"]["line"] == position_of(broken, "fn four", 0)[0],
        "previous item diagnostics: %r" % diagnostics,
    )
    expect_same(item_answers(server, main, broken), before, "previous item answers after an edit")
    expect_same(hover(server, main, broken, "fn three", 3), three_before, "previous item gap after an edit")
    did_change(server, main, good, 3)
    expect_same(published(server.settle(), main), [], "previous item fixed diagnostics")
    expect_same(item_answers(server, main, good), before, "previous item answers after the fix")
    close(server)

    fresh = start("recover_previous_item_fresh")
    did_open(fresh, main, broken, 1)
    diagnostics = published(fresh.settle(), main)
    expect(
        len(diagnostics) == 1 and diagnostics[0]["range"]["start"]["line"] == position_of(broken, "fn four", 0)[0],
        "previous item fresh diagnostics: %r" % diagnostics,
    )
    expect_same(item_answers(fresh, main, broken), before, "previous item answers on a fresh open")
    expect_same(hover(fresh, main, broken, "fn three", 3), three_before, "previous item header kept without a good parse")
    close(fresh)


RECOVER_BODY_GOOD = [
    "const LIMIT i32 = 4",
    "",
    "fn one() i32 {",
    "    return LIMIT",
    "}",
    "",
    "fn two() i32 {",
    "    x i32 = 3",
    "    return x + LIMIT",
    "}",
    "",
    "fn three() i32 {",
    "    return LIMIT + 3",
    "}",
]

RECOVER_BODY_BROKEN = list(RECOVER_BODY_GOOD)
RECOVER_BODY_BROKEN[7] = "    x i32 = = 3"

BODY_PROBES = [
    ("const", "const LIMIT", 6),
    ("one", "fn one", 3),
    ("one limit", "fn one() i32 {\n    return LIMIT", 26),
    ("three", "fn three", 3),
    ("three limit", "LIMIT + 3", 0),
]


def body_answers(server, path, text):
    return {name: hover(server, path, text, anchor, offset) for name, anchor, offset in BODY_PROBES}


def run_recover_body_error():
    folder = os.path.join(work, "recover_body_error")
    main = os.path.join(folder, "main.fas")
    good = join_lines(RECOVER_BODY_GOOD)
    broken = join_lines(RECOVER_BODY_BROKEN)
    write_file(main, good)

    server = start("recover_body_error")
    did_open(server, main, good, 1)
    expect_same(published(server.settle(), main), [], "body error good diagnostics")
    before = body_answers(server, main, good)
    for key, value in before.items():
        expect_answered(value, "body error good %s" % key)
    two_before = hover(server, main, good, "fn two", 3)
    expect_answered(two_before, "body error good two")
    did_change(server, main, broken, 2)
    diagnostics = published(server.settle(), main)
    expect(
        len(diagnostics) == 1 and diagnostics[0]["range"]["start"]["line"] == position_of(broken, "x i32 = = 3", 0)[0],
        "body error diagnostics: %r" % diagnostics,
    )
    expect_same(body_answers(server, main, broken), before, "body error answers after an edit")
    expect_same(hover(server, main, broken, "fn two", 3), two_before, "body error gap after an edit")
    close(server)

    fresh = start("recover_body_error_fresh")
    did_open(fresh, main, broken, 1)
    diagnostics = published(fresh.settle(), main)
    expect(
        len(diagnostics) == 1 and diagnostics[0]["range"]["start"]["line"] == position_of(broken, "x i32 = = 3", 0)[0],
        "body error fresh diagnostics: %r" % diagnostics,
    )
    expect_same(body_answers(fresh, main, broken), before, "body error answers on a fresh open")
    expect_same(hover(fresh, main, broken, "fn two", 3), two_before, "body error header kept without a good parse")
    close(fresh)


RECOVER_TWO_GOOD = [
    "const LIMIT i32 = 4",
    "",
    "fn one() i32 {",
    "    return LIMIT",
    "}",
    "",
    "fn two() i32 {",
    "    x i32 = 3",
    "    return x + LIMIT",
    "}",
    "",
    "fn three() i32 {",
    "    y i32 = 4",
    "    return y + LIMIT",
    "}",
    "",
    "fn four() i32 {",
    "    return LIMIT + 1",
    "}",
    "",
    "fn five() i32 {",
    "    return four() + LIMIT",
    "}",
]

RECOVER_TWO_BROKEN = list(RECOVER_TWO_GOOD)
RECOVER_TWO_BROKEN[7] = "    x i32 = = 3"
RECOVER_TWO_BROKEN[12] = "    y i32 = = 4"


def run_recover_positions():
    folder = os.path.join(work, "recover_positions")
    main = os.path.join(folder, "main.fas")
    good = join_lines(RECOVER_TWO_GOOD)
    broken = join_lines(RECOVER_TWO_BROKEN)
    write_file(main, good)
    first_error = position_of(broken, "x i32 = = 3", 0)[0]
    probes = {
        "one": ("hover", "fn one", 3),
        "four": ("hover", "fn four", 3),
        "five": ("hover", "fn five", 3),
        "five call": ("definition", "four() + LIMIT", 0),
        "four limit": ("definition", "LIMIT + 1", 0),
    }

    def probe(server, text):
        found = {}
        for name, (kind, anchor, offset) in probes.items():
            if kind == "hover":
                found[name] = hover(server, main, text, anchor, offset)
            else:
                found[name] = definition(server, main, text, anchor, offset)
        return found

    server = start("recover_positions")
    did_open(server, main, good, 1)
    expect_same(published(server.settle(), main), [], "positions good diagnostics")
    before = probe(server, good)
    for key, value in before.items():
        expect_answered(value, "positions good %s" % key)
    did_change(server, main, broken, 2)
    diagnostics = published(server.settle(), main)
    expect(
        len(diagnostics) == 1
        and diagnostics[0]["range"]["start"]["line"] == first_error
        and "x i32 = = 3" in broken.split("\n")[first_error],
        "positions diagnostics do not match the source line: %r" % diagnostics,
    )
    expect_same(probe(server, broken), before, "positions answers below and above the gaps after an edit")
    close(server)

    fresh = start("recover_positions_fresh")
    did_open(fresh, main, broken, 1)
    diagnostics = published(fresh.settle(), main)
    expect(
        len(diagnostics) == 1 and diagnostics[0]["range"]["start"]["line"] == first_error,
        "positions fresh diagnostics: %r" % diagnostics,
    )
    expect_same(probe(fresh, broken), before, "positions answers on a fresh open")
    close(fresh)


RECOVER_EOF_GOOD = [
    "const LIMIT i32 = 4",
    "",
    "fn one() i32 {",
    "    return LIMIT",
    "}",
    "",
    "fn two() i32 {",
    "    return one() + LIMIT",
    "}",
]

RECOVER_EOF_BROKEN = RECOVER_EOF_GOOD[:-1]


def run_recover_eof_gap():
    folder = os.path.join(work, "recover_eof_gap")
    main = os.path.join(folder, "main.fas")
    good = join_lines(RECOVER_EOF_GOOD)
    broken = join_lines(RECOVER_EOF_BROKEN)
    write_file(main, broken)

    server = start("recover_eof_gap")
    did_open(server, main, broken, 1)
    diagnostics = published(server.settle(), main)
    expect(len(diagnostics) == 1, "eof gap diagnostics: %r" % diagnostics)
    expect_answered(hover(server, main, broken, "const LIMIT", 6), "eof gap const")
    expect_answered(hover(server, main, broken, "fn one", 3), "eof gap one")
    expect_answered(hover(server, main, broken, "return LIMIT", 7), "eof gap limit before the gap")
    two_header = hover(server, main, broken, "fn two", 3)
    expect_answered(two_header, "eof gap two header without a good parse")
    expect("fn two() i32" in two_header["contents"]["value"], "eof gap two header: %r" % two_header)
    expect(
        {"one"} <= set(outline_names(server, main)),
        "eof gap outline: %r" % outline_names(server, main),
    )
    did_change(server, main, good, 2)
    expect_same(published(server.settle(), main), [], "eof gap fixed diagnostics")
    expect_answered(hover(server, main, good, "fn two", 3), "eof gap two after the fix")
    close(server)


KEEP_KEYWORDS = {"fn", "return", "if", "void", "i32"}


def delete_closing_brace(lines):
    expect(lines[13] == "    }", "unexpected line 13: %r" % lines[13])
    broken = list(lines)
    broken[13] = ""
    return broken


KEEP_VOID_GOOD = [
    "const LIMIT i32 = 4",
    "",
    "fn one() i32 {",
    "    return LIMIT",
    "}",
    "",
    "fn two() i32 {",
    "    return one() + 1",
    "}",
    "",
    "fn three(a i32) void {",
    "    if two() > a {",
    "        a = a + 1",
    "    }",
    "}",
    "",
    "fn four() i32 {",
    "    three(LIMIT)",
    "    return two() + LIMIT",
    "}",
]

KEEP_VOID_BROKEN = delete_closing_brace(KEEP_VOID_GOOD)

KEEP_I32_GOOD = [
    "const LIMIT i32 = 4",
    "",
    "fn one() i32 {",
    "    return LIMIT",
    "}",
    "",
    "fn two() i32 {",
    "    return one() + 1",
    "}",
    "",
    "fn three(a i32) i32 {",
    "    if two() > a {",
    "        return a",
    "    }",
    "    return 0",
    "}",
    "",
    "fn four() i32 {",
    "    x i32 = three(LIMIT)",
    "    return x + LIMIT",
    "}",
]

KEEP_I32_BROKEN = delete_closing_brace(KEEP_I32_GOOD)

KEEP_RAW_GOOD = [
    'use "C" as c <<C',
    "#if 0",
    "fn x() {",
    "struct y {",
    "#endif",
    "int c_twice(int value) { return value * 2; }",
    "struct point { int x; int y; };",
    "C",
    "",
    "const LIMIT i32 = 4",
    "",
    "fn first() i32 {",
    "    return c.c_twice(LIMIT)",
    "}",
    "",
    "fn middle() i32 {",
    "    return first() + 1",
    "}",
    "",
    "fn last() i32 {",
    "    return middle() + LIMIT",
    "}",
]

expect(KEEP_RAW_GOOD[16] == "    return first() + 1", "unexpected raw fixture line 16")
KEEP_RAW_BROKEN = list(KEEP_RAW_GOOD)
KEEP_RAW_BROKEN[16] = "    x i32 = = 3"


def point_of(text, index):
    prefix = text[:index]
    return prefix.count("\n"), utf16_units(prefix.rsplit("\n", 1)[-1])


def fn_four_points(text):
    start = text.index("fn four")
    end = text.index("\n}", start) + 2
    points = []
    for match in re.finditer(r"[A-Za-z_][A-Za-z0-9_]*", text[start:end]):
        if match.group(0) not in KEEP_KEYWORDS:
            line, character = point_of(text, start + match.start())
            points.append((match.group(0), line, character))
    return points


def hover_point(server, path, line, character):
    return server.result(
        "textDocument/hover",
        {"textDocument": {"uri": uri_of(path)}, "position": {"line": line, "character": character}},
    )


def keep_answers(server, path, text):
    points = fn_four_points(text)
    expect(len(points) >= 5, "fn four identifiers: %r" % points)
    answers = {
        "three name": hover(server, path, text, "fn three", 3),
        "three param": hover(server, path, text, "(a i32", 1),
    }
    for index, (name, line, character) in enumerate(points):
        answers["four %d %s" % (index, name)] = hover_point(server, path, line, character)
    return answers


def raw_answers(server, path, text):
    return {
        "const": hover(server, path, text, "const LIMIT", 6),
        "first": hover(server, path, text, "fn first", 3),
        "c call": hover(server, path, text, "c_twice(LIMIT)", 0),
        "middle": hover(server, path, text, "fn middle", 3),
        "middle call": hover(server, path, text, "middle() + LIMIT", 0),
        "last": hover(server, path, text, "fn last", 3),
    }


def run_edit_case(label, good_lines, broken_lines, error_anchor, probe):
    folder = os.path.join(work, label)
    main = os.path.join(folder, "main.fas")
    good = join_lines(good_lines)
    broken = join_lines(broken_lines)
    write_file(main, good)

    server = start(label)
    did_open(server, main, good, 1)
    expect_same(published(server.settle(), main), [], "%s good diagnostics" % label)
    before = probe(server, main, good)
    for key, value in before.items():
        expect_answered(value, "%s good %s" % (label, key))
    did_change(server, main, broken, 2)
    diagnostics = published(server.settle(), main)
    expect(
        len(diagnostics) == 1 and diagnostics[0]["range"]["start"]["line"] == position_of(broken, error_anchor, 0)[0],
        "%s diagnostics: %r" % (label, diagnostics),
    )
    expect_same(probe(server, main, broken), before, "%s answers after an edit" % label)
    close(server)

    fresh = start(label + "_fresh")
    did_open(fresh, main, broken, 1)
    diagnostics = published(fresh.settle(), main)
    expect(
        len(diagnostics) == 1 and diagnostics[0]["range"]["start"]["line"] == position_of(broken, error_anchor, 0)[0],
        "%s fresh diagnostics: %r" % (label, diagnostics),
    )
    expect_same(probe(fresh, main, broken), before, "%s answers on a fresh open" % label)
    close(fresh)


def run_keep_header():
    run_edit_case("keep_header_void", KEEP_VOID_GOOD, KEEP_VOID_BROKEN, "fn four", keep_answers)
    run_edit_case("keep_header_i32", KEEP_I32_GOOD, KEEP_I32_BROKEN, "fn four", keep_answers)
    run_edit_case("keep_header_raw", KEEP_RAW_GOOD, KEEP_RAW_BROKEN, "x i32 = = 3", raw_answers)


run_recover_previous_item()
run_recover_body_error()
run_recover_positions()
run_recover_eof_gap()
run_keep_header()
run_brace_gap()
run_use_line()
run_raw_block()
run_use_file_gap()
run_fresh_parse_gap()
run_missing_header()
run_type_error()
run_never_checked()
run_chain()
run_cap(60, True)
run_cap(70, False)
run_stub_hover()
PY
