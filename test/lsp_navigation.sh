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
TIMEOUT = 60
servers = []

MAIN = [
    'use "C" <string.h>',
    'use "lib.fas"',
    "",
    "const LIMIT i32 = 4",
    "const TABLE arr[2,i32] = {1, 2}",
    "var GLOBAL i32 = 5",
    "",
    "struct Point { x i32 y i32 }",
    "struct Box[T] { value T }",
    "",
    "fn identity[T](value T) T {",
    "    return value",
    "}",
    "",
    "fn first_byte(pointer addr) u8 {",
    "    view first = pointer[u8]",
    "    return first",
    "}",
    "",
    "fn main() i32 {",
    "    point Point = {.x = 1, .y = 2}",
    "    local i32 = point.x + GLOBAL + LIMIT + TABLE[0]",
    '    text usize = strlen(c"abc")',
    "    box Box[i32] = {7}",
    "    return identity[i32](local) + helper() + box.value + point.y",
    "}",
]

OUTLINE = [
    "const LIMIT i32 = 4",
    "var GLOBAL i32 = 5",
    "opaque Handle",
    "struct Box[T] { value T }",
    "fn compute(count i32) i32 {",
    "    local i32 = count + LIMIT + GLOBAL",
    "    return local",
    "}",
]

LIB = ["fn helper() i32 { return 1 }"]


def fail(message):
    for server in servers:
        server.proc.kill()
    sys.stderr.write("lsp_navigation: %s\n" % message)
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


def loc(path, line, start, end):
    return {"uri": uri_of(path), "range": rng(line, start, line, end)}


def at(path, source, anchor, offset=0):
    line, character = position_of(source, anchor, offset)
    return {
        "textDocument": {"uri": uri_of(path)},
        "position": {"line": line, "character": character},
    }


def with_context(params, include_declaration):
    result = dict(params)
    result["context"] = {"includeDeclaration": include_declaration}
    return result


def node(name, kind, span, selection, children=()):
    return {
        "name": name,
        "detail": "",
        "kind": kind,
        "range": rng(*span),
        "selectionRange": rng(*selection),
        "children": list(children),
    }


def error_code(reply):
    return reply.get("error", {}).get("code")


def published(publishes, path):
    found = None
    for uri, diagnostics in publishes:
        if uri == uri_of(path):
            found = diagnostics
    expect(found is not None, "no publishDiagnostics for %s" % path)
    return found


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

    def next_reply(self):
        while not self.replies:
            self.pump()
        return self.replies.pop(0)

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

    def expect_invalid(self, method, params, label):
        reply = self.request(method, params)
        expect(error_code(reply) == -32602, "%s: got %r" % (label, reply))
        message = reply["error"].get("message")
        expect(isinstance(message, str) and message != "", "%s: no message in %r" % (label, reply))

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
    return result


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


def did_close(server, path):
    server.notify("textDocument/didClose", {"textDocument": {"uri": uri_of(path)}})


def expect_published(publishes, path, expected, label):
    found = published(publishes, path)
    expect(found == expected, "%s: got %r, expected %r" % (label, found, expected))


def definitions(s, path, source):
    cases = [
        ("identity[i32]", 0, loc(path, 10, 3, 11)),
        ("return value", 7, loc(path, 10, 15, 20)),
        ("local i32 = point", 0, loc(path, 21, 4, 9)),
        ("(local)", 1, loc(path, 21, 4, 9)),
        ("return first", 7, loc(path, 15, 9, 14)),
        ("GLOBAL + LIMIT", 0, loc(path, 5, 4, 10)),
        ("LIMIT + TABLE", 0, loc(path, 3, 6, 11)),
        ("TABLE[0]", 0, loc(path, 4, 6, 11)),
        ("Point = {", 0, loc(path, 7, 7, 12)),
        ("point.x", 6, loc(path, 7, 15, 16)),
        ("{.x = 1", 2, loc(path, 7, 15, 16)),
        ("value T) T", 6, loc(path, 10, 12, 13)),
        ("fn main", 3, loc(path, 19, 3, 7)),
    ]
    for anchor, offset, expected in cases:
        got = s.result("textDocument/definition", at(path, source, anchor, offset))
        expect(got == expected, "definition %r+%d: got %r, expected %r" % (anchor, offset, got, expected))


C_HEADER = "int nav_add(int left, int right);\n"

C_USER = [
    'use "C" "nav_c.h"',
    "fn main() i32 {",
    "    total i32 = nav_add(1, 2)",
    "    return total + nav_add(total, 3)",
    "}",
]


def c_header_navigation(s, folder):
    header = os.path.realpath(os.path.join(folder, "nav_c.h"))
    user = os.path.join(folder, "cnav.fas")
    write_file(header, C_HEADER)
    user_text = join_lines(C_USER)
    write_file(user, user_text)
    did_open(s, user, user_text, 1)
    expect_published(s.settle(), user, [], "open C header user")
    use_line = at(user, user_text, '"nav_c.h"', 1)
    expect(
        s.result("textDocument/definition", use_line) is None,
        "definition on the C use line is not null",
    )
    expect(
        s.result("textDocument/references", with_context(use_line, True)) is None,
        "references on the C use line are not null",
    )
    declared = loc(header, 0, 0, 0)
    uses = [loc(user, 2, 16, 23), loc(user, 3, 19, 26)]
    for anchor in ("nav_add(1", "nav_add(total"):
        call = at(user, user_text, anchor, 0)
        got = s.result("textDocument/definition", call)
        expect(got == declared, "C definition %r: got %r, expected %r" % (anchor, got, declared))
        got = s.result("textDocument/references", with_context(call, True))
        expect(got == [declared] + uses, "C references %r with declaration: got %r" % (anchor, got))
        got = s.result("textDocument/references", with_context(call, False))
        expect(got == uses, "C references %r without declaration: got %r" % (anchor, got))
    did_close(s, user)
    s.settle()


def run_navigation():
    s = Server("navigation")
    result = initialize(s)
    for key in ("definitionProvider", "referencesProvider", "documentSymbolProvider"):
        expect(result["capabilities"].get(key) is True, "capability %s is not true" % key)

    folder = os.path.join(work, "navigation")
    main = os.path.join(folder, "main.fas")
    lib = os.path.join(folder, "lib.fas")
    outline = os.path.join(folder, "outline.fas")
    orphan = os.path.join(folder, "orphan.fas")
    main_text = join_lines(MAIN)
    lib_text = join_lines(LIB)
    outline_text = join_lines(OUTLINE)
    write_file(main, main_text)
    write_file(lib, lib_text)
    write_file(outline, outline_text)
    write_file(orphan, "fn orphan() i32 { return 0 }\n")

    did_open(s, main, main_text, 1)
    expect_published(s.settle(), main, [], "open main")

    definitions(s, main, main_text)

    c_reply = s.result("textDocument/definition", at(main, main_text, "strlen(c"))
    expect(c_reply["uri"].endswith("/string.h"), "C jump uri %r" % c_reply["uri"])
    header = urllib.parse.unquote(c_reply["uri"][len("file://") :])
    with open(header, encoding="utf-8", errors="replace") as handle:
        header_lines = handle.read().split("\n")
    rows = [index for index, line in enumerate(header_lines) if line.startswith("extern size_t strlen")]
    expect(len(rows) == 1, "strlen declaration found %d times in %s" % (len(rows), header))
    expect(c_reply["range"] == rng(rows[0], 0, rows[0], 0), "C jump range %r, header row %d" % (c_reply["range"], rows[0]))

    keyword =s.result("textDocument/definition", at(main, main_text, "fn main", 0))
    expect(keyword is None, "keyword gave %r" % (keyword,))

    references = [
        ("point.x", 6, True, [loc(main, 7, 15, 16), loc(main, 20, 20, 21), loc(main, 21, 22, 23)]),
        ("point.x", 6, False, [loc(main, 20, 20, 21), loc(main, 21, 22, 23)]),
        ("LIMIT + TABLE", 0, True, [loc(main, 3, 6, 11), loc(main, 21, 35, 40)]),
        ("LIMIT + TABLE", 0, False, [loc(main, 21, 35, 40)]),
        ("return first", 7, True, [loc(main, 15, 9, 14), loc(main, 16, 11, 16)]),
        ("return first", 7, False, [loc(main, 16, 11, 16)]),
        ("helper()", 0, True, [loc(lib, 0, 3, 9), loc(main, 24, 34, 40)]),
        ("helper()", 0, False, [loc(main, 24, 34, 40)]),
    ]
    for anchor, offset, include, expected in references:
        params = with_context(at(main, main_text, anchor, offset), include)
        got = s.result("textDocument/references", params)
        expect(got == expected, "references %r+%d %r: got %r, expected %r" % (anchor, offset, include, got, expected))

    cross = s.result("textDocument/definition", at(main, main_text, "helper()"))
    expect(cross == loc(lib, 0, 3, 9), "cross-file definition: got %r" % (cross,))
    unopened = s.result("textDocument/definition", at(lib, lib_text, "fn helper", 3))
    expect(unopened == loc(lib, 0, 3, 9), "unopened lib definition: got %r" % (unopened,))

    expect(
        s.result("textDocument/definition", at(orphan, "fn orphan() i32 { return 0 }\n", "fn orphan", 3)) is None,
        "orphan definition is not null",
    )

    s.expect_invalid("textDocument/definition", {"textDocument": {"uri": uri_of(main)}}, "definition without position")
    s.expect_invalid(
        "textDocument/definition",
        {"position": {"line": 0, "character": 0}},
        "definition without textDocument",
    )
    s.expect_invalid(
        "textDocument/references",
        at(main, main_text, "point.x", 6),
        "references without context",
    )
    s.expect_invalid(
        "textDocument/references",
        with_context(at(main, main_text, "point.x", 6), "yes"),
        "references with string includeDeclaration",
    )
    s.expect_invalid(
        "textDocument/definition",
        {"textDocument": {"uri": uri_of(main)}, "position": {"line": -1, "character": 0}},
        "definition with negative line",
    )
    s.expect_invalid("textDocument/documentSymbol", {}, "documentSymbol without textDocument")
    expect(
        s.result("textDocument/definition", at(main, main_text, "identity[i32]")) is not None,
        "valid request after invalid params",
    )

    did_open(s, outline, outline_text, 1)
    expect_published(s.settle(), outline, [], "open outline")
    def outline_request():
        return s.result("textDocument/documentSymbol", {"textDocument": {"uri": uri_of(outline)}})

    symbols = outline_request()
    expected_symbols = [
        node("LIMIT", 14, (0, 0, 0, 19), (0, 6, 0, 11)),
        node("GLOBAL", 13, (1, 0, 1, 18), (1, 4, 1, 10)),
        node("Handle", 23, (2, 0, 2, 13), (2, 7, 2, 13)),
        node("Box", 23, (3, 0, 3, 25), (3, 7, 3, 10), [node("value", 8, (3, 16, 3, 21), (3, 16, 3, 21))]),
        node(
            "compute",
            12,
            (4, 0, 7, 1),
            (4, 3, 4, 10),
            [
                node("count", 13, (4, 11, 4, 16), (4, 11, 4, 16)),
                node("local", 13, (5, 4, 5, 9), (5, 4, 5, 9)),
            ],
        ),
    ]
    expect(symbols == expected_symbols, "documentSymbol: got %r" % (symbols,))

    did_change(s, outline, join_lines(OUTLINE + ["broken ("]), 2)
    expect(published(s.settle(), outline) != [], "appended broken line has no diagnostics")
    appended = outline_request()
    expect(appended == expected_symbols, "outline after appended broken line: got %r" % (appended,))

    did_change(s, outline, join_lines(OUTLINE[:2] + ["broken ("] + OUTLINE[2:]), 3)
    expect(published(s.settle(), outline) != [], "inserted broken line has no diagnostics")
    shifted_outline = [
        node("LIMIT", 14, (0, 0, 0, 19), (0, 6, 0, 11)),
        node("GLOBAL", 13, (1, 0, 1, 18), (1, 4, 1, 10)),
        node("Handle", 23, (3, 0, 3, 13), (3, 7, 3, 13)),
        node("Box", 23, (4, 0, 4, 25), (4, 7, 4, 10), [node("value", 8, (4, 16, 4, 21), (4, 16, 4, 21))]),
        node(
            "compute",
            12,
            (5, 0, 8, 1),
            (5, 3, 5, 10),
            [
                node("count", 13, (5, 11, 5, 16), (5, 11, 5, 16)),
                node("local", 13, (6, 4, 6, 9), (6, 4, 6, 9)),
            ],
        ),
    ]
    shifted = outline_request()
    expect(shifted == shifted_outline, "outline after inserted broken line: got %r" % (shifted,))

    replaced_body = list(OUTLINE)
    replaced_body[5] = "    local i32 = ("
    did_change(s, outline, join_lines(replaced_body), 4)
    expect(published(s.settle(), outline) != [], "replaced body line has no diagnostics")
    kept_body = [
        expected_symbols[0],
        expected_symbols[1],
        expected_symbols[2],
        expected_symbols[3],
        node("compute", 12, (4, 0, 7, 1), (4, 3, 4, 10), [node("count", 13, (4, 11, 4, 16), (4, 11, 4, 16))]),
    ]
    edited = outline_request()
    expect(edited == kept_body, "outline with replaced body line: got %r" % (edited,))

    replaced_header = list(OUTLINE)
    replaced_header[4] = "fn compute(count i32) i32 ("
    did_change(s, outline, join_lines(replaced_header), 5)
    expect(published(s.settle(), outline) != [], "replaced header line has no diagnostics")
    expect(
        outline_request() == expected_symbols[:4],
        "outline with replaced header line: got %r" % (outline_request(),),
    )

    did_change(s, outline, outline_text, 6)
    expect_published(s.settle(), outline, [], "fixed outline")
    expect(outline_request() == expected_symbols, "outline after the fix: got %r" % (outline_request(),))

    broken_a = list(MAIN)
    broken_a[22] = '    text usize = strlen(c"abc"'
    text_a = join_lines(broken_a)
    did_change(s, main, text_a, 2)
    diagnostics_a = published(s.settle(), main)
    expect(diagnostics_a != [], "broken scenario A has no diagnostics")
    expect(
        s.result("textDocument/definition", at(main, text_a, "strlen(c")) is None,
        "definition inside the edited region is not null",
    )
    expect(
        s.result("textDocument/definition", at(main, text_a, "point.x", 6)) == loc(main, 7, 15, 16),
        "definition above the edited region",
    )

    broken_b = ["broken one (", "broken two ("] + MAIN
    text_b = join_lines(broken_b)
    did_change(s, main, text_b, 3)
    diagnostics_b = published(s.settle(), main)
    expect(diagnostics_b != [], "broken scenario B has no diagnostics")
    expect(
        s.result("textDocument/definition", at(main, text_b, "broken one", 0)) is None,
        "definition inside inserted broken lines is not null",
    )
    shifted = s.result("textDocument/references", with_context(at(main, text_b, "point.x", 6), True))
    expected_shifted = [loc(main, 9, 15, 16), loc(main, 22, 20, 21), loc(main, 23, 22, 23)]
    expect(shifted == expected_shifted, "shifted references: got %r, expected %r" % (shifted, expected_shifted))

    did_close(s, main)
    s.settle()
    expect(
        s.result("textDocument/definition", at(main, main_text, "point.x", 6)) is None,
        "definition in closed main is not null",
    )
    expect(
        s.result("textDocument/definition", at(lib, lib_text, "fn helper", 3)) is None,
        "definition in lib after closing its root is not null",
    )
    c_header_navigation(s, folder)
    shutdown(s)
    s.finish(0)


run_navigation()
PY

echo "lsp_navigation: definition, references, and outline: ok"
