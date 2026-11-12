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

cat >"$WORK/fake-pkg-config" <<'SH'
#!/bin/sh
case "$1" in
  --list-all) printf '%s\n' 'zlib zlib' ;;
  --cflags)
    printf '%s\n' "$2" >>"$LSP_PKG_LOG"
    case "$2" in
      good) printf '%s\n' '-DLSP_PKG=1 -pthread' ;;
      bare) printf '\n' ;;
      *)
        printf 'Package %s was not found in the pkg-config search path.\n' "$2" >&2
        exit 1
        ;;
    esac
    ;;
  --libs) printf '%s\n' "$2" >>"$LSP_LIBS_LOG" ;;
  *) exit 2 ;;
esac
SH
chmod +x "$WORK/fake-pkg-config"
: >"$WORK/pkg.log"
: >"$WORK/libs.log"
LSP_PKG_LOG=$WORK/pkg.log
LSP_LIBS_LOG=$WORK/libs.log
FAS_PKG_CONFIG=$WORK/fake-pkg-config
export LSP_PKG_LOG LSP_LIBS_LOG FAS_PKG_CONFIG

python3 -I - "$OCAML_FAS" "$WORK" <<'PY'
import json
import os
import re
import select
import subprocess
import sys
import urllib.parse

exe = sys.argv[1]
work = sys.argv[2]
real_cc = os.environ["REAL_CC"]
TIMEOUT = 60
servers = []

GOOD_HEADER = """#include <stdint.h>

#define CN_LIMIT 42

struct CnPoint {
  int32_t x;
  int32_t y;
};

typedef struct CnPoint CnPoint;

int cn_add(int a, int b);

int cn_mixed(int first, int, int third);

int cn_fmt(const char *format, ...);

int cn_none(int, int);

int cn_kw(int view);

CnPoint cn_scale(CnPoint point, int, const char *name) __attribute__((nonnull(3)));

int cn_sum(struct CnPoint point);
"""

GOOD_MAIN = "\n".join(
    [
        'use "C" <string.h>',
        'use "C" <stdlib.h>',
        'use "C" <stdio.h>',
        'use "C" "cn.h"',
        "fn main() i32 {",
        '  count usize = strlen(c"abc")',
        "  raw addr = malloc(8)",
        '  copied addr = memcpy(raw, c"abc", 4)',
        "  point CnPoint = {.x = 1, .y = 2}",
        "  limit i32 = CN_LIMIT",
        "  total i32 = cn_add(point.x, point.y)",
        "  mixed i32 = cn_mixed(1, 2, 3)",
        '  shown i32 = cn_fmt(c"%d")',
        "  unnamed i32 = cn_none(1, 2)",
        "  keyed i32 = cn_kw(3)",
        '  printed i32 = printf(c"x")',
        "  scaled CnPoint",
        '  scaled = cn_scale(point, 2, c"s")',
        "  summed i32 = cn_sum(scaled)",
        "  return 0",
        "}",
    ]
) + "\n"

BAD_HEADER = """#include <stdint.h>

int cn_ok(int value);

long double cn_wide(long double value);
"""

BAD_BROKEN = "\n".join(
    [
        'use "C" "bad.h"',
        "fn main() i32 {",
        "  ok i32 = cn_ok(1)",
        "  wide i32 = cn_wide(1)",
        "  return 0",
        "}",
    ]
) + "\n"

BAD_FIXED = BAD_BROKEN.replace("wide i32 = cn_wide(1)", "wide i32 = cn_ok(2)")

BAD_SHIFTED = "\n" + BAD_BROKEN

OTHER = "\n".join(
    [
        "fn main() i32 {",
        "  wide i32 = cn_wide(1)",
        "  return 0",
        "}",
    ]
) + "\n"

def fail(message):
    for server in servers:
        server.proc.kill()
    sys.stderr.write("lsp_c_names: %s\n" % message)
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


def read_text(path):
    with open(path, encoding="utf-8", newline="") as handle:
        return handle.read()


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
        expect(
            reply.get("error", {}).get("code") == -32601,
            "%s: settle got %r" % (self.name, reply),
        )
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


def valid_range(value):
    expect(isinstance(value, dict) and set(value) == {"start", "end"}, "range shape: %r" % (value,))
    for point in (value["start"], value["end"]):
        expect(isinstance(point, dict) and set(point) == {"line", "character"}, "position shape: %r" % (value,))
        expect(type(point["line"]) is int and type(point["character"]) is int, "position type: %r" % (value,))
    start = (value["start"]["line"], value["start"]["character"])
    end = (value["end"]["line"], value["end"]["character"])
    expect(start <= end, "range order: %r" % (value,))


def hover_text(result):
    if result is None:
        return None
    expect(set(result) == {"contents", "range"}, "hover keys: %r" % (result,))
    valid_range(result["range"])
    expect(set(result["contents"]) == {"kind", "value"}, "hover contents keys: %r" % (result,))
    expect(result["contents"]["kind"] == "markdown", "hover kind: %r" % (result,))
    return result["contents"]["value"]


def hover_at(s, path, source, anchor, offset=0):
    result = s.result("textDocument/hover", at(path, source, anchor, offset))
    return result, hover_text(result)


def expect_value(s, path, source, anchor, offset, expected, label):
    _, value = hover_at(s, path, source, anchor, offset)
    expect(value == expected, "%s: hover at %r+%d: got %r, expected %r" % (label, anchor, offset, value, expected))


def clang_paths(name):
    probe = os.path.join(work, "probe.c")
    write_file(probe, "#include <%s>\n" % name)
    output = subprocess.run(
        [real_cc, "-H", "-fsyntax-only", probe],
        capture_output=True,
        text=True,
        timeout=TIMEOUT,
        check=True,
    ).stderr
    paths = set()
    for line in output.splitlines():
        match = re.match(r"^\.+ (/.+)$", line)
        if match:
            paths.add(match.group(1))
    expect(paths, "clang -H lists no headers for %s" % name)
    return paths


def find_line(path, pattern):
    matches = [
        number
        for number, line in enumerate(read_text(path).split("\n"), 1)
        if re.match(pattern, line)
    ]
    expect(len(matches) == 1, "%s: pattern %r matches lines %r" % (path, pattern, matches))
    return matches[0]


def c_declaration(text, line):
    lines = text.split("\n")
    start = sum(len(item) + 1 for item in lines[: line - 1])
    depth_paren = 0
    depth_brace = 0
    raw = []
    index = start
    while index < len(text):
        if text.startswith("/*", index):
            index = text.index("*/", index + 2) + 2
            continue
        if text.startswith("//", index):
            index = text.index("\n", index)
            continue
        char = text[index]
        if char in "\"'":
            end = index + 1
            while text[end] != char:
                end += 2 if text[end] == "\\" else 1
            raw.append(text[index : end + 1])
            index = end + 1
            continue
        raw.append(char)
        if char == "(":
            depth_paren += 1
        elif char == ")":
            depth_paren -= 1
        elif char == "{":
            depth_brace += 1
        elif char == "}":
            depth_brace -= 1
            if depth_brace == 0:
                break
        elif char == ";" and depth_paren == 0 and depth_brace == 0:
            break
        index += 1
    joined = "".join(raw)
    keep_lines = "{" in joined
    collapsed = re.sub(
        r"\s+",
        lambda match: "\n" if keep_lines and "\n" in match.group(0) else " ",
        joined,
    )
    return collapsed.strip()


def split_location(value):
    path, _, number = value.rpartition("\n")[2].rpartition(":")
    return path, int(number)


def expect_c_hover(s, path, source, anchor, offset, fas, facts, pattern, allowed, label):
    result, value = hover_at(s, path, source, anchor, offset)
    expect(value is not None, "%s: no hover at %r+%d" % (label, anchor, offset))
    header, number = split_location(value)
    expect(header in allowed, "%s: header %r not in %r" % (label, header, sorted(allowed)))
    line = find_line(header, pattern)
    expect(number == line, "%s: header line %d, grep found %d" % (label, number, line))
    declaration = c_declaration(read_text(header), line)
    expected = "\n\n".join(
        ["```fas\n%s\n```" % fas]
        + facts
        + ["```c\n%s\n```" % declaration, "%s:%d" % (header, line)]
    )
    expect(value == expected, "%s: got %r, expected %r" % (label, value, expected))
    return result


UNSUPPORTED_REASON = "floating-point types are not supported"


def check_names_reason(bad):
    output = subprocess.run(
        [exe, "check", bad],
        capture_output=True,
        text=True,
        timeout=TIMEOUT,
    )
    text = output.stdout + output.stderr
    expect(
        "C declaration `cn_wide` is not supported: %s" % UNSUPPORTED_REASON in text,
        "fas check does not report the cn_wide reason: %r" % (text,),
    )


def expect_unsupported(s, path, source, anchor, offset, reason, header_path, pattern, label):
    result, value = hover_at(s, path, source, anchor, offset)
    expect(value is not None, "%s: no hover at %r+%d" % (label, anchor, offset))
    header, number = split_location(value)
    expect(header == header_path, "%s: header %r, expected %r" % (label, header, header_path))
    line = find_line(header_path, pattern)
    expect(number == line, "%s: header line %d, grep found %d" % (label, number, line))
    expected = "C declaration `cn_wide` is not supported: %s\n\n%s:%d" % (reason, header_path, line)
    expect(value == expected, "%s: got %r, expected %r" % (label, value, expected))
    position_line, character = position_of(source, anchor, offset)
    expect(
        result["range"] == rng(position_line, character, position_line, character + len("cn_wide")),
        "%s: range %r" % (label, result["range"]),
    )


def pins_good(s, main, cn_h):
    system_string = clang_paths("string.h")
    system_stdlib = clang_paths("stdlib.h")
    system_stdio = clang_paths("stdio.h")
    local = {cn_h}
    expect_c_hover(
        s, main, GOOD_MAIN, "strlen(c", 1,
        "extern fn strlen(__s addr) usize",
        ["nonnull: __s; C string: __s"],
        r"^extern .*[ *]strlen \(", system_string, "strlen",
    )
    expect_c_hover(
        s, main, GOOD_MAIN, "malloc(8)", 0,
        "extern fn malloc(__size usize) addr",
        ["alloc_size: __size"],
        r"^extern .*[ *]malloc \(", system_stdlib, "malloc",
    )
    expect_c_hover(
        s, main, GOOD_MAIN, "memcpy(raw", 0,
        "extern fn memcpy(__dest addr, __src addr, __n usize) addr",
        ["nonnull: __dest, __src"],
        r"^extern .*[ *]memcpy \(", system_string, "memcpy",
    )
    expect_c_hover(
        s, main, GOOD_MAIN, "CnPoint =", 0,
        "struct CnPoint @align(4)",
        ["```text\nsize 8, align 4\nx  i32  at offset 0\ny  i32  at offset 4\n```"],
        r"^struct CnPoint \{", local, "struct CnPoint",
    )
    expect_c_hover(
        s, main, GOOD_MAIN, "point.x", 6,
        "x i32 at offset 0",
        [],
        r"^\s+int32_t x;", local, "field x",
    )
    expect_c_hover(
        s, main, GOOD_MAIN, "cn_add(point", 0,
        "extern fn cn_add(a i32, b i32) i32",
        [],
        r"^int cn_add\(", local, "cn_add",
    )
    expect_c_hover(
        s, main, GOOD_MAIN, "cn_mixed(1", 0,
        "extern fn cn_mixed(first i32, arg1 i32, third i32) i32",
        [],
        r"^int cn_mixed\(", local, "cn_mixed",
    )
    expect_c_hover(
        s, main, GOOD_MAIN, "cn_fmt(c", 0,
        "extern fn cn_fmt(format addr, ...) i32",
        ["C string: format"],
        r"^int cn_fmt\(", local, "cn_fmt",
    )
    expect_c_hover(
        s, main, GOOD_MAIN, "cn_none(1", 0,
        "extern fn cn_none(arg0 i32, arg1 i32) i32",
        [],
        r"^int cn_none\(", local, "cn_none",
    )
    expect_c_hover(
        s, main, GOOD_MAIN, "cn_kw(3", 0,
        "extern fn cn_kw(view i32) i32",
        [],
        r"^int cn_kw\(", local, "cn_kw",
    )
    expect_c_hover(
        s, main, GOOD_MAIN, "cn_scale(point", 0,
        "extern fn cn_scale(point CnPoint, arg1 i32, name addr) CnPoint",
        ["nonnull: name; C string: name"],
        r"^CnPoint cn_scale\(", local, "cn_scale",
    )
    expect_c_hover(
        s, main, GOOD_MAIN, "cn_sum(scaled", 0,
        "extern fn cn_sum(point CnPoint) i32",
        [],
        r"^int cn_sum\(", local, "cn_sum",
    )
    expect_c_hover(
        s, main, GOOD_MAIN, "printf(c", 0,
        "extern fn printf(__format addr, ...) i32",
        ["C string: __format"],
        r"^extern .*[ *]printf \(", system_stdio, "printf",
    )
    expect_value(
        s, main, GOOD_MAIN, "CN_LIMIT", 0,
        "```fas\nconst CN_LIMIT i32 = 42\n```\n\n```c\n#define CN_LIMIT 42\n```\n\n%s:3" % cn_h,
        "macro CN_LIMIT",
    )
    expect(
        s.result("textDocument/definition", at(main, GOOD_MAIN, "CN_LIMIT", 0)) == loc(cn_h, 2, 8, 8),
        "definition of macro CN_LIMIT",
    )


def run_c_names():
    s = Server("c_names")
    result = initialize(s)
    expect(result["capabilities"].get("hoverProvider") is True, "capability hoverProvider is not true")
    expect(result["capabilities"].get("definitionProvider") is True, "capability definitionProvider is not true")

    folder = os.path.join(work, "good")
    main = os.path.join(folder, "main.fas")
    cn_h = os.path.realpath(os.path.join(folder, "cn.h"))
    write_file(cn_h, GOOD_HEADER)
    write_file(main, GOOD_MAIN)
    did_open(s, main, GOOD_MAIN, 1)
    expect(published(s.settle(), main) == [], "open good main has diagnostics")

    pins_good(s, main, cn_h)

    expect(
        s.result("textDocument/definition", at(main, GOOD_MAIN, "CnPoint =", 0)) == loc(cn_h, 4, 0, 0),
        "definition of CnPoint",
    )
    expect(
        s.result("textDocument/definition", at(main, GOOD_MAIN, "point.x", 6)) == loc(cn_h, 5, 0, 0),
        "definition of field x",
    )

    bad_folder = os.path.join(work, "bad")
    bad_h = os.path.realpath(os.path.join(bad_folder, "bad.h"))
    bad = os.path.join(bad_folder, "bad.fas")
    other = os.path.join(bad_folder, "other.fas")
    write_file(bad_h, BAD_HEADER)
    write_file(bad, BAD_BROKEN)
    check_names_reason(bad)
    write_file(other, OTHER)

    did_open(s, other, OTHER, 1)
    expect(published(s.settle(), other) != [], "other.fas should fail check")
    expect_value(s, other, OTHER, "cn_wide(1", 0, None, "outside the closure")

    did_open(s, bad, BAD_BROKEN, 1)
    expect(published(s.settle(), bad) != [], "broken bad.fas has no diagnostics")
    expect_unsupported(
        s, bad, BAD_BROKEN, "cn_wide(1", 0, UNSUPPORTED_REASON, bad_h,
        r"^long double cn_wide\(", "unsupported cn_wide",
    )

    did_change(s, bad, BAD_FIXED, 2)
    expect(published(s.settle(), bad) == [], "fixed bad.fas has diagnostics")
    expect_c_hover(
        s, bad, BAD_FIXED, "cn_ok(1", 0,
        "extern fn cn_ok(value i32) i32",
        [],
        r"^int cn_ok\(", {bad_h}, "cn_ok",
    )

    did_change(s, bad, BAD_BROKEN, 3)
    expect(published(s.settle(), bad) != [], "restored cn_wide line has no diagnostics")
    expect_unsupported(
        s, bad, BAD_BROKEN, "cn_wide(1", 0, UNSUPPORTED_REASON, bad_h,
        r"^long double cn_wide\(", "unsupported cn_wide in changed region",
    )
    expect_c_hover(
        s, bad, BAD_BROKEN, "cn_ok(1", 0,
        "extern fn cn_ok(value i32) i32",
        [],
        r"^int cn_ok\(", {bad_h}, "cn_ok from snapshot",
    )

    did_change(s, bad, BAD_SHIFTED, 4)
    expect(published(s.settle(), bad) != [], "shifted bad.fas has no diagnostics")
    expect_unsupported(
        s, bad, BAD_SHIFTED, "cn_wide(1", 0, UNSUPPORTED_REASON, bad_h,
        r"^long double cn_wide\(", "unsupported cn_wide after line insert",
    )
    expect_value(s, other, OTHER, "cn_wide(1", 0, None, "outside the closure after bad root")

    pins_good(s, main, cn_h)

    did_close(s, other)
    did_close(s, bad)
    did_close(s, main)
    s.settle()
    shutdown(s)
    s.finish(0)


PKG_GOOD_HOVER = '```sh\npkg-config --cflags "good"\n```\n\n```text\n-DLSP_PKG=1\n-pthread\n```'
PKG_BARE_HOVER = '```sh\npkg-config --cflags "bare"\n```\n\n(none)'
PKG_BROKEN_HOVER = (
    '```sh\npkg-config --cflags "broken"\n```\n\n'
    "Package broken was not found in the pkg-config search path."
)


def pkg_source(modules):
    return 'use pkg "%s"\nuse "C" <pkg_hover.h>\n\nfn main() i32 {\n    return 0\n}\n' % modules


def pkg_calls():
    return len(read_text(os.path.join(work, "pkg.log")).splitlines())


def expect_pkg_hover(s, name, modules, expected):
    path = os.path.join(work, "pkg", name + ".fas")
    source = pkg_source(modules)
    write_file(path, source)
    did_open(s, path, source, 1)
    expect(bool(published(s.settle(), path)), "%s: unresolved header has no diagnostics" % name)
    anchor = '"%s"' % modules
    start = position_of(source, anchor, 0)
    end = position_of(source, anchor, len(anchor))
    for offset in (1, len(modules)):
        before = pkg_calls()
        result = s.result("textDocument/hover", at(path, source, anchor, offset))
        expect(pkg_calls() == before + 1, "%s: hover ran pkg-config %d times" % (name, pkg_calls() - before))
        expect(result is not None, "%s: pkg hover is null" % name)
        value = hover_text(result)
        expect(value == expected, "%s: pkg hover %r, expected %r" % (name, value, expected))
        expect(
            result["range"] == rng(start[0], start[1], end[0], end[1]),
            "%s: pkg hover range %r" % (name, result["range"]),
        )
    did_close(s, path)
    s.settle()


def run_pkg_hover():
    s = Server("pkg_hover")
    initialize(s)
    expect_pkg_hover(s, "good", "good", PKG_GOOD_HOVER)
    expect_pkg_hover(s, "bare", "bare", PKG_BARE_HOVER)
    expect_pkg_hover(s, "broken", "broken", PKG_BROKEN_HOVER)
    expect(read_text(os.path.join(work, "libs.log")) == "", "pkg hover ran pkg-config --libs")
    shutdown(s)
    s.finish(0)


MO_HEADER_A = """#define MO_ONE 1
#define MO_MID 2
"""

MO_HEADER_B = """#define MO_TWO 2
#undef MO_MID
#define MO_MID 4
"""

MO_MAIN = "\n".join(
    [
        'use "C" <stdio.h>',
        'use "C" "mo_a.h"',
        'use "C" "mo_b.h"',
        'use "C" <<MO_BLK',
        "#define MO_BLOCK 11",
        "static int mo_block(void) { return MO_BLOCK; }",
        "MO_BLK",
        "",
        "fn main() i32 {",
        "  first i32 = MO_ONE",
        "  mid i32 = MO_MID",
        "  second i32 = MO_TWO",
        "  eof i32 = EOF",
        "  blk i32 = MO_BLOCK",
        "  return 0",
        "}",
    ]
) + "\n"

FLAG_MAIN = "\n".join(
    [
        'use pkg "good"',
        'use "C" "mo_a.h"',
        "",
        "fn main() i32 {",
        "  flag i32 = LSP_PKG",
        "  return 0",
        "}",
    ]
) + "\n"


def macro_text(fas, define, header, line):
    return "\n\n".join(
        ["```fas\n%s\n```" % fas, "```c\n%s\n```" % define, "%s:%d" % (header, line)]
    )


def expect_macro(s, path, source, anchor, fas, define, header, line, label):
    expect(
        read_text(header).split("\n")[line - 1] == define,
        "%s: %s line %d is not %r" % (label, header, line, define),
    )
    expect_value(s, path, source, anchor, 2, macro_text(fas, define, header, line), label)
    expect(
        s.result("textDocument/definition", at(path, source, anchor, 2)) == loc(header, line - 1, 8, 8),
        "%s: definition" % label,
    )


def run_macro_origins():
    s = Server("macro_origins")
    initialize(s)
    folder = os.path.join(work, "macros")
    header_a = os.path.realpath(os.path.join(folder, "mo_a.h"))
    header_b = os.path.realpath(os.path.join(folder, "mo_b.h"))
    main = os.path.join(folder, "mo.fas")
    flag = os.path.join(folder, "flag.fas")
    write_file(header_a, MO_HEADER_A)
    write_file(header_b, MO_HEADER_B)
    write_file(main, MO_MAIN)
    write_file(flag, FLAG_MAIN)
    did_open(s, main, MO_MAIN, 1)
    expect(published(s.settle(), main) == [], "macro main has diagnostics")

    expect_macro(
        s, main, MO_MAIN, "= MO_ONE", "const MO_ONE i32 = 1", "#define MO_ONE 1",
        header_a, 1, "macro MO_ONE",
    )
    expect_macro(
        s, main, MO_MAIN, "= MO_MID", "const MO_MID i32 = 4", "#define MO_MID 4",
        header_b, 3, "macro MO_MID after undef",
    )
    expect_macro(
        s, main, MO_MAIN, "= MO_TWO", "const MO_TWO i32 = 2", "#define MO_TWO 2",
        header_b, 1, "macro MO_TWO",
    )
    expect_macro(
        s, main, MO_MAIN, "= MO_BLOCK", "const MO_BLOCK i32 = 11", "#define MO_BLOCK 11",
        main, 5, "macro MO_BLOCK in C block",
    )

    stdio = [path for path in clang_paths("stdio.h") if os.path.basename(path) == "stdio.h"]
    expect(len(stdio) == 1, "clang -H lists %r for stdio.h" % (stdio,))
    eof_lines = [
        number
        for number, line in enumerate(read_text(stdio[0]).split("\n"), 1)
        if re.match(r"^#\s*define\s+EOF\b", line)
    ]
    if eof_lines:
        expect_macro(
            s, main, MO_MAIN, "= EOF", "const EOF i32 = -1", "#define EOF (-1)",
            stdio[0], eof_lines[0], "macro EOF",
        )
    else:
        print("lsp_c_names: skipped macro EOF, stdio.h has no #define EOF")

    did_open(s, flag, FLAG_MAIN, 1)
    s.settle()
    expect_value(s, flag, FLAG_MAIN, "= LSP_PKG", 2, None, "command-line macro hover")
    expect(
        s.result("textDocument/definition", at(flag, FLAG_MAIN, "= LSP_PKG", 2)) is None,
        "command-line macro definition is not null",
    )

    did_close(s, flag)
    did_close(s, main)
    s.settle()
    shutdown(s)
    s.finish(0)


run_c_names()
run_pkg_hover()
run_macro_origins()
PY

echo "lsp_c_names: imported C name hovers, unsupported names, definitions, pkg flag hovers, and macro origins: ok"
