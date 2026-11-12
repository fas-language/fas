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

LSP_TMP=$(mktemp -d)
TMPDIR=$LSP_TMP/tmp
mkdir -p "$LSP_TMP/work" "$TMPDIR"
WORK=$(cd "$LSP_TMP/work" && pwd -P)
export TMPDIR
trap 'rm -rf "$LSP_TMP"' EXIT HUP INT TERM

FAS_OPT=$(command -v "$LLVM_OPT")
FAS_LLC=$(command -v "$LLVM_LLC")
FAS_CC=$(command -v "$CC")
FAS_PKG_CONFIG=$WORK/fake-pkg-config
export FAS_OPT FAS_LLC FAS_CC FAS_PKG_CONFIG

python3 -I - "$OCAML_FAS" "$WORK" <<'PY'
import json
import os
import select
import subprocess
import sys
import time
import urllib.parse

exe = sys.argv[1]
work = sys.argv[2]
TIMEOUT = 60
PACING = 0.005
started = time.monotonic()
servers = []
root = os.path.join(work, "fasge")
include_dir = os.path.join(root, "include")
pkg_config = os.path.join(work, "fake-pkg-config")

SDL_PATH = "engine/sdl.fas"
MAIN_PATH = "main.fas"
CORE_PATH = "engine/core.fas"
FAKE_PATH = "include/fake.h"


def join_lines(lines):
    return "\n".join(lines) + "\n"


PKG_TEXT = """#!/bin/sh
case "$1" in
  --cflags)
    [ "$2" = fake ] || exit 2
    printf '%s\\n' '-I@INCLUDE@'
    ;;
  --libs)
    [ "$2" = fake ] || exit 2
    printf '\\n'
    ;;
  *) exit 2 ;;
esac
""".replace("@INCLUDE@", include_dir)

MAIN_TEXT = join_lines(
    [
        'use "engine/sdl.fas"',
        "",
        "fn game_init() i32 {",
        "    return 0",
        "}",
    ]
)

SDL_TEXT = join_lines(
    [
        'use pkg "fake"',
        'use "C" <fake.h>',
        'use "core.fas"',
        "",
        "fn spin(turns i32) i32 {",
        "    return turns + 1",
        "}",
        "",
        "fn rebuild(scale i32) i32 {",
        "    return scale * FAKE_LIMIT",
        "}",
        "",
        "fn main() i32 {",
        "    rect Rect = {.width = FAKE_LIMIT, .height = ORIGIN_X + 2}",
        "    drawn i32 = fake_draw(rect.width, rect.height)",
        "    kept i32 = keep(drawn)",
        "    turned i32 = spin(kept)",
        "    return game_init() + rect_area(rect.width, rect.height) + rebuild(turned)",
        "}",
    ]
)

CORE_TEXT = join_lines(
    [
        "struct Rect {",
        "    width i32",
        "    height i32",
        "}",
        "",
        "const ORIGIN_X i32 = 0",
        "",
        "fn rect_area(width i32, height i32) i32 {",
        "    return width * height",
        "}",
        "",
        "fn keep[T](value T) T {",
        "    return value",
        "}",
    ]
)

FAKE_TEXT = join_lines(
    [
        "#ifndef FAKE_H",
        "#define FAKE_H",
        "",
        "#define FAKE_LIMIT 16",
        "",
        "int fake_draw(int surface, int x);",
        "",
        "#endif",
    ]
)

RET_SCALE = "    return scale * FAKE_LIMIT"
BURST = "    scratch i32 = ("


def with_line(line):
    return SDL_TEXT.replace(RET_SCALE + "\n", RET_SCALE + "\n" + line + "\n", 1)


def burst_text(length):
    return SDL_TEXT if length == 0 else with_line(BURST[:length])


TWO_BROKEN = SDL_TEXT.replace("    return turns + 1\n}\n", "    return turns + 1\n", 1).replace(
    RET_SCALE + "\n", "    return scale *\n", 1
)

HEADER_CUT = SDL_TEXT.replace('use pkg "fake"\n', 'use pkg "fake\n', 1)

HOVER_RECT = "```fas\nstruct Rect\n```\n\n```text\nsize 8, align 4\nwidth   i32  at offset 0\nheight  i32  at offset 4\n```"
HOVER_KEEP = "```fas\nfn keep[T](value T) T\n```\n\nT = i32"
HOVER_GAME = "```fas\nfn game_init() i32\n```"
HOVER_FAKE = "```fas\nextern fn fake_draw(surface i32, x i32) i32\n```\n\n```c\nint fake_draw(int surface, int x);\n```\n\n@INCLUDE@/fake.h:6"
HOVER_LIMIT = "```fas\nconst FAKE_LIMIT i32 = 16\n```\n\n```c\n#define FAKE_LIMIT 16\n```\n\n@INCLUDE@/fake.h:4"
HOVER_AREA = "```fas\nfn rect_area(width i32, height i32) i32\n```"
HOVER_PKG = "```sh\npkg-config --cflags \"fake\"\n```\n\n```text\n-I@INCLUDE@\n```"

P_RECT = (SDL_PATH, "Rect = {", 0, 4, HOVER_RECT, (CORE_PATH, 0, 7))
P_KEEP = (SDL_PATH, "keep(drawn)", 0, 4, HOVER_KEEP, (CORE_PATH, 11, 3))
P_GAME = (SDL_PATH, "game_init()", 0, 9, HOVER_GAME, (MAIN_PATH, 2, 3))
P_FAKE = (SDL_PATH, "fake_draw(rect", 0, 9, HOVER_FAKE, (FAKE_PATH, 5, 0))
P_LIMIT = (SDL_PATH, "FAKE_LIMIT, .height", 0, 10, HOVER_LIMIT, (FAKE_PATH, 3, 8))
P_AREA = (SDL_PATH, "rect_area(rect", 0, 9, HOVER_AREA, (CORE_PATH, 7, 3))
P_PKG = (SDL_PATH, 'use pkg "fake"', 8, 6, HOVER_PKG, None)
P_MAIN = (MAIN_PATH, "game_init() i32", 0, 9, HOVER_GAME, (MAIN_PATH, 2, 3))

SDL_PROBES = [P_RECT, P_KEEP, P_GAME, P_FAKE, P_LIMIT, P_AREA, P_PKG]
ALL_PROBES = SDL_PROBES + [P_MAIN]
BURST_PROBES = [P_FAKE, P_KEEP]

HOVER_SPIN = "```fas\nfn spin(turns i32) i32\n```"
HOVER_TURNS = "```fas\nturns i32\n```"
HOVER_REBUILD = "```fas\nfn rebuild(scale i32) i32\n```"
HOVER_SCALE = "```fas\nscale i32\n```"
HOVER_MAIN = "```fas\nfn main() i32\n```"

P_SPIN = (SDL_PATH, "spin(turns", 0, 4, HOVER_SPIN, (SDL_PATH, 4, 3))
P_TURNS = (SDL_PATH, "turns i32) i32", 0, 5, HOVER_TURNS, (SDL_PATH, 4, 8))
P_TURN_USE = (SDL_PATH, "turns + 1", 0, 5, HOVER_TURNS, (SDL_PATH, 4, 8))
P_REBUILD = (SDL_PATH, "rebuild(scale", 0, 7, HOVER_REBUILD, (SDL_PATH, 8, 3))
P_SCALE = (SDL_PATH, "scale i32) i32", 0, 5, HOVER_SCALE, (SDL_PATH, 8, 11))
P_MAIN_DECL = (SDL_PATH, "main() i32", 0, 4, HOVER_MAIN, (SDL_PATH, 12, 3))

HEADER_CUT_PROBES = [P_SPIN, P_TURNS, P_TURN_USE, P_REBUILD, P_SCALE, P_MAIN_DECL]

SDL_SYMBOLS = [
    ("spin", 12, 0),
    ("turns", 13, 1),
    ("rebuild", 12, 0),
    ("scale", 13, 1),
    ("main", 12, 0),
    ("rect", 13, 1),
    ("drawn", 13, 1),
    ("kept", 13, 1),
    ("turned", 13, 1),
]
MAIN_SYMBOLS = [("game_init", 12, 0)]
HEADER_CUT_SYMBOLS = [
    ("spin", 12, 0),
    ("turns", 13, 1),
    ("rebuild", 12, 0),
    ("scale", 13, 1),
    ("main", 12, 0),
    ("rect", 13, 1),
    ("drawn", 13, 1),
    ("kept", 13, 1),
    ("turned", 13, 1),
]
HEADER_CUT_FRESH_SYMBOLS = [
    ("spin", 12, 0),
    ("turns", 13, 1),
    ("rebuild", 12, 0),
    ("scale", 13, 1),
    ("main", 12, 0),
]

INITIALIZE = {
    "processId": os.getpid(),
    "clientInfo": {"name": "Zed"},
    "rootUri": None,
    "workspaceFolders": [{"uri": None, "name": "fasge"}],
    "capabilities": {
        "workspace": {
            "workspaceFolders": True,
            "didChangeWatchedFiles": {"dynamicRegistration": True},
        },
        "textDocument": {
            "synchronization": {"didSave": True, "willSave": False, "willSaveWaitUntil": False},
            "hover": {"contentFormat": ["markdown", "plaintext"]},
            "definition": {"linkSupport": True},
            "documentSymbol": {"hierarchicalDocumentSymbolSupport": True},
            "inlayHint": {"dynamicRegistration": False},
            "publishDiagnostics": {"relatedInformation": True},
        },
    },
}


def fail(message):
    for server in servers:
        server.proc.kill()
    sys.stderr.write("lsp_session: %s\n" % message)
    sys.exit(1)


def expect(condition, message):
    if not condition:
        fail(message)


def frame_of(message):
    body = json.dumps(message).encode("utf-8")
    return b"Content-Length: %d\r\n\r\n" % len(body) + body


def uri_of(path):
    return "file://" + urllib.parse.quote(path, safe="/")


def uri(rel):
    return uri_of(os.path.join(root, rel))


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


def at(rel, source, anchor, offset=0):
    line, character = position_of(source, anchor, offset)
    return {
        "textDocument": {"uri": uri(rel)},
        "position": {"line": line, "character": character},
    }


def flatten(symbols, depth=0):
    out = []
    for symbol in symbols:
        out.append((symbol["name"], symbol["kind"], depth))
        out.extend(flatten(symbol.get("children", []), depth + 1))
    return out


def last_published(s, rel):
    found = None
    for published_uri, diagnostics in s.publishes:
        if published_uri == uri(rel):
            found = diagnostics
    return found


def expect_first_error(s, rel, message, line, start, end_line, end):
    diagnostics = last_published(s, rel)
    expect(
        diagnostics,
        "%s: expected a broken-state diagnostic, got %r" % (rel, diagnostics),
    )
    first = diagnostics[0]
    expect(first.get("severity") == 1, "%s: severity %r" % (rel, first.get("severity")))
    expect(
        first["message"].split("\n")[0] == message,
        "%s: first error %r, expected %r" % (rel, first["message"], message),
    )
    expect(
        first["range"] == rng(line, start, end_line, end),
        "%s: first error range %r" % (rel, first["range"]),
    )


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
        self.client_requests = []
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
        if "method" in message and "id" in message:
            self.client_requests.append((message["method"], message["id"]))
            self.write(frame_of({"jsonrpc": "2.0", "id": message["id"], "result": None}))
        elif "method" in message:
            if message["method"] == "textDocument/publishDiagnostics":
                params = message["params"]
                self.publishes.append((params["uri"], params["diagnostics"]))
        else:
            self.replies.append(message)

    def request_many(self, requests):
        idents = []
        for method, params in requests:
            self.next_id += 1
            idents.append(self.next_id)
            message = {"jsonrpc": "2.0", "id": self.next_id, "method": method}
            if params is not None:
                message["params"] = params
            self.write(frame_of(message))
        while len(self.replies) < len(idents):
            self.pump()
        arrived = [reply.get("id") for reply in self.replies]
        expect(arrived == idents, "%s: replies %r, expected ids %r" % (self.name, arrived, idents))
        replies = self.replies
        self.replies = []
        for reply in replies:
            expect("error" not in reply, "%s: error reply %r" % (self.name, reply))
            expect("result" in reply, "%s: reply without result %r" % (self.name, reply))
        return replies

    def call(self, method, params=None):
        return self.request_many([(method, params)])[0]["result"]

    def notify(self, method, params=None):
        message = {"jsonrpc": "2.0", "method": method}
        if params is not None:
            message["params"] = params
        self.write(frame_of(message))

    def finish(self, code):
        self.notify("exit")
        status = self.proc.wait(timeout=TIMEOUT)
        expect(status == code, "%s: exit status %d, expected %d" % (self.name, status, code))


def initialize(s):
    params = json.loads(json.dumps(INITIALIZE))
    params["rootUri"] = uri("")
    params["workspaceFolders"][0]["uri"] = uri("")
    result = s.call("initialize", params)
    expect(result["serverInfo"]["name"] == "fas", "serverInfo %r" % (result.get("serverInfo"),))
    capabilities = result["capabilities"]
    expect(capabilities["textDocumentSync"] == 1, "textDocumentSync %r" % (capabilities.get("textDocumentSync"),))
    for provider in ("definitionProvider", "documentSymbolProvider", "hoverProvider", "inlayHintProvider", "referencesProvider"):
        expect(capabilities.get(provider) is True, "%s missing" % provider)


def open_doc(s, rel, text):
    s.notify(
        "textDocument/didOpen",
        {"textDocument": {"uri": uri(rel), "languageId": "fas", "version": 1, "text": text}},
    )


def change_doc(s, versions, rel, text):
    versions[rel] += 1
    s.notify(
        "textDocument/didChange",
        {"textDocument": {"uri": uri(rel), "version": versions[rel]}, "contentChanges": [{"text": text}]},
    )


def probe_requests(texts, probes):
    requests = []
    for path, anchor, offset, _, _, _ in probes:
        params = at(path, texts[path], anchor, offset)
        requests.append(("textDocument/hover", params))
        requests.append(("textDocument/definition", params))
    return requests


def check_probes(s, texts, probes, label):
    replies = s.request_many(probe_requests(texts, probes))
    expect(len(replies) == 2 * len(probes), "%s: reply count %d" % (label, len(replies)))
    for index, probe in enumerate(probes):
        path, anchor, offset, length, value, target = probe
        line, character = position_of(texts[path], anchor, offset)
        expected_hover = {
            "contents": {"kind": "markdown", "value": value.replace("@INCLUDE@", include_dir)},
            "range": rng(line, character, line, character + length),
        }
        actual_hover = replies[2 * index]["result"]
        expect(
            actual_hover == expected_hover,
            "%s: hover %r: got %r, expected %r" % (label, anchor, actual_hover, expected_hover),
        )
        actual_definition = replies[2 * index + 1]["result"]
        if target is None:
            expect(actual_definition is None, "%s: definition %r: got %r" % (label, anchor, actual_definition))
        else:
            tpath, tline, tchar = target
            expect(
                actual_definition is not None
                and actual_definition.get("uri") == uri(tpath)
                and actual_definition.get("range", {}).get("start") == {"line": tline, "character": tchar},
                "%s: definition %r: got %r, expected %s %d:%d" % (label, anchor, actual_definition, tpath, tline, tchar),
            )


def fresh_header_cut():
    s = Server("header")
    initialize(s)
    s.notify("initialized", {})
    texts = {SDL_PATH: HEADER_CUT}
    open_doc(s, SDL_PATH, HEADER_CUT)
    check_probes(s, texts, HEADER_CUT_PROBES, "fresh header cut")
    symbols = s.call("textDocument/documentSymbol", {"textDocument": {"uri": uri(SDL_PATH)}})
    expect(
        flatten(symbols) == HEADER_CUT_FRESH_SYMBOLS,
        "fresh header cut sdl symbols %r" % (flatten(symbols),),
    )
    expect_first_error(s, SDL_PATH, "unterminated string literal", 2, 13, 2, 14)
    expect(s.call("shutdown") is None, "fresh header cut shutdown did not return null")
    s.finish(0)


def run():
    write_file(os.path.join(root, MAIN_PATH), MAIN_TEXT)
    write_file(os.path.join(root, SDL_PATH), SDL_TEXT)
    write_file(os.path.join(root, CORE_PATH), CORE_TEXT)
    write_file(os.path.join(root, FAKE_PATH), FAKE_TEXT)
    write_file(pkg_config, PKG_TEXT)
    os.chmod(pkg_config, 0o755)

    s = Server("session")
    initialize(s)
    s.notify("initialized", {})
    texts = {SDL_PATH: SDL_TEXT, MAIN_PATH: MAIN_TEXT}
    versions = {SDL_PATH: 1, MAIN_PATH: 1}
    open_doc(s, SDL_PATH, SDL_TEXT)
    open_doc(s, MAIN_PATH, MAIN_TEXT)

    hints = s.call(
        "textDocument/inlayHint",
        {"textDocument": {"uri": uri(SDL_PATH)}, "range": rng(0, 0, SDL_TEXT.count("\n") + 1, 0)},
    )
    hint_line, hint_char = position_of(SDL_TEXT, "keep(drawn)", 4)
    expect(
        hints == [{"position": {"line": hint_line, "character": hint_char}, "label": "[i32]", "kind": 1, "paddingLeft": False}],
        "inlay hints %r" % (hints,),
    )

    symbols = s.call("textDocument/documentSymbol", {"textDocument": {"uri": uri(SDL_PATH)}})
    expect(flatten(symbols) == SDL_SYMBOLS, "sdl symbols %r" % (flatten(symbols),))
    symbols = s.call("textDocument/documentSymbol", {"textDocument": {"uri": uri(MAIN_PATH)}})
    expect(flatten(symbols) == MAIN_SYMBOLS, "main symbols %r" % (flatten(symbols),))

    check_probes(s, texts, ALL_PROBES, "valid")

    for length in range(1, len(BURST) + 1):
        texts[SDL_PATH] = burst_text(length)
        change_doc(s, versions, SDL_PATH, texts[SDL_PATH])
        time.sleep(PACING)
        check_probes(s, texts, BURST_PROBES, "burst %d" % length)
        check_probes(s, texts, ALL_PROBES, "burst %d full" % length)
    expect_first_error(s, SDL_PATH, "expected an expression, found `}`", 11, 0, 11, 1)

    for length in range(len(BURST) - 1, -1, -1):
        texts[SDL_PATH] = burst_text(length)
        change_doc(s, versions, SDL_PATH, texts[SDL_PATH])
        time.sleep(PACING)
        check_probes(s, texts, BURST_PROBES, "delete %d" % length)
        check_probes(s, texts, ALL_PROBES, "delete %d full" % length)
    expect(texts[SDL_PATH] == SDL_TEXT, "delete-back did not restore the valid text")

    texts[SDL_PATH] = TWO_BROKEN
    change_doc(s, versions, SDL_PATH, TWO_BROKEN)
    check_probes(s, texts, ALL_PROBES, "two broken")
    expect_first_error(s, SDL_PATH, "expected an expression, found `fn`", 7, 0, 7, 2)

    texts[SDL_PATH] = HEADER_CUT
    change_doc(s, versions, SDL_PATH, HEADER_CUT)
    check_probes(s, texts, HEADER_CUT_PROBES, "header cut")
    symbols = s.call("textDocument/documentSymbol", {"textDocument": {"uri": uri(SDL_PATH)}})
    expect(flatten(symbols) == HEADER_CUT_SYMBOLS, "header cut sdl symbols %r" % (flatten(symbols),))
    expect_first_error(s, SDL_PATH, "unterminated string literal", 2, 13, 2, 14)
    fresh_header_cut()

    texts[SDL_PATH] = SDL_TEXT
    change_doc(s, versions, SDL_PATH, SDL_TEXT)
    check_probes(s, texts, ALL_PROBES, "restored")
    expect(last_published(s, SDL_PATH) == [], "sdl diagnostics after fix: %r" % (last_published(s, SDL_PATH),))
    expect(last_published(s, MAIN_PATH) in (None, []), "main diagnostics after fix: %r" % (last_published(s, MAIN_PATH),))

    write_file(os.path.join(root, SDL_PATH), SDL_TEXT)
    s.notify("textDocument/didSave", {"textDocument": {"uri": uri(SDL_PATH)}})
    check_probes(s, texts, ALL_PROBES, "saved")

    s.notify("textDocument/didClose", {"textDocument": {"uri": uri(SDL_PATH)}})
    check_probes(s, texts, [P_MAIN], "sdl closed")
    s.notify("textDocument/didClose", {"textDocument": {"uri": uri(MAIN_PATH)}})

    expect(s.call("shutdown") is None, "shutdown did not return null")
    s.finish(0)
    elapsed = round((time.monotonic() - started) * 1000)

    expect(
        s.client_requests == [("client/registerCapability", "fas-roots-watch")],
        "client requests %r" % (s.client_requests,),
    )
    known = {uri(SDL_PATH), uri(MAIN_PATH), uri(CORE_PATH), uri(FAKE_PATH)}
    for published_uri, _ in s.publishes:
        expect(published_uri in known, "unexpected diagnostics for %s" % published_uri)
    expect(
        any(diagnostics for published_uri, diagnostics in s.publishes if published_uri == uri(SDL_PATH)),
        "sdl never published a broken-state diagnostic",
    )
    for rel in (SDL_PATH, MAIN_PATH, CORE_PATH):
        final = last_published(s, rel)
        expect(final is None or final == [], "%s: final diagnostics %r" % (rel, final))

    print("lsp_session: %d ms" % elapsed)


run()
PY

echo "lsp_session: editor session replay: ok"
