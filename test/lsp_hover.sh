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

HOVER = [
    'use "C" <string.h>',
    'use "C" "mixed.h"',
    'use "C" "layouts.h"',
    'use "C" "colors.h"',
    "",
    "const LIMIT i32 = 4",
    "const MASK u32 = 4294967295",
    "const SMALL u8 = 200",
    "const WIDE u16 = 65535",
    "const NEGATIVE i32 = -5",
    "var GLOBAL i32 = 5",
    "const GLYPHS arr[3, i32] = {1, 2, 3}",
    "var TABLE arr[2, u8] = {4, 5}",
    "const ORIGIN Point = {.x = 7, .y = 8}",
    "const PAIR Point = {3, 4}",
    "const ORIGIN_X addr = &GLOBAL",
    "const ELEMENT addr = &TABLE[0]",
    "const ORIGIN_FIELD addr = &ORIGIN.x",
    "const SHIFTED addr = &TABLE[LIMIT - 3]",
    "",
    "struct Point { x i32 y i32 }",
    "struct Packet @align(16) { tag u8 length u32 body arr[3, u16] pointer addr count u64 }",
    "struct Box[T] { value T }",
    "struct Tagged[N const usize] { tag u8 wide u64 data arr[N, u8] }",
    "struct Blend[T, N const usize] { head T tail arr[N, u8] }",
    "opaque Ctx",
    "",
    "fn identity[T](value T) T {",
    "    return value",
    "}",
    "",
    "fn lanes[T, N const usize](value T) usize {",
    "    return N",
    "}",
    "",
    "fn pick[A, B](first A, second B) B {",
    "    return second",
    "}",
    "",
    "fn sum[T](left T, right T) T {",
    "    held T = left + right",
    "    return held",
    "}",
    "",
    "fn read_window[T](source addr) T {",
    "    view window = source[T]",
    "    return window",
    "}",
    "",
    "fn first_byte(pointer addr) u8 {",
    "    view first = pointer[u8]",
    "    return first",
    "}",
    "",
    "fn take_ctx(ctx handle[Ctx]) usize {",
    "    return 0",
    "}",
    "",
    'extern "C" {',
    "    fn emit_line(fmt addr, ...) i32",
    "}",
    "",
    "fn main() i32 {",
    "    point Point = {.x = 1, .y = 2}",
    "    local i32 = point.x + GLOBAL + LIMIT + NEGATIVE",
    "    tiny u8 = identity[u8](7)",
    "    small i32 = identity[i32](local)",
    "    width usize = lanes[u8, 16](1) + lanes[u32, 4](2)",
    "    chosen u32 = pick[u8, u32](1, 2)",
    "    total u8 = sum[u8](1, 2)",
    "    wider u32 = sum[u32](1, 2)",
    '    seen u8 = read_window[u8](c"abc")',
    '    later u32 = read_window[u32](c"abc")',
    "    box Box[i32] = {7}",
    "    slots usize = len(GLYPHS)",
    "    element i32 = GLYPHS[1]",
    "    table_head u8 = TABLE[1]",
    "    small_tag Tagged[3]",
    "    big_tag Tagged[10]",
    "    blend Blend[u32, 3]",
    "    padded CPad = {}",
    "    nested CNest = {}",
    "    filled CArr = {}",
    "    narrow CPacked = {}",
    "    aligned_c CAligned = {}",
    "    mixed FasMixed = {0}",
    "    mixed_word u32 = mixed.word",
    '    count usize = strlen(c"abc")',
    '    logged i32 = emit_line(c"abc")',
    "    palette i32 = RED + GREEN + BLUE + LOW + HIGH",
    "    return small + box.value + point.y + LIMIT",
    "}",
]

C_LAYOUT = """#include <stdalign.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include "mixed.h"
#include "layouts.h"
#include "colors.h"

struct Tagged3 {
    uint8_t tag;
    uint64_t wide;
    uint8_t data[3];
};

struct Tagged10 {
    uint8_t tag;
    uint64_t wide;
    uint8_t data[10];
};

struct Blend3 {
    uint32_t head;
    uint8_t tail[3];
};

struct Point {
    int32_t x;
    int32_t y;
};

struct Packet {
    uint8_t tag;
    uint32_t length;
    uint16_t body[3];
    void *pointer;
    uint64_t count;
} __attribute__((aligned(16)));

struct BoxI32 {
    int32_t value;
};

int main(void) {
    printf("Point %zu %zu %zu %zu\\n", sizeof(struct Point), alignof(struct Point),
           offsetof(struct Point, x), offsetof(struct Point, y));
    printf("Packet %zu %zu %zu %zu %zu %zu %zu\\n", sizeof(struct Packet), alignof(struct Packet),
           offsetof(struct Packet, tag), offsetof(struct Packet, length), offsetof(struct Packet, body),
           offsetof(struct Packet, pointer), offsetof(struct Packet, count));
    printf("Box %zu %zu %zu\\n", sizeof(struct BoxI32), alignof(struct BoxI32), offsetof(struct BoxI32, value));
    printf("FasMixed %zu %zu %zu %zu %zu\\n", sizeof(union FasMixed), alignof(union FasMixed),
           offsetof(union FasMixed, word), offsetof(union FasMixed, pair), offsetof(union FasMixed, bytes));
    printf("Tagged3 %zu %zu %zu %zu %zu\\n", sizeof(struct Tagged3), alignof(struct Tagged3),
           offsetof(struct Tagged3, tag), offsetof(struct Tagged3, wide), offsetof(struct Tagged3, data));
    printf("Tagged10 %zu %zu %zu %zu %zu\\n", sizeof(struct Tagged10), alignof(struct Tagged10),
           offsetof(struct Tagged10, tag), offsetof(struct Tagged10, wide), offsetof(struct Tagged10, data));
    printf("Blend3 %zu %zu %zu %zu\\n", sizeof(struct Blend3), alignof(struct Blend3),
           offsetof(struct Blend3, head), offsetof(struct Blend3, tail));
    printf("CPad %zu %zu %zu %zu %zu\\n", sizeof(struct CPad), alignof(struct CPad),
           offsetof(struct CPad, a), offsetof(struct CPad, b), offsetof(struct CPad, c));
    printf("CNest %zu %zu %zu %zu %zu\\n", sizeof(struct CNest), alignof(struct CNest),
           offsetof(struct CNest, inner), offsetof(struct CNest, tag), offsetof(struct CNest, big));
    printf("CArr %zu %zu %zu %zu\\n", sizeof(struct CArr), alignof(struct CArr),
           offsetof(struct CArr, bytes), offsetof(struct CArr, w));
    printf("CPacked %zu %zu %zu %zu\\n", sizeof(struct CPacked), alignof(struct CPacked),
           offsetof(struct CPacked, a), offsetof(struct CPacked, b));
    printf("CAligned %zu %zu\\n", sizeof(struct CAligned), alignof(struct CAligned));
    printf("colors %d %d %d %d %d\\n", RED, GREEN, BLUE, LOW, HIGH);
    printf("hex %#lx %#x\\n", (unsigned long)4294967295UL, (unsigned)65535u);
    return 0;
}
"""

FENCE = "```fas\n"
LIMIT_T = FENCE + "const LIMIT i32 = 4\n```"
MASK_T = FENCE + "const MASK u32 = 4294967295 (0xffffffff)\n```"
SMALL_T = FENCE + "const SMALL u8 = 200\n```"
WIDE_T = FENCE + "const WIDE u16 = 65535 (0xffff)\n```"
NEGATIVE_T = FENCE + "const NEGATIVE i32 = -5\n```"
GLOBAL_T = FENCE + "GLOBAL i32\n```"
POINT_T = FENCE + "struct Point\n```\n\n```text\nsize 8, align 4\nx  i32  at offset 0\ny  i32  at offset 4\n```"
PACKET_T = (
    FENCE
    + "struct Packet @align(16)\n```\n\n```text\nsize 32, align 16\ntag      u8           at offset 0\nlength   u32          at offset 4\nbody     arr[3, u16]  at offset 8\npointer  addr         at offset 16\ncount    u64          at offset 24\n```"
)
BOX_T = FENCE + "struct Box[T]\n```\n\nvalue T\n\n```text\nBox[i32]: size 4, align 4\nvalue  i32  at offset 0\n```"
BOX_VALUE_T = FENCE + "value T\n```\n\nBox[i32]: value i32 at offset 0"
X_T = FENCE + "x i32 at offset 0\n```"
Y_T = FENCE + "y i32 at offset 4\n```"
TAG_T = FENCE + "tag u8 at offset 0\n```"
BODY_T = FENCE + "body arr[3, u16] at offset 8\n```"
POINTER_FIELD_T = FENCE + "pointer addr at offset 16\n```"
IDENTITY_SIG = FENCE + "fn identity[T](value T) T\n```"
EMIT_SIG = FENCE + "extern fn emit_line(fmt addr, ...) i32\n```"
LANES_SIG = FENCE + "fn lanes[T, N const usize](value T) usize\n```"
PICK_SIG = FENCE + "fn pick[A, B](first A, second B) B\n```"
MAIN_SIG = FENCE + "fn main() i32\n```"
IDENTITY_U8 = IDENTITY_SIG + "\n\nT = u8"
IDENTITY_I32 = IDENTITY_SIG + "\n\nT = i32"
LANES_U8 = LANES_SIG + "\n\nT = u8, N = 16"
LANES_U32 = LANES_SIG + "\n\nT = u32, N = 4"
PICK_U8 = PICK_SIG + "\n\nA = u8, B = u32"
POINT_LOCAL_T = FENCE + "point Point\n```"
TINY_T = FENCE + "tiny u8\n```"
WIDTH_T = FENCE + "width usize\n```"
CHOSEN_T = FENCE + "chosen u32\n```"
BOX_LOCAL_T = FENCE + "box Box[i32]\n```"
COUNT_T = FENCE + "count usize\n```"
SMALL_LOCAL_T = FENCE + "small i32\n```"
LOCAL_USE_T = FENCE + "local i32\n```"
VIEW_T = FENCE + "view first u8\n```"
POINTER_PARAM_T = FENCE + "pointer addr\n```"
U32_LIT_T = FENCE + "u32\n```"
I32_LIT_T = FENCE + "i32\n```"
VALUE_BODY_T = FENCE + "value T\n```\n\nT = u8\n\nT = i32"
TYPE_IDENTITY_T = FENCE + "T\n```\n\nT = u8\n\nT = i32"
N_LANES_T = FENCE + "N usize\n```\n\nN = 16\n\nN = 4"
LEFT_T = FENCE + "left T\n```\n\nT = u8\n\nT = u32"
HELD_T = FENCE + "held T\n```\n\nT = u8\n\nT = u32"
SUM_EXPR_T = FENCE + "u8 | u32\n```"
WINDOW_T = FENCE + "view window u8 | u32\n```\n\nT = u8\n\nT = u32"

MIXED_LINE = 3
MIXED_HEADER = """#include <stdint.h>

union FasMixed {
  uint32_t word;
  uint16_t pair[3];
  uint8_t bytes[5];
};
"""
LAYOUTS_HEADER = """#include <stdint.h>

struct CPad {
  uint8_t a;
  uint32_t b;
  uint8_t c;
};

struct CInner {
  uint16_t x;
  uint8_t y;
};

struct CNest {
  struct CInner inner;
  uint8_t tag;
  uint64_t big;
};

struct CArr {
  uint8_t bytes[5];
  uint16_t w;
};

struct CPacked {
  uint8_t a;
  uint32_t b;
} __attribute__((packed));

struct CAligned {
  uint8_t a;
} __attribute__((aligned(8)));
"""
MIXED_C ="```c\nunion FasMixed {\nuint32_t word;\nuint16_t pair[3];\nuint8_t bytes[5];\n}\n```"
UNION_T_FORMAT = (
    FENCE
    + "union FasMixed @align(4)\n```\n\n```text\nsize 8, align 4\nword   u32          at offset 0\npair   arr[3, u16]  at offset 0\nbytes  arr[5, u8]   at offset 0\n```"
    + "\n\n"
    + MIXED_C
    + "\n\n%s:%d"
)
MIXED_WORD_LINE = 4
WORD_T_FORMAT = (
    FENCE
    + "word u32 at offset 0\n```\n\n```c\nuint32_t word;\n```"
    + "\n\n%s:%d"
)
GLYPHS_T = FENCE + "const GLYPHS arr[3, i32]\n```\n\n3 elements"
TABLE_T = FENCE + "TABLE arr[2, u8]\n```\n\n2 elements"
OPAQUE_T = FENCE + "opaque Ctx\n```\n\nno layout"
TAGGED_T = (
    FENCE
    + "struct Tagged[N const usize]\n```\n\ntag u8\n\nwide u64\n\ndata arr[N, u8]"
    + "\n\n```text\nTagged[3]: size 24, align 8\ntag   u8          at offset 0\nwide  u64         at offset 8\ndata  arr[3, u8]  at offset 16\n```"
    + "\n\n```text\nTagged[10]: size 32, align 8\ntag   u8           at offset 0\nwide  u64          at offset 8\ndata  arr[10, u8]  at offset 16\n```"
)
TAGGED_TAG_T = FENCE + "tag u8\n```\n\nTagged[3]: tag u8 at offset 0\n\nTagged[10]: tag u8 at offset 0"
TAGGED_DATA_T = (
    FENCE
    + "data arr[N, u8]\n```\n\nTagged[3]: data arr[3, u8] at offset 16"
    + "\n\nTagged[10]: data arr[10, u8] at offset 16"
)
BLEND_T = (
    FENCE
    + "struct Blend[T, N const usize]\n```\n\nhead T\n\ntail arr[N, u8]"
    + "\n\n```text\nBlend[u32, 3]: size 8, align 4\nhead  u32         at offset 0\ntail  arr[3, u8]  at offset 4\n```"
)
CPAD_T = (
    FENCE
    + "struct CPad @align(4)\n```\n\n```text\nsize 12, align 4\na  u8   at offset 0\nb  u32  at offset 4\nc  u8   at offset 8\n```"
    + "\n\n```c\nstruct CPad {\nuint8_t a;\nuint32_t b;\nuint8_t c;\n}\n```\n\n%s:%d"
)
CNEST_T = (
    FENCE
    + "struct CNest @align(8)\n```\n\n```text\nsize 16, align 8\ninner  CInner  at offset 0\ntag    u8      at offset 4\nbig    u64     at offset 8\n```"
    + "\n\n```c\nstruct CNest {\nstruct CInner inner;\nuint8_t tag;\nuint64_t big;\n}\n```\n\n%s:%d"
)
CARR_T = (
    FENCE
    + "struct CArr @align(2)\n```\n\n```text\nsize 8, align 2\nbytes  arr[5, u8]  at offset 0\nw      u16         at offset 6\n```"
    + "\n\n```c\nstruct CArr {\nuint8_t bytes[5];\nuint16_t w;\n}\n```\n\n%s:%d"
)
CPACKED_T = (
    FENCE
    + "struct CPacked @align(1)\n```\n\n```text\nsize 5, align 1\na  u8   at offset 0\nb  u32  at offset 1\n```"
    + "\n\n```c\nstruct CPacked {\nuint8_t a;\nuint32_t b;\n}\n```\n\n%s:%d"
)
CALIGNED_T = (
    FENCE
    + "struct CAligned @align(8)\n```\n\n```text\nsize 8, align 8\na  u8  at offset 0\n```"
    + "\n\n```c\nstruct CAligned {\nuint8_t a;\n}\n```\n\n%s:%d"
)
COLORS_HEADER = """enum color { RED, GREEN = 5, BLUE };
typedef enum { LOW, HIGH } level;
"""
ENUM_RED_T = FENCE + "const RED i32 = 0\n```\n\nfrom enum color\n\n%s:1"
ENUM_GREEN_T = FENCE + "const GREEN i32 = 5\n```\n\nfrom enum color\n\n%s:1"
ENUM_BLUE_T = FENCE + "const BLUE i32 = 6\n```\n\nfrom enum color\n\n%s:1"
ANON_LOW_T = FENCE + "const LOW i32 = 0\n```\n\nfrom anonymous enum\n\n%s:2"
ANON_HIGH_T = FENCE + "const HIGH i32 = 1\n```\n\nfrom anonymous enum\n\n%s:2"
ORIGIN_T = (
    FENCE
    + "const ORIGIN Point\n```\n\n```text\nsize 8, align 4\nx  i32  at offset 0\ny  i32  at offset 4\n```"
)
PAIR_T = (
    FENCE
    + "const PAIR Point\n```\n\n```text\nsize 8, align 4\nx  i32  at offset 0\ny  i32  at offset 4\n```"
)
ORIGIN_X_T = FENCE + "const ORIGIN_X addr = &GLOBAL\n```"
ELEMENT_T = FENCE + "const ELEMENT addr = &TABLE[0]\n```"
ORIGIN_FIELD_T = FENCE + "const ORIGIN_FIELD addr = &ORIGIN.x\n```"
SHIFTED_T = FENCE + "const SHIFTED addr\n```"
PINS = [
    ("const LIMIT", 6, LIMIT_T),
    ("+ LIMIT + NEGATIVE", 2, LIMIT_T),
    ("const MASK", 6, MASK_T),
    ("const SMALL", 6, SMALL_T),
    ("const WIDE", 6, WIDE_T),
    ("const NEGATIVE", 6, NEGATIVE_T),
    ("+ LIMIT + NEGATIVE", 12, NEGATIVE_T),
    ("var GLOBAL", 4, GLOBAL_T),
    ("struct Point", 7, POINT_T),
    ("struct Packet", 7, PACKET_T),
    ("struct Box", 7, BOX_T),
    ("{ x i32", 2, X_T),
    ("y i32 }", 0, Y_T),
    ("tag u8 length", 0, TAG_T),
    ("body arr[3, u16]", 0, BODY_T),
    ("pointer addr count", 0, POINTER_FIELD_T),
    ("point.y", 6, Y_T),
    ("fn identity", 3, IDENTITY_SIG),
    ("fn lanes", 3, LANES_SIG),
    ("fn pick", 3, PICK_SIG),
    ("fn main", 3, MAIN_SIG),
    ("identity[u8", 2, IDENTITY_U8),
    ("identity[i32", 2, IDENTITY_I32),
    ("lanes[u8", 2, LANES_U8),
    ("lanes[u32", 2, LANES_U32),
    ("pick[u8", 2, PICK_U8),
    ("point Point", 0, POINT_LOCAL_T),
    ("tiny u8", 0, TINY_T),
    ("width usize", 0, WIDTH_T),
    ("chosen u32", 0, CHOSEN_T),
    ("box Box[i32]", 0, BOX_LOCAL_T),
    ("count usize", 0, COUNT_T),
    ("small i32", 0, SMALL_LOCAL_T),
    ("(local)", 1, LOCAL_USE_T),
    ("return small", 7, SMALL_LOCAL_T),
    ("view first", 5, VIEW_T),
    ("return first", 7, VIEW_T),
    ("(pointer addr", 1, POINTER_PARAM_T),
    ("pointer[u8]", 0, POINTER_PARAM_T),
    ("pick[u8, u32](1, 2)", 17, U32_LIT_T),
    ("{.x = 1", 6, I32_LIT_T),
    ("return value", 7, VALUE_BODY_T),
    ("(value T) T", 1, VALUE_BODY_T),
    ("identity[T", 9, TYPE_IDENTITY_T),
    ("lanes[T, N const usize]", 9, N_LANES_T),
    ("return N", 7, N_LANES_T),
    ("left T", 0, LEFT_T),
    ("held T", 0, HELD_T),
    ("return held", 7, HELD_T),
    ("left + right", 5, SUM_EXPR_T),
    ("view window", 5, WINDOW_T),
    ("return window", 7, WINDOW_T),
    ("{ value", 2, BOX_VALUE_T),
    ("box.value", 4, BOX_VALUE_T),
    ("const GLYPHS", 6, GLYPHS_T),
    ("GLYPHS[1", 0, GLYPHS_T),
    ("len(GLYPHS", 4, GLYPHS_T),
    ("var TABLE", 4, TABLE_T),
    ("TABLE[1", 0, TABLE_T),
    ("opaque Ctx", 7, OPAQUE_T),
    ("Ctx]) usize", 0, OPAQUE_T),
    ("struct Tagged", 7, TAGGED_T),
    ("small_tag Tagged", 10, TAGGED_T),
    ("big_tag Tagged", 8, TAGGED_T),
    ("tag u8 wide", 0, TAGGED_TAG_T),
    ("data arr[N", 0, TAGGED_DATA_T),
    ("struct Blend", 7, BLEND_T),
    ("blend Blend", 6, BLEND_T),
    ("const ORIGIN Point", 6, ORIGIN_T),
    ("const PAIR Point", 6, PAIR_T),
    ("const ORIGIN_X addr", 6, ORIGIN_X_T),
    ("const ELEMENT addr", 6, ELEMENT_T),
    ("const ORIGIN_FIELD addr", 6, ORIGIN_FIELD_T),
    ("const SHIFTED addr", 6, SHIFTED_T),
    ("fn emit_line", 3, EMIT_SIG),
    ("emit_line(c", 0, EMIT_SIG),
    ("fn main", 0, None),
]

def fail(message):
    for server in servers:
        server.proc.kill()
    sys.stderr.write("lsp_hover: %s\n" % message)
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


def expect_hover(s, path, source, anchor, offset, expected, label):
    got = hover_text(s.result("textDocument/hover", at(path, source, anchor, offset)))
    expect(got == expected, "%s: hover at %r+%d: got %r, expected %r" % (label, anchor, offset, got, expected))


def c_oracle():
    source = os.path.join(work, "layout.c")
    binary = os.path.join(work, "layout")
    write_file(os.path.join(work, "mixed.h"), MIXED_HEADER)
    write_file(os.path.join(work, "layouts.h"), LAYOUTS_HEADER)
    write_file(os.path.join(work, "colors.h"), COLORS_HEADER)
    write_file(source, C_LAYOUT)
    subprocess.run([os.environ["REAL_CC"], "-std=c11", "-o", binary, source], check=True, timeout=TIMEOUT)
    output = subprocess.run([binary], check=True, capture_output=True, text=True, timeout=TIMEOUT).stdout
    rows = {}
    for line in output.splitlines():
        name, *values = line.split()
        rows[name] = [int(value, 0) for value in values]
    return rows


def run_hover():
    s = Server("hover")
    result = initialize(s)
    expect(result["capabilities"].get("hoverProvider") is True, "capability hoverProvider is not true")
    folder = os.path.join(work, "hover")
    main = os.path.join(folder, "main.fas")
    mixed_h = os.path.realpath(os.path.join(folder, "mixed.h"))
    write_file(mixed_h, MIXED_HEADER)
    layouts_h = os.path.realpath(os.path.join(folder, "layouts.h"))
    write_file(layouts_h, LAYOUTS_HEADER)
    colors_h = os.path.realpath(os.path.join(folder, "colors.h"))
    write_file(colors_h, COLORS_HEADER)
    expect(MIXED_HEADER.split("\n")[MIXED_LINE - 1].startswith("union FasMixed"), "mixed.h line")
    text = join_lines(HOVER)
    write_file(main, text)
    did_open(s, main, text, 1)
    expect_published(s.settle(), main, [], "open main")

    oracle = c_oracle()
    expect(oracle.get("Point") == [8, 4, 0, 4], "C layout of Point: %r" % (oracle.get("Point"),))
    expect(oracle.get("Packet") == [32, 16, 0, 4, 8, 16, 24], "C layout of Packet: %r" % (oracle.get("Packet"),))
    expect(oracle.get("Box") == [4, 4, 0], "C layout of Box: %r" % (oracle.get("Box"),))
    expect(oracle.get("hex") == [0xFFFFFFFF, 0xFFFF], "C hex values: %r" % (oracle.get("hex"),))
    expect(oracle.get("FasMixed") == [8, 4, 0, 0, 0], "C layout of FasMixed: %r" % (oracle.get("FasMixed"),))
    expect(oracle.get("Tagged3") == [24, 8, 0, 8, 16], "C layout of Tagged3: %r" % (oracle.get("Tagged3"),))
    expect(oracle.get("Tagged10") == [32, 8, 0, 8, 16], "C layout of Tagged10: %r" % (oracle.get("Tagged10"),))
    expect(oracle.get("Blend3") == [8, 4, 0, 4], "C layout of Blend3: %r" % (oracle.get("Blend3"),))
    expect(oracle.get("CPad") == [12, 4, 0, 4, 8], "C layout of CPad: %r" % (oracle.get("CPad"),))
    expect(oracle.get("CNest") == [16, 8, 0, 4, 8], "C layout of CNest: %r" % (oracle.get("CNest"),))
    expect(oracle.get("CArr") == [8, 2, 0, 6], "C layout of CArr: %r" % (oracle.get("CArr"),))
    expect(oracle.get("CPacked") == [5, 1, 0, 1], "C layout of CPacked: %r" % (oracle.get("CPacked"),))
    expect(oracle.get("CAligned") == [8, 8], "C layout of CAligned: %r" % (oracle.get("CAligned"),))
    expect(oracle.get("colors") == [0, 5, 6, 0, 1], "C enum values: %r" % (oracle.get("colors"),))

    union_text = UNION_T_FORMAT % (mixed_h, MIXED_LINE)
    word_text = WORD_T_FORMAT % (mixed_h, MIXED_WORD_LINE)
    for anchor, offset, expected in PINS + [
        ("FasMixed = {0}", 0, union_text),
        ("mixed.word", 6, word_text),
        ("padded CPad", 7, CPAD_T % (layouts_h, 3)),
        ("nested CNest", 7, CNEST_T % (layouts_h, 14)),
        ("filled CArr", 7, CARR_T % (layouts_h, 20)),
        ("narrow CPacked", 7, CPACKED_T % (layouts_h, 25)),
        ("aligned_c CAligned", 10, CALIGNED_T % (layouts_h, 30)),
        ("RED + GREEN", 0, ENUM_RED_T % colors_h),
        ("GREEN + BLUE", 0, ENUM_GREEN_T % colors_h),
        ("BLUE + LOW", 0, ENUM_BLUE_T % colors_h),
        ("+ LOW + HIGH", 2, ANON_LOW_T % colors_h),
        ("LOW + HIGH", 6, ANON_HIGH_T % colors_h),
    ]:
        expect_hover(s, main, text, anchor, offset, expected, "open")
    strlen = hover_text(s.result("textDocument/hover", at(main, text, "strlen(c", 1)))
    expect(
        strlen is not None
        and strlen.startswith(
            "```fas\nextern fn strlen(__s addr) usize\n```\n\nnonnull: __s; C string: __s\n\n```c\nextern size_t strlen"
        )
        and re.search(r"\n\n[^\n]+:[0-9]+$", strlen) is not None,
        "imported C name hover for strlen: %r" % (strlen,),
    )

    name = s.result("textDocument/hover", at(main, text, "return value", 7))
    line, character = position_of(text, "return value", 7)
    expect(name["range"] == rng(line, character, line, character + 5), "name range: %r" % (name,))
    expression = s.result("textDocument/hover", at(main, text, "left + right", 5))
    line, character = position_of(text, "left + right", 0)
    expect(
        expression["range"] == rng(line, character, line, character + 12),
        "expression range: %r" % (expression,),
    )

    broken = list(HOVER)
    row = next(index for index, line in enumerate(HOVER) if line.startswith("    width usize"))
    broken[row] = "    width usize = lanes[u8, 16](1) +"
    broken_text = join_lines(broken)
    did_change(s, main, broken_text, 2)
    expect(published(s.settle(), main) != [], "broken edit has no diagnostics")
    expect_hover(s, main, broken_text, "width usize", 0, None, "broken edit")
    expect_hover(s, main, broken_text, "chosen u32", 0, CHOSEN_T, "broken edit below")
    expect_hover(s, main, broken_text, "const LIMIT", 6, LIMIT_T, "broken edit above")
    expect_hover(s, main, broken_text, "+ LIMIT + NEGATIVE", 2, LIMIT_T, "broken edit above use")

    inserted_text = join_lines(["broken one ("] + HOVER)
    did_change(s, main, inserted_text, 3)
    expect(published(s.settle(), main) != [], "inserted broken line has no diagnostics")
    expect_hover(s, main, inserted_text, "broken one", 0, None, "inserted broken line")
    expect_hover(s, main, inserted_text, "const LIMIT", 6, LIMIT_T, "shifted const")
    expect_hover(s, main, inserted_text, "chosen u32", 0, CHOSEN_T, "shifted local")

    did_change(s, main, text, 4)
    expect_published(s.settle(), main, [], "fixed main")
    expect_hover(s, main, text, "const LIMIT", 6, LIMIT_T, "fixed")

    did_close(s, main)
    s.settle()
    shutdown(s)
    s.finish(0)


run_hover()
PY

echo "lsp_hover: hover kinds, layouts, and broken-edit mapping: ok"
