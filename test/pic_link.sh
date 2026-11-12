#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
READELF=${READELF:-readelf}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
PIC_TMP=$(mktemp -d)
trap 'rm -rf "$PIC_TMP"' EXIT HUP INT TERM

fail() {
    echo "pic_link: $*" >&2
    exit 1
}

check_no_textrel() {
    if "$READELF" -d "$1" | grep -F TEXTREL >/dev/null; then
        fail "$1 contains TEXTREL"
    fi
}

for level in 0 2; do
    "$OCAML_FAS" -O"$level" -c "$ROOT/test/pic_link.fas" -o "$PIC_TMP/pic-$level.o"
    "$CC" -Werror -std=c17 -O"$level" "$ROOT/test/pic_link.c" \
        "$PIC_TMP/pic-$level.o" -o "$PIC_TMP/pic-$level"
    "$PIC_TMP/pic-$level" >"$PIC_TMP/actual"
    printf 'pic-link: ok\n' >"$PIC_TMP/expected"
    cmp "$PIC_TMP/expected" "$PIC_TMP/actual" || fail "O$level PIE execution differed"

    "$CC" -shared "$PIC_TMP/pic-$level.o" -o "$PIC_TMP/libpic-$level.so"
    check_no_textrel "$PIC_TMP/libpic-$level.so"

    "$OCAML_FAS" -O"$level" -c "$ROOT/test/pic_asm.fas" -o "$PIC_TMP/asm-$level.o"
    "$CC" -Werror -std=c17 -O"$level" -DPIC_ASM "$ROOT/test/pic_link.c" \
        "$PIC_TMP/pic-$level.o" "$PIC_TMP/asm-$level.o" -o "$PIC_TMP/asm-$level"
    "$PIC_TMP/asm-$level" >"$PIC_TMP/actual"
    cmp "$PIC_TMP/expected" "$PIC_TMP/actual" || fail "O$level raw assembly execution differed"

    "$CC" -shared "$PIC_TMP/pic-$level.o" "$PIC_TMP/asm-$level.o" \
        -o "$PIC_TMP/libasm-$level.so"
    check_no_textrel "$PIC_TMP/libasm-$level.so"
done

"$OCAML_FAS" "$ROOT/test/ir_simple.fas" -o "$PIC_TMP/fas-pie"
if ! "$READELF" -h "$PIC_TMP/fas-pie" | grep -F 'Type:                              DYN' >/dev/null; then
    fail "Fas executable is not PIE"
fi

echo "PIC object and executable links: ok"
