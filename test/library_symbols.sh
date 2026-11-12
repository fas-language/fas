#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM
fail() { echo "library_symbols: $*" >&2; exit 1; }

"$CC" -std=c11 -Wall -Wextra -Werror "$ROOT/test/library_symbols_oracle.c" \
  -o "$TMP/oracle"
"$TMP/oracle" || fail "independent C oracle failed"

DEFINITIONS="$ROOT/test/library_symbols.fas"
"$OCAML_FAS" --emit-llvm "$DEFINITIONS" >"$TMP/definitions.ll"
for name in memset strlen malloc; do
  grep -F "\"no-builtin-$name\"" "$TMP/definitions.ll" >/dev/null \
    || fail "missing LLVM-recognized definition guard for $name"
done
"$LLVM_OPT" -passes=verify "$TMP/definitions.ll" -disable-output

for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$TMP/definitions.ll" \
    -o "$TMP/definitions.O$level.ll"
  "$LLVM_OPT" -passes=verify "$TMP/definitions.O$level.ll" -disable-output
  "$OCAML_FAS" -O"$level" "$DEFINITIONS" -o "$TMP/fas-$level"
  "$TMP/fas-$level" || fail "Fas library-name bodies failed at O$level"
done

IMPORTED="$ROOT/test/library_symbols_imported.fas"
"$OCAML_FAS" --emit-llvm "$IMPORTED" >"$TMP/imported.ll"
if grep -F '"no-builtin-strlen"' "$TMP/imported.ll" >/dev/null; then
  fail "header-only strlen was marked no-builtin"
fi
grep -F 'call i64 @strlen' "$TMP/imported.ll" >/dev/null \
  || fail 'header-imported strlen call was absent before optimization'
"$LLVM_OPT" -passes=verify "$TMP/imported.ll" -disable-output
"$LLVM_OPT" -S '-passes=default<O2>' "$TMP/imported.ll" -o "$TMP/imported.O2.ll"
"$LLVM_OPT" -passes=verify "$TMP/imported.O2.ll" -disable-output
grep -F 'ret i32 0' "$TMP/imported.O2.ll" >/dev/null \
  || fail 'header-imported strlen did not fold at O2'
if grep -F 'call i64 @strlen' "$TMP/imported.O2.ll" >/dev/null; then
  fail 'header-imported strlen call remained at O2'
fi
for level in 0 2; do
  "$OCAML_FAS" -O"$level" "$IMPORTED" -o "$TMP/imported-fas-$level"
  "$TMP/imported-fas-$level" \
    || fail "header-imported strlen behavior failed at O$level"
done

echo "library_symbols: memset, strlen, malloc definitions and imported strlen at O0/O2: ok"
