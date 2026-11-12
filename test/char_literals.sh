#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
CHAR_TMP=$(mktemp -d)
trap 'rm -rf "$CHAR_TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/char_literals.fas" >"$CHAR_TMP/char_literals.ll"
"$LLVM_OPT" -passes=verify "$CHAR_TMP/char_literals.ll" -disable-output
for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$CHAR_TMP/char_literals.ll" \
    -o "$CHAR_TMP/char_literals-$level.ll"
  "$LLVM_OPT" -passes=verify "$CHAR_TMP/char_literals-$level.ll" -disable-output
  "$CC" -Werror -Wno-override-module -std=c17 -O"$level" \
    "$CHAR_TMP/char_literals-$level.ll" -o "$CHAR_TMP/fas-$level"
  "$CC" -Werror -std=c17 -O"$level" "$ROOT/test/char_literals.c" \
    -o "$CHAR_TMP/c-$level"
  "$CHAR_TMP/fas-$level" >"$CHAR_TMP/fas-$level.out"
  "$CHAR_TMP/c-$level" >"$CHAR_TMP/c-$level.out"
  cmp "$CHAR_TMP/c-$level.out" "$CHAR_TMP/fas-$level.out"
done

echo "character literals: C oracle matched at O0/O2; LLVM verified before and after optimization: ok"
