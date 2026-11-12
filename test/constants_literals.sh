#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
CONSTANTS_TMP=$(mktemp -d)
trap 'rm -rf "$CONSTANTS_TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/constants_literals.fas" >"$CONSTANTS_TMP/constants.ll"
"$LLVM_OPT" -passes=verify "$CONSTANTS_TMP/constants.ll" -disable-output

ulimit -c 0 || true
for level in 0 2 3; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$CONSTANTS_TMP/constants.ll" \
    -o "$CONSTANTS_TMP/constants-$level.ll"
  "$LLVM_OPT" -passes=verify "$CONSTANTS_TMP/constants-$level.ll" -disable-output
  "$CC" -Werror -Wno-override-module -std=c17 -O"$level" \
    "$CONSTANTS_TMP/constants-$level.ll" "$ROOT/test/constants_literals.c" \
    -o "$CONSTANTS_TMP/constants-$level"
  timeout 30 "$CONSTANTS_TMP/constants-$level"
done

echo "constants and literals: O0/O2/O3 verified and linked: ok"
