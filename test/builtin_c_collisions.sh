#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ulimit -Sv 4194304
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/builtin_c_collisions.fas" >"$TMP/program.ll"
"$LLVM_OPT" -passes=verify "$TMP/program.ll" -disable-output
"$LLVM_OPT" -S -passes='default<O2>' -verify-each "$TMP/program.ll" \
  -o "$TMP/program-o2.ll"
"$LLVM_OPT" -passes=verify "$TMP/program-o2.ll" -disable-output

for level in 0 2; do
  "$OCAML_FAS" -O"$level" "$ROOT/test/builtin_c_collisions.fas" \
    "$ROOT/test/builtin_c_collisions.c" -o "$TMP/program-O$level"
  timeout 60 "$TMP/program-O$level"
done

echo 'builtin_c_collisions: float builtins, select, and trunc with C headers at O0/O2: ok'
