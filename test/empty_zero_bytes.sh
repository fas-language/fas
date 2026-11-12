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

"$CC" -Werror -std=c17 "$ROOT/test/empty_zero_oracle.c" -o "$TMP/oracle"
"$TMP/oracle" >"$TMP/expected"
"$OCAML_FAS" --emit-llvm "$ROOT/test/empty_zero_bytes.fas" >"$TMP/program.ll"
"$LLVM_OPT" -passes=verify "$TMP/program.ll" -disable-output
for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$TMP/program.ll" \
    -o "$TMP/program.O$level.ll"
  "$LLVM_OPT" -passes=verify "$TMP/program.O$level.ll" -disable-output
  "$OCAML_FAS" -O"$level" "$ROOT/test/empty_zero_bytes.fas" \
    -o "$TMP/program.O$level"
  timeout 30 "$TMP/program.O$level" >"$TMP/actual"
  diff -u "$TMP/expected" "$TMP/actual"
done

echo 'empty_zero_bytes: local, array, nested vector and union bytes at O0/O2: ok'
