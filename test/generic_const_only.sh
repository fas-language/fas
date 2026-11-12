#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu
ulimit -Sv 4194304
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM

SOURCE="$ROOT/test/generic_const_only.fas"
"$OCAML_FAS" --emit-llvm "$SOURCE" >"$TMP/program.ll"
"$LLVM_OPT" -passes=verify "$TMP/program.ll" -disable-output
for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$TMP/program.ll" -o "$TMP/optimized.ll"
  "$LLVM_OPT" -passes=verify "$TMP/optimized.ll" -disable-output
  "$OCAML_FAS" -O"$level" "$SOURCE" -o "$TMP/program"
  timeout 30 "$TMP/program"
done
echo 'generic_const_only: concrete generic structs in const-only functions, C oracle, O0/O2: ok'
