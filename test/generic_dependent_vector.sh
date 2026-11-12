#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu
ulimit -Sv 4194304
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
LLVM_OPT=${LLVM_OPT:-opt-22}
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/generic_dependent_vector.fas" >"$TMP/program.ll"
"$LLVM_OPT" -passes=verify "$TMP/program.ll" -disable-output
for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$TMP/program.ll" -o "$TMP/optimized.ll"
  "$LLVM_OPT" -passes=verify "$TMP/optimized.ll" -disable-output
  "$OCAML_FAS" -O"$level" "$ROOT/test/generic_dependent_vector.fas" -o "$TMP/program"
  timeout 30 "$TMP/program"
done
echo 'generic_dependent_vector: brace peers against C oracle at O0/O2: ok'
