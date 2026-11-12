#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
LLVM_OPT=${LLVM_OPT:-opt-22}
COMPONENT_TMP=$(mktemp -d)
trap 'rm -rf "$COMPONENT_TMP"' EXIT HUP INT TERM
"$OCAML_FAS" --emit-llvm "$ROOT/test/literal_aggregates.fas" > "$COMPONENT_TMP/program.ll"
"$LLVM_OPT" -passes=verify "$COMPONENT_TMP/program.ll" -disable-output
for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$COMPONENT_TMP/program.ll" -o "$COMPONENT_TMP/optimized.ll"
  "$LLVM_OPT" -passes=verify "$COMPONENT_TMP/optimized.ll" -disable-output
  "$OCAML_FAS" -O"$level" "$ROOT/test/literal_aggregates.fas" -o "$COMPONENT_TMP/program"
  "$COMPONENT_TMP/program"
done
OCAML_FAS="$OCAML_FAS" FAS_OPT="$LLVM_OPT" LLVM_OPT="$LLVM_OPT" python3 "$ROOT/test/literal_aggregates.py"
echo 'literal_aggregates: constants and relocations against C oracle at O0/O2: ok'
