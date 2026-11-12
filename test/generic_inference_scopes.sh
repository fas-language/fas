#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
LLVM_OPT=${LLVM_OPT:-opt-22}
COMPONENT_TMP=$(mktemp -d)
trap 'rm -rf "$COMPONENT_TMP"' EXIT HUP INT TERM
SOURCE=$ROOT/test/generic_inference_scopes.fas
ORACLE=$ROOT/test/generic_inference_scopes.c
for level in 0 2; do
  "$OCAML_FAS" --emit-llvm "$SOURCE" > "$COMPONENT_TMP/module.ll"
  "$LLVM_OPT" -passes=verify "$COMPONENT_TMP/module.ll" -disable-output
  "$LLVM_OPT" -S "-passes=default<O$level>" "$COMPONENT_TMP/module.ll" -o "$COMPONENT_TMP/optimized.ll"
  "$LLVM_OPT" -passes=verify "$COMPONENT_TMP/optimized.ll" -disable-output
  "$OCAML_FAS" -O"$level" "$SOURCE" "$ORACLE" -o "$COMPONENT_TMP/program"
  "$COMPONENT_TMP/program"
done
echo 'generic_inference_scopes: inferred types match explicit calls and C oracle at O0/O2: ok'
