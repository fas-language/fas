#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
LAYOUT_TMP=$(mktemp -d)
trap 'rm -rf "$LAYOUT_TMP"' EXIT HUP INT TERM
SOURCE="$ROOT/test/generic_layout.fas"
ORACLE="$ROOT/test/generic_layout_oracle.c"
"$OCAML_FAS" --emit-llvm "$SOURCE" >"$LAYOUT_TMP/program.ll"
"$LLVM_OPT" -passes=verify "$LAYOUT_TMP/program.ll" -disable-output
for level in 0 2; do
    "$LLVM_OPT" -S "-passes=default<O$level>" "$LAYOUT_TMP/program.ll" -o "$LAYOUT_TMP/optimized.ll"
    "$LLVM_OPT" -passes=verify "$LAYOUT_TMP/optimized.ll" -disable-output
    "$OCAML_FAS" -O"$level" "$SOURCE" "$ORACLE" -o "$LAYOUT_TMP/fas"
    "$LAYOUT_TMP/fas"
done
echo "generic_layout: generic sizeof/alignof/offsetof, C oracle, O0/O2, verified LLVM: ok"
