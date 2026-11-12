#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
COMPONENT_TMP=$(mktemp -d)
trap 'rm -rf "$COMPONENT_TMP"' EXIT HUP INT TERM
ulimit -c 0 || true

for level in 0 2; do
  "$OCAML_FAS" --emit-llvm -O"$level" "$ROOT/test/c_import_types.fas" \
    >"$COMPONENT_TMP/program-O$level.ll"
  "$LLVM_OPT" -passes=verify "$COMPONENT_TMP/program-O$level.ll" -disable-output
  "$LLVM_OPT" -S "-passes=default<O$level>" "$COMPONENT_TMP/program-O$level.ll" \
    -o "$COMPONENT_TMP/program-O$level.opt.ll"
  "$LLVM_OPT" -passes=verify "$COMPONENT_TMP/program-O$level.opt.ll" -disable-output
  "$OCAML_FAS" -O"$level" "$ROOT/test/c_import_types.fas" \
    "$ROOT/test/c_import_types_runtime.c" -o "$COMPONENT_TMP/fas-O$level"
  "$CC" -std=gnu17 -Werror -O"$level" \
    "$ROOT/test/c_import_types_oracle.c" \
    "$ROOT/test/c_import_types_runtime.c" -o "$COMPONENT_TMP/oracle-O$level"
  timeout 30 "$COMPONENT_TMP/fas-O$level"
  timeout 30 "$COMPONENT_TMP/oracle-O$level"
done

echo "c_import_types: O0/O2 C oracle and LLVM verification passed"
