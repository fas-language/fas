#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM
ulimit -c 0 || true

"$CC" -Werror -std=gnu17 "$ROOT/test/c_import_acceptance_oracle.c" \
  "$ROOT/test/c_import_acceptance_runtime.c" -o "$TMP/oracle"
timeout 30 "$TMP/oracle"
for level in 0 2; do
  "$OCAML_FAS" --emit-llvm "$ROOT/test/c_import_acceptance.fas" \
    >"$TMP/program.ll"
  "$LLVM_OPT" -passes=verify "$TMP/program.ll" -disable-output
  "$LLVM_OPT" -S "-passes=default<O$level>" "$TMP/program.ll" \
    -o "$TMP/program-O$level.ll"
  "$LLVM_OPT" -passes=verify "$TMP/program-O$level.ll" -disable-output
  "$OCAML_FAS" -O"$level" "$ROOT/test/c_import_acceptance.fas" \
    "$ROOT/test/c_import_acceptance_runtime.c" -o "$TMP/fas-O$level"
  timeout 30 "$TMP/fas-O$level"
done
echo "c_import_acceptance: pointer, enum, offset and macro ABI checks passed at O0/O2"
