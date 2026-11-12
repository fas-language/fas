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
fail() { echo "c_enum_types: $*" >&2; exit 1; }

"$OCAML_FAS" --emit-llvm "$ROOT/test/c_enum_types.fas" >"$TMP/program.ll"
"$LLVM_OPT" -passes=verify "$TMP/program.ll" -disable-output
for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$TMP/program.ll" -o "$TMP/program.O$level.ll"
  "$LLVM_OPT" -passes=verify "$TMP/program.O$level.ll" -disable-output
  "$OCAML_FAS" -O"$level" -o "$TMP/fas.O$level" \
    "$ROOT/test/c_enum_types.fas" "$ROOT/test/c_enum_types_runtime.c"
  "$CC" -Werror -std=c17 -O"$level" "$ROOT/test/c_enum_types_oracle.c" \
    "$ROOT/test/c_enum_types_runtime.c" -o "$TMP/oracle.O$level"
  "$TMP/fas.O$level" >"$TMP/fas.O$level.out" \
    || fail "Fas O$level enum behavior failed"
  "$TMP/oracle.O$level" >"$TMP/oracle.O$level.out" \
    || fail "C O$level enum oracle failed"
  cmp -s "$TMP/fas.O$level.out" "$TMP/oracle.O$level.out" \
    || fail "Fas and C enum results differ at O$level"
done
echo "c_enum_types: enum constants, ABI types, fields and constant tables at O0/O2: ok"
