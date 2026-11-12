#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
NUMBER_TMP=$(mktemp -d)
trap 'rm -rf "$NUMBER_TMP"' EXIT HUP INT TERM
fail() { echo "const_number_tables: $*" >&2; exit 1; }
"$OCAML_FAS" --emit-llvm "$ROOT/test/const_number_tables.fas" >"$NUMBER_TMP/program.ll"
"$LLVM_OPT" -passes=verify "$NUMBER_TMP/program.ll" -disable-output
if grep -E 'inbounds|dso_local|nonnull|noalias|llvm.global_ctors|nsw|nuw' "$NUMBER_TMP/program.ll"; then
  fail "unexpected facts or runtime initialization"
fi
grep -F '@CELLS = private constant [72 x i8] c' "$NUMBER_TMP/program.ll" >/dev/null || fail "CELLS is not static byte storage"
grep -F '@Grid = private constant [24 x i8] c' "$NUMBER_TMP/program.ll" >/dev/null || fail "Grid is not static byte storage"
grep -F '@Rows = private constant [112 x i8] c' "$NUMBER_TMP/program.ll" >/dev/null || fail "Rows is not static byte storage"
"$CC" -std=c17 -Werror -O0 -DNUMBER_ORACLE "$ROOT/test/const_number_tables_oracle.c" "$ROOT/test/const_number_tables_runtime.c" -o "$NUMBER_TMP/oracle"
timeout 30 "$NUMBER_TMP/oracle" >"$NUMBER_TMP/oracle.out"
for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$NUMBER_TMP/program.ll" -o "$NUMBER_TMP/optimized.ll"
  "$LLVM_OPT" -passes=verify "$NUMBER_TMP/optimized.ll" -disable-output
  "$OCAML_FAS" -O"$level" -c "$ROOT/test/const_number_tables.fas" -o "$NUMBER_TMP/program.o"
  "$CC" -O"$level" "$NUMBER_TMP/program.o" "$ROOT/test/const_number_tables_runtime.c" -o "$NUMBER_TMP/fas"
  timeout 30 "$NUMBER_TMP/fas" >"$NUMBER_TMP/fas.out"
  cmp "$NUMBER_TMP/fas.out" "$NUMBER_TMP/oracle.out" || fail "O$level differs from C oracle"
done
echo "const_number_tables: C oracle, O0/O2, verified LLVM before and after -O2, padded bytes: ok"
