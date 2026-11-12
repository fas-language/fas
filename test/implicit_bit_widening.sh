#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
WIDEN_TMP=$(mktemp -d)
trap 'rm -rf "$WIDEN_TMP"' EXIT HUP INT TERM
fail() { echo "implicit_bit_widening: $*" >&2; exit 1; }
"$OCAML_FAS" --emit-llvm "$ROOT/test/implicit_bit_widening.fas" >"$WIDEN_TMP/program.ll"
"$LLVM_OPT" -passes=verify "$WIDEN_TMP/program.ll" -disable-output
if grep -E 'inbounds|dso_local|nonnull|noalias|llvm.global_ctors|nsw|nuw' "$WIDEN_TMP/program.ll"; then
  fail "unexpected facts or runtime initialization"
fi
"$CC" -std=c17 -Werror -O0 -DBIT_WIDENING_ORACLE "$ROOT/test/implicit_bit_widening_oracle.c" "$ROOT/test/implicit_bit_widening_runtime.c" -o "$WIDEN_TMP/oracle"
timeout 30 "$WIDEN_TMP/oracle" >"$WIDEN_TMP/oracle.out"
for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$WIDEN_TMP/program.ll" -o "$WIDEN_TMP/optimized.ll"
  "$LLVM_OPT" -passes=verify "$WIDEN_TMP/optimized.ll" -disable-output
  "$OCAML_FAS" -O"$level" -c "$ROOT/test/implicit_bit_widening.fas" -o "$WIDEN_TMP/program.o"
  "$CC" -O"$level" "$WIDEN_TMP/program.o" "$ROOT/test/implicit_bit_widening_runtime.c" -o "$WIDEN_TMP/fas"
  timeout 30 "$WIDEN_TMP/fas" >"$WIDEN_TMP/fas.out"
  cmp "$WIDEN_TMP/fas.out" "$WIDEN_TMP/oracle.out" || fail "O$level differs from C oracle"
done
echo "implicit_bit_widening: C oracle, O0/O2, verified LLVM before and after -O2: ok"
