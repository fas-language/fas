#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
FIXTURES=$ROOT/test/widening_help
WIDENING_TMP=$(mktemp -d)
trap 'rm -rf "$WIDENING_TMP"' EXIT HUP INT TERM

fail() {
  echo "widening_help: $*" >&2
  exit 1
}

"$OCAML_FAS" --emit-llvm "$FIXTURES/cases.fas" >"$WIDENING_TMP/cases.ll" \
  || fail "cases.fas does not compile to LLVM"
"$LLVM_OPT" -passes=verify "$WIDENING_TMP/cases.ll" -disable-output
"$LLVM_OPT" -S "-passes=default<O2>" "$WIDENING_TMP/cases.ll" -o "$WIDENING_TMP/cases.O2.ll"
"$LLVM_OPT" -passes=verify "$WIDENING_TMP/cases.O2.ll" -disable-output

"$CC" -Werror -std=c17 -O2 "$FIXTURES/oracle.c" -o "$WIDENING_TMP/oracle"
"$WIDENING_TMP/oracle" >"$WIDENING_TMP/oracle.out"

for level in 0 2; do
  exe="$WIDENING_TMP/cases.O$level"
  "$OCAML_FAS" "-O$level" -o "$exe" "$FIXTURES/cases.fas" \
    2>"$WIDENING_TMP/cases.O$level.err"
  [ ! -s "$WIDENING_TMP/cases.O$level.err" ] \
    || fail "-O$level build wrote diagnostics"
  timeout 30 "$exe" >"$exe.out" || fail "-O$level program failed"
  cmp -s "$exe.out" "$WIDENING_TMP/oracle.out" \
    || fail "-O$level program output differs from the C oracle"
done
