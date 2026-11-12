#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ulimit -Sv 4194304
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM

assert_no_fast_math() {
  if grep -E '(^|[ ,])(nnan|ninf|nsz|arcp|contract|afn|reassoc|fast)([ ,]|$)|llvm\.fmuladd|llvm\.fma' "$1"; then
    echo "float types: fast-math flag or fused intrinsic in $1" >&2
    exit 1
  fi
}

"$OCAML_FAS" --emit-llvm "$ROOT/test/float_types.fas" >"$TMP/float_types.ll"
"$LLVM_OPT" -passes=verify "$TMP/float_types.ll" -disable-output
"$CC" -Werror -std=c17 -O0 -ffp-contract=off -fno-fast-math \
  -c "$ROOT/test/float_types.c" -o "$TMP/oracle.o"

for level in 0 2; do
  if [ "$level" -eq 0 ]; then
    cp "$TMP/float_types.ll" "$TMP/float_types.O$level.ll"
  else
    "$LLVM_OPT" -S "-passes=default<O$level>" "$TMP/float_types.ll" \
      -o "$TMP/float_types.O$level.ll"
  fi
  assert_no_fast_math "$TMP/float_types.O$level.ll"
  "$LLVM_OPT" -passes=verify "$TMP/float_types.O$level.ll" -disable-output
  "$LLVM_LLC" -relocation-model=pic -filetype=obj "$TMP/float_types.O$level.ll" \
    -o "$TMP/float_types.O$level.o"
  "$CC" -Werror -std=c17 -O0 -ffp-contract=off -fno-fast-math \
    "$TMP/float_types.O$level.o" "$TMP/oracle.o" -o "$TMP/float_types.O$level"
  timeout 30 "$TMP/float_types.O$level"
done

echo "float types: globals, constants, arrays, struct layout, raw/view/volatile memory, parameters, results and copy at O0/O2: ok"
