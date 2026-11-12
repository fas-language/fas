#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
CC=${CC:-clang-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
SHIFT_TMP=$(mktemp -d)
trap 'rm -rf "$SHIFT_TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/integer_vector_shift_counts.fas" \
  >"$SHIFT_TMP/shifts.ll"
"$LLVM_OPT" -passes=verify "$SHIFT_TMP/shifts.ll" -disable-output

for level in 0 2; do
  FAS_OPT="$LLVM_OPT" FAS_LLC="$LLVM_LLC" FAS_CC="$CC" \
    "$OCAML_FAS" -O"$level" "$ROOT/test/integer_vector_shift_counts.fas" \
    -o "$SHIFT_TMP/shifts-$level"
  "$SHIFT_TMP/shifts-$level"
done

echo "integer vector shift counts: ok"
