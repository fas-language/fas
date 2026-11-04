#!/bin/sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
CC=${CC:-clang-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
UNARY_TMP=$(mktemp -d)
trap 'rm -rf "$UNARY_TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/integer_vector_unary.fas" \
  >"$UNARY_TMP/unary.ll"
"$LLVM_OPT" -passes=verify "$UNARY_TMP/unary.ll" -disable-output

for level in 0 2; do
  FAS_OPT="$LLVM_OPT" FAS_LLC="$LLVM_LLC" FAS_CC="$CC" \
    "$OCAML_FAS" -O"$level" "$ROOT/test/integer_vector_unary.fas" \
    -o "$UNARY_TMP/unary-$level"
  "$UNARY_TMP/unary-$level"
done

echo "integer vector unary: ok"
