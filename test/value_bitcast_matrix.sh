#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
CC=${CC:-clang-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
BITCAST_TMP=$(mktemp -d)
trap 'rm -rf "$BITCAST_TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/value_bitcast_matrix.fas" \
  >"$BITCAST_TMP/bitcast_matrix.ll"
"$LLVM_OPT" -passes=verify "$BITCAST_TMP/bitcast_matrix.ll" -disable-output

ulimit -c 0 || true
for level in 0 2 3; do
  FAS_OPT="$LLVM_OPT" FAS_LLC="$LLVM_LLC" FAS_CC="$CC" \
    "$OCAML_FAS" -O"$level" "$ROOT/test/value_bitcast_matrix.fas" \
    -o "$BITCAST_TMP/bitcast_matrix-$level"
  timeout 10 "$BITCAST_TMP/bitcast_matrix-$level"
done

echo "value bitcast matrix: ok"
