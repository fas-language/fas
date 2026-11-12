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
dune build --root "$ROOT" test/float_proof.exe
PROOF="$ROOT/_build/default/test/float_proof.exe"
case ${1:-all} in
  all)
    for section in unary binary literal; do "$PROOF" "$section"; done
    ;;
  runtime) ;;
  unary|binary|literal) "$PROOF" "$1"; exit ;;
  *) exit 2 ;;
esac
for group in 0 1 2 3 4 5; do
  python3 "$ROOT/test/float_proof_runtime.py" "$TMP/runtime.fas" "$group"
  "$OCAML_FAS" --emit-llvm "$TMP/runtime.fas" > "$TMP/runtime.ll"
  "$LLVM_OPT" -passes=verify "$TMP/runtime.ll" -disable-output
  for level in 0 2; do
    "$LLVM_OPT" -S "-passes=default<O$level>" "$TMP/runtime.ll" -o "$TMP/optimized.ll"
    "$LLVM_OPT" -passes=verify "$TMP/optimized.ll" -disable-output
    "$LLVM_LLC" -O"$level" -relocation-model=pic -filetype=obj "$TMP/optimized.ll" -o "$TMP/runtime.o"
    "$CC" -shared "$TMP/runtime.o" -lm -o "$TMP/runtime.so"
    "$PROOF" runtime "$TMP/runtime.so" "$group"
  done
done
