#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
ADDRESS_TMP=$(mktemp -d)
trap 'rm -rf "$ADDRESS_TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/address_reconstruction.fas" >"$ADDRESS_TMP/address.ll"
"$LLVM_OPT" -passes=verify "$ADDRESS_TMP/address.ll" -disable-output

ulimit -c 0 || true
for level in 0 2 3; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$ADDRESS_TMP/address.ll" \
    -o "$ADDRESS_TMP/address-$level.ll"
  "$LLVM_OPT" -passes=verify "$ADDRESS_TMP/address-$level.ll" -disable-output
  "$CC" -Werror -Wno-override-module -std=c17 -O"$level" \
    "$ADDRESS_TMP/address-$level.ll" "$ROOT/test/address_reconstruction.c" \
    -o "$ADDRESS_TMP/address-$level"
  timeout 30 "$ADDRESS_TMP/address-$level"
done

echo "address reconstruction: O0/O2/O3 verified and linked: ok"
