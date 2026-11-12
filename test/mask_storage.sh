#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
MASK_STORAGE_TMP=$(mktemp -d)
trap 'rm -rf "$MASK_STORAGE_TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/mask_storage.fas" >"$MASK_STORAGE_TMP/mask_storage.ll"
"$LLVM_OPT" -passes=verify "$MASK_STORAGE_TMP/mask_storage.ll" -disable-output

for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" -verify-each \
    "$MASK_STORAGE_TMP/mask_storage.ll" -o "$MASK_STORAGE_TMP/mask_storage.O$level.ll"
  "$LLVM_OPT" -passes=verify "$MASK_STORAGE_TMP/mask_storage.O$level.ll" -disable-output
  "$CC" -Werror -Wno-override-module -std=c17 -O"$level" \
    "$MASK_STORAGE_TMP/mask_storage.O$level.ll" "$ROOT/test/mask_storage.c" \
    -o "$MASK_STORAGE_TMP/mask_storage.O$level"
  "$MASK_STORAGE_TMP/mask_storage.O$level"
done

echo "mask storage: ok"
