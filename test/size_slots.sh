#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
SLOT_TMP=$(mktemp -d)
trap 'rm -rf "$SLOT_TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/size_slots.fas" >"$SLOT_TMP/input.ll"
"$LLVM_OPT" -passes=verify "$SLOT_TMP/input.ll" -disable-output

for level in 0 2; do
  "$LLVM_OPT" -passes="default<O$level>" -S "$SLOT_TMP/input.ll" -o "$SLOT_TMP/opt-$level.ll"
  "$LLVM_OPT" -passes=verify "$SLOT_TMP/opt-$level.ll" -disable-output
  "$CC" -Werror -Wno-override-module -O"$level" "$SLOT_TMP/opt-$level.ll" \
    "$ROOT/test/size_slots.c" -o "$SLOT_TMP/size-slots-$level"
  "$SLOT_TMP/size-slots-$level"
done

echo "usize size slots: ok"
