#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
BATTERY_TMP=$(mktemp -d)
trap 'rm -rf "$BATTERY_TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/memory_battery.fas" >"$BATTERY_TMP/memory.ll"
"$LLVM_OPT" -passes=verify "$BATTERY_TMP/memory.ll" -disable-output

ulimit -c 0 || true
for level in 0 2 3; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$BATTERY_TMP/memory.ll" \
    -o "$BATTERY_TMP/memory-$level.ll"
  "$LLVM_OPT" -passes=verify "$BATTERY_TMP/memory-$level.ll" -disable-output
  "$CC" -Werror -Wno-override-module -std=c17 -O"$level" \
    "$BATTERY_TMP/memory-$level.ll" "$ROOT/test/memory_battery.c" \
    -o "$BATTERY_TMP/memory-$level"
  timeout 30 "$BATTERY_TMP/memory-$level"
done

echo "memory battery: O0/O2/O3 verified and linked: ok"
