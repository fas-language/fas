#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
SWITCH_LABELS_TMP=$(mktemp -d)
trap 'rm -rf "$SWITCH_LABELS_TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/switch_labels.fas" >"$SWITCH_LABELS_TMP/input.ll"
"$LLVM_OPT" -passes=verify "$SWITCH_LABELS_TMP/input.ll" -disable-output
for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$SWITCH_LABELS_TMP/input.ll" \
    -o "$SWITCH_LABELS_TMP/optimized-$level.ll"
  "$LLVM_OPT" -passes=verify "$SWITCH_LABELS_TMP/optimized-$level.ll" -disable-output
  "$CC" -Werror -Wno-override-module -std=c17 -O"$level" \
    "$SWITCH_LABELS_TMP/optimized-$level.ll" -o "$SWITCH_LABELS_TMP/fas-$level"
  "$CC" -Werror -std=c17 -O"$level" "$ROOT/test/switch_labels.c" \
    -o "$SWITCH_LABELS_TMP/c-$level"
  timeout 30 "$SWITCH_LABELS_TMP/fas-$level" >"$SWITCH_LABELS_TMP/fas-$level.out"
  timeout 30 "$SWITCH_LABELS_TMP/c-$level" >"$SWITCH_LABELS_TMP/c-$level.out"
  cmp "$SWITCH_LABELS_TMP/c-$level.out" "$SWITCH_LABELS_TMP/fas-$level.out"
done

echo "switch and loop labels: C oracle matched at O0/O2; LLVM verified before and after optimization: ok"
