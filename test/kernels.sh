#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
KERNEL_TMP=$(mktemp -d)
trap 'rm -rf "$KERNEL_TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/kernels.fas" >"$KERNEL_TMP/kernels.ll"
"$LLVM_OPT" -passes=verify "$KERNEL_TMP/kernels.ll" -disable-output

ulimit -c 0 2>/dev/null || true
for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$KERNEL_TMP/kernels.ll" \
    -o "$KERNEL_TMP/kernels.O$level.ll"
  "$LLVM_OPT" -passes=verify "$KERNEL_TMP/kernels.O$level.ll" -disable-output
  "$CC" -Werror -Wno-override-module -std=c17 -O"$level" \
    "$KERNEL_TMP/kernels.O$level.ll" "$ROOT/test/kernels.c" \
    -o "$KERNEL_TMP/kernels.O$level"
  if [ "$level" -eq 2 ]; then
    timeout 120 "$KERNEL_TMP/kernels.O$level" --measure
  else
    timeout 120 "$KERNEL_TMP/kernels.O$level"
  fi
done

printf 'kernels: O0/O2 verified and linked: ok\n'
