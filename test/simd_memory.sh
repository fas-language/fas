#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
SIMD_TMP=$(mktemp -d)
trap 'rm -rf "$SIMD_TMP"' EXIT HUP INT TERM

PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/test/simd_memory_generate.py" "$SIMD_TMP"
cat "$ROOT/test/simd_memory.fas" "$SIMD_TMP/simd_memory_matrix.fas" >"$SIMD_TMP/simd_memory.fas"
ulimit -c 0 2>/dev/null || true

for level in 0 2 3; do
  "$OCAML_FAS" --emit-llvm "$SIMD_TMP/simd_memory.fas" >"$SIMD_TMP/simd_memory.ll"
  if grep -Eq 'getelementptr inbounds| (nsw|nuw)( |$)' "$SIMD_TMP/simd_memory.ll"; then
    printf 'simd memory: LLVM contains an unproved pointer or arithmetic fact\n' >&2
    exit 1
  fi
  "$LLVM_OPT" -passes=verify "$SIMD_TMP/simd_memory.ll" -disable-output
  "$LLVM_OPT" -S "-passes=default<O$level>" "$SIMD_TMP/simd_memory.ll" \
    -o "$SIMD_TMP/simd_memory.O$level.ll"
  "$LLVM_OPT" -passes=verify "$SIMD_TMP/simd_memory.O$level.ll" -disable-output
  "$CC" -Werror -Wno-override-module -std=c17 -O"$level" \
    -I"$SIMD_TMP" "$SIMD_TMP/simd_memory.O$level.ll" "$ROOT/test/simd_memory.c" \
    -o "$SIMD_TMP/simd_memory.O$level"
  timeout 30 "$SIMD_TMP/simd_memory.O$level"
done

printf 'simd memory: O0/O2/O3 verified and linked: ok\n'
