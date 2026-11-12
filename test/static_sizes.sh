#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
SIZES_TMP=$(mktemp -d)
trap 'rm -rf "$SIZES_TMP"' EXIT HUP INT TERM
"$OCAML_FAS" --emit-llvm "$ROOT/test/static_sizes.fas" >"$SIZES_TMP/program.ll"
"$LLVM_OPT" -passes=verify "$SIZES_TMP/program.ll" -disable-output
for level in 0 2; do
    "$LLVM_OPT" -S "-passes=default<O$level>" "$SIZES_TMP/program.ll" -o "$SIZES_TMP/optimized.ll"
    "$LLVM_OPT" -passes=verify "$SIZES_TMP/optimized.ll" -disable-output
    "$OCAML_FAS" -O"$level" "$ROOT/test/static_sizes.fas" "$ROOT/test/static_sizes.c" -o "$SIZES_TMP/fas"
    "$CC" -std=c11 -Wall -Wextra -Werror -pedantic -O"$level" -DSTATIC_SIZES_ORACLE "$ROOT/test/static_sizes.c" -o "$SIZES_TMP/oracle"
    "$SIZES_TMP/fas" >"$SIZES_TMP/fas.out"
    "$SIZES_TMP/oracle" >"$SIZES_TMP/oracle.out"
    cmp "$SIZES_TMP/fas.out" "$SIZES_TMP/oracle.out"
done
echo "static_sizes: named static sizes, C oracle, O0/O2, verified LLVM: ok"
