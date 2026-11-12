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
"$CC" -std=c17 -Werror -O0 -ffp-contract=off -fno-fast-math \
  "$ROOT/test/float_programs_oracle.c" "$ROOT/test/float_programs_driver.c" -lm -o "$TMP/oracle"
"$OCAML_FAS" --emit-llvm "$ROOT/test/float_programs.fas" > "$TMP/programs.ll"
"$LLVM_OPT" -passes=verify "$TMP/programs.ll" -disable-output
sizes='16 20 1 64 16'
if [ "${FULL:-0}" = 1 ]; then sizes='128 2000 8 1024 256'; fi
kind=0
for size in $sizes; do
  "$TMP/oracle" "$kind" "$size" > "$TMP/expected.$kind"
  kind=$((kind + 1))
done
for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$TMP/programs.ll" -o "$TMP/optimized.ll"
  "$LLVM_OPT" -passes=verify "$TMP/optimized.ll" -disable-output
  "$LLVM_LLC" -O"$level" -relocation-model=pic -filetype=obj "$TMP/optimized.ll" -o "$TMP/programs.o"
  "$CC" -O0 -ffp-contract=off -fno-fast-math "$TMP/programs.o" \
    "$ROOT/test/float_programs_driver.c" -lm -o "$TMP/programs"
  kind=0
  for size in $sizes; do
    "$TMP/programs" "$kind" "$size" > "$TMP/actual"
    cmp "$TMP/expected.$kind" "$TMP/actual"
    echo "float programs: kind=$kind size=$size O$level byte-identical"
    kind=$((kind + 1))
  done
done
