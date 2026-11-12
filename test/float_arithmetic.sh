#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ulimit -Sv 4194304
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
LLVM_OBJDUMP=${LLVM_OBJDUMP:-llvm-objdump-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/float_arithmetic.fas" >"$TMP/float_arithmetic.ll"
"$LLVM_OPT" -passes=verify "$TMP/float_arithmetic.ll" -disable-output
"$CC" -Werror -std=c17 -O0 -ffp-contract=off -fno-fast-math \
  -c "$ROOT/test/float_arithmetic.c" -o "$TMP/oracle.o"

for level in 0 2; do
  if [ "$level" -eq 0 ]; then
    cp "$TMP/float_arithmetic.ll" "$TMP/float_arithmetic.O$level.ll"
  else
    "$LLVM_OPT" -S "-passes=default<O$level>" "$TMP/float_arithmetic.ll" \
      -o "$TMP/float_arithmetic.O$level.ll"
  fi
  current="$TMP/float_arithmetic.O$level.ll"
  "$LLVM_OPT" -passes=verify "$current" -disable-output
  if grep -E '(^|[ ,])(nnan|ninf|nsz|arcp|contract|afn|reassoc|fast)([ ,]|$)|llvm\.fmuladd|llvm\.fma' \
    "$current"; then
    echo "float arithmetic: fast-math flag or fused intrinsic in O$level LLVM" >&2
    exit 1
  fi
  if [ "$level" -eq 2 ]; then
    for instruction in 'fmul float' 'fadd float' 'fmul double' 'fadd double'; do
      if ! grep -q "$instruction" "$current"; then
        echo "float arithmetic: optimized LLVM lost $instruction" >&2
        exit 1
      fi
    done
  fi
  "$LLVM_LLC" -relocation-model=pic -filetype=obj "$current" \
    -o "$TMP/float_arithmetic.O$level.o"
  "$LLVM_OBJDUMP" --disassemble "$TMP/float_arithmetic.O$level.o" \
    >"$TMP/float_arithmetic.O$level.asm"
  if grep -E 'vfmadd' "$TMP/float_arithmetic.O$level.asm"; then
    echo "float arithmetic: object fused a multiply and add at O$level" >&2
    exit 1
  fi
  "$CC" -Werror -std=c17 -O0 -ffp-contract=off -fno-fast-math \
    "$TMP/float_arithmetic.O$level.o" "$TMP/oracle.o" \
    -o "$TMP/float_arithmetic.O$level"
  timeout 30 "$TMP/float_arithmetic.O$level"
done

echo "float arithmetic: IEEE bits, predicates, compound assignment and separate multiply/add at O0/O2: ok"
