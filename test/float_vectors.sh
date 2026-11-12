#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu
ulimit -Sv 4194304
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_OBJDUMP=${LLVM_OBJDUMP:-llvm-objdump-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/float_vectors.fas" >"$TMP/float_vectors.ll"
"$LLVM_OPT" -passes=verify "$TMP/float_vectors.ll" -disable-output
if grep -Eq 'getelementptr inbounds| (nsw|nuw)( |$)' "$TMP/float_vectors.ll"; then
  echo 'float vectors: LLVM contains an unproved pointer or arithmetic fact' >&2
  exit 1
fi
if grep -E 'llvm\.fmuladd|(^|[ ,])(nnan|ninf|nsz|arcp|contract|afn|reassoc|fast)([ ,]|$)' \
  "$TMP/float_vectors.ll" >/dev/null; then
  echo 'float vectors: unexpected fused operation or fast-math flag' >&2
  exit 1
fi
for width in v2f32 v4f32 v8f32 v16f32 v2f64 v4f64 v8f64; do
  grep -F "@llvm.fma.$width" "$TMP/float_vectors.ll" >/dev/null || {
    echo "float vectors: missing @llvm.fma.$width" >&2
    exit 1
  }
done
"$CC" -Werror -std=c17 -O0 -ffp-contract=off -fno-fast-math -c \
  "$ROOT/test/float_vectors.c" -o "$TMP/oracle.o"

for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$TMP/float_vectors.ll" \
    -o "$TMP/float_vectors.O$level.ll"
  "$LLVM_OPT" -passes=verify "$TMP/float_vectors.O$level.ll" -disable-output
  if grep -E 'llvm\.fmuladd|(^|[ ,])(nnan|ninf|nsz|arcp|contract|afn|reassoc|fast)([ ,]|$)' \
    "$TMP/float_vectors.O$level.ll" >/dev/null; then
    echo "float vectors: O$level LLVM contains a fused operation or fast-math flag" >&2
    exit 1
  fi
  "$CC" -Werror -Wno-override-module -std=c17 -O0 -ffp-contract=off \
    -fno-fast-math -c "$TMP/float_vectors.O$level.ll" \
    -o "$TMP/float_vectors.O$level.o"
  "$CC" "$TMP/float_vectors.O$level.o" "$TMP/oracle.o" -lm \
    -o "$TMP/float_vectors.O$level"
  timeout 90 "$TMP/float_vectors.O$level"
done

"$LLVM_OBJDUMP" -d --disassemble-symbols=float_vectors_plain32 \
  "$TMP/float_vectors.O2.o" >"$TMP/plain.disassembly"
if grep -E 'vfmadd' "$TMP/plain.disassembly" >/dev/null; then
  echo 'float vectors: ordinary multiply-add contracted in the O2 object' >&2
  exit 1
fi

echo 'float vectors: f32/f64 lane counts 2/4/8/16, C oracle, O0/O2 and no automatic contraction: ok'
