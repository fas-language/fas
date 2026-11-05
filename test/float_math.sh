#!/bin/sh
set -eu

ulimit -Sv 4194304
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM

"$CC" -Werror -std=c17 -O0 -ffp-contract=off -fno-fast-math \
  "$ROOT/test/float_math_oracle.c" -lm -o "$TMP/oracle"
"$TMP/oracle" >"$TMP/expected"
"$OCAML_FAS" --emit-llvm "$ROOT/test/float_math.fas" >"$TMP/float_math.ll"
"$LLVM_OPT" -passes=verify "$TMP/float_math.ll" -disable-output
for intrinsic in sqrt fma floor ceil roundeven fabs; do
  for width in f32 f64; do
    grep -F "@llvm.$intrinsic.$width" "$TMP/float_math.ll" >/dev/null || {
      echo "float math: missing llvm.$intrinsic.$width" >&2
      exit 1
    }
  done
done
if grep -E 'llvm\.fmuladd|(^|[ ,])(nnan|ninf|nsz|arcp|contract|afn|reassoc|fast)([ ,]|$)' \
  "$TMP/float_math.ll" >/dev/null; then
  echo 'float math: unexpected fused operation or fast-math flag' >&2
  exit 1
fi
"$LLVM_OPT" -S -passes='default<O0>' "$TMP/float_math.ll" -o "$TMP/float_math.O0.ll"
"$LLVM_OPT" -passes=verify "$TMP/float_math.O0.ll" -disable-output
"$LLVM_OPT" -S -passes='default<O2>' "$TMP/float_math.ll" -o "$TMP/float_math.O2.ll"
"$LLVM_OPT" -passes=verify "$TMP/float_math.O2.ll" -disable-output

for level in 0 2; do
  "$OCAML_FAS" -O"$level" "$ROOT/test/float_math.fas" -o "$TMP/float_math.O$level"
  timeout 60 "$TMP/float_math.O$level" >"$TMP/actual"
  diff -u "$TMP/expected" "$TMP/actual"
done

# Native backend: fma goes through the software runtime (runtime/fma.fas)
"$OCAML_FAS" --backend=native "$ROOT/test/float_math.fas" -o "$TMP/float_math.native"
timeout 60 "$TMP/float_math.native" >"$TMP/actual"
diff -u "$TMP/expected" "$TMP/actual"
FAS_FIR=roundtrip "$OCAML_FAS" --backend=native "$ROOT/test/float_math.fas" \
  -o "$TMP/float_math.fir"
timeout 60 "$TMP/float_math.fir" >"$TMP/actual"
diff -u "$TMP/expected" "$TMP/actual"

echo 'float math: sqrt, fma, floor, ceil, round, abs, constants and 2048 random patterns per width at O0/O2 and native: ok'
