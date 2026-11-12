#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ulimit -Sv 4194304
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
LLVM_AS=${LLVM_AS:-llvm-as-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/float_constants.fas" >"$TMP/float_constants.ll"
"$LLVM_OPT" -passes=verify "$TMP/float_constants.ll" -disable-output
"$LLVM_AS" "$TMP/float_constants.ll" -o "$TMP/float_constants.bc"
# Fir writes data as bytes: the tables must hold the exact IEEE images
# (signed zero, subnormals, infinities, quiet NaNs), little-endian.
for image in \
  '@FloatValues = private constant [28 x i8] c"\00\00\00\80\01\00\00\00\00\00\80?\FF\FF\7F\00\00\00\80\00\00\00\80\7F\00\00\C0\7F"' \
  '@ConstFloat64Values = private constant [32 x i8] c"\00\00\00\00\00\00\00\80\01\00\00\00\00\00\00\00\00\00\00\00\00\00\F0\7F\00\00\00\00\00\00\F8\7F"'; do
  if ! grep -F "$image" "$TMP/float_constants.ll" >/dev/null; then
    echo "float constants: missing exact data image $image" >&2
    exit 1
  fi
done
for constant in 'float 0x7FF0000000000000' 'double 0x7FF0000000000000'; do
  if ! grep -F "$constant" "$TMP/float_constants.ll" >/dev/null; then
    echo "float constants: missing exact LLVM constant $constant" >&2
    exit 1
  fi
done
"$CC" -Werror -std=c17 -O0 -ffp-contract=off -fno-fast-math \
  -c "$ROOT/test/float_constants.c" -o "$TMP/oracle.o"

for level in 0 2; do
  if [ "$level" -eq 0 ]; then
    cp "$TMP/float_constants.ll" "$TMP/float_constants.O$level.ll"
  else
    "$LLVM_OPT" -S "-passes=default<O$level>" "$TMP/float_constants.ll" \
      -o "$TMP/float_constants.O$level.ll"
  fi
  current="$TMP/float_constants.O$level.ll"
  "$LLVM_OPT" -passes=verify "$current" -disable-output
  "$LLVM_LLC" -O"$level" -relocation-model=pic -filetype=obj "$current" \
    -o "$TMP/float_constants.O$level.o"
  "$CC" -Werror -std=c17 -O0 -ffp-contract=off -fno-fast-math \
    "$TMP/float_constants.O$level.o" "$TMP/oracle.o" -o "$TMP/float_constants.O$level"
  timeout 60 "$TMP/float_constants.O$level"
done

echo 'float constants: binary32 rounding, IEEE edge values, comparisons, conversions, aggregates and O0/O2 parity: ok'
