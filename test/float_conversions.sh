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

"$OCAML_FAS" --emit-llvm "$ROOT/test/float_conversions.fas" >"$TMP/conversions.ll"
"$LLVM_OPT" -passes=verify "$TMP/conversions.ll" -disable-output
for operation in sitofp uitofp fpext fptrunc llvm.fptosi.sat llvm.fptoui.sat; do
  if ! grep -F "$operation" "$TMP/conversions.ll" >/dev/null; then
    echo "float conversions: missing $operation" >&2
    exit 1
  fi
done
if ! grep -F 'sitofp i64' "$TMP/conversions.ll" | grep -F 'to float' >/dev/null; then
  echo 'float conversions: i64 to f32 did not lower directly' >&2
  exit 1
fi
if ! grep -F 'uitofp i64' "$TMP/conversions.ll" | grep -F 'to float' >/dev/null; then
  echo 'float conversions: u64 to f32 did not lower directly' >&2
  exit 1
fi
"$CC" -Werror -std=c17 -O0 -ffp-contract=off -fno-fast-math \
  -c "$ROOT/test/float_conversions.c" -o "$TMP/oracle.o"

for level in 0 2; do
  if [ "$level" -eq 0 ]; then
    cp "$TMP/conversions.ll" "$TMP/conversions.O$level.ll"
  else
    "$LLVM_OPT" -S "-passes=default<O$level>" "$TMP/conversions.ll" \
      -o "$TMP/conversions.O$level.ll"
  fi
  "$LLVM_OPT" -passes=verify "$TMP/conversions.O$level.ll" -disable-output
  "$LLVM_LLC" -O"$level" -relocation-model=pic -filetype=obj \
    "$TMP/conversions.O$level.ll" -o "$TMP/conversions.O$level.o"
  "$CC" -Werror -std=c17 -O0 -ffp-contract=off -fno-fast-math \
    "$TMP/conversions.O$level.o" "$TMP/oracle.o" -o "$TMP/conversions.O$level"
  timeout 60 "$TMP/conversions.O$level"
done

echo 'float conversions: every integer width, direct rounding, float conversion, saturation and bitcasts at O0/O2: ok'
