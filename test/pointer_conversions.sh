#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
CC=${CC:-clang-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
POINTER_TMP=$(mktemp -d)
trap 'rm -rf "$POINTER_TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/pointer_conversions.fas" \
  >"$POINTER_TMP/pointers.ll"
"$LLVM_OPT" -passes=verify "$POINTER_TMP/pointers.ll" -disable-output

for instruction in 'ptrtoint ptr' 'inttoptr i64'; do
  if ! grep -Fq "$instruction" "$POINTER_TMP/pointers.ll"; then
    echo "pointer conversions: missing $instruction" >&2
    exit 1
  fi
done

if grep -Eiq 'noalias|nonnull|dereferenceable|inbounds|ptr align [0-9]' \
  "$POINTER_TMP/pointers.ll"; then
  echo "pointer conversions: unexpected pointer guarantee in LLVM" >&2
  exit 1
fi

for level in 0 2; do
  FAS_OPT="$LLVM_OPT" FAS_LLC="$LLVM_LLC" FAS_CC="$CC" \
    "$OCAML_FAS" -O"$level" "$ROOT/test/pointer_conversions.fas" \
    -o "$POINTER_TMP/pointers-$level"
  "$POINTER_TMP/pointers-$level"
done

echo "pointer conversions: ok"
