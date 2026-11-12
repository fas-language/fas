#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
CC=${CC:-clang-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
OPAQUE_TMP=$(mktemp -d)
trap 'rm -rf "$OPAQUE_TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/opaque_types.fas" >"$OPAQUE_TMP/opaque.ll"
"$LLVM_OPT" -passes=verify "$OPAQUE_TMP/opaque.ll" -disable-output

for signature in \
  'define internal ptr @preserve(ptr' \
  'define internal ptr @make_addr(ptr' \
  'define internal ptr @recover(ptr' \
  'define internal i64 @pointer_layout()'; do
  if ! grep -Fq "$signature" "$OPAQUE_TMP/opaque.ll"; then
    echo "opaque types: missing lowered signature: $signature" >&2
    exit 1
  fi
done

for level in 0 2; do
  FAS_OPT="$LLVM_OPT" FAS_LLC="$LLVM_LLC" FAS_CC="$CC" \
    "$OCAML_FAS" -O"$level" "$ROOT/test/opaque_types.fas" \
    -o "$OPAQUE_TMP/opaque-$level"
  "$OPAQUE_TMP/opaque-$level"
done

echo "opaque types: ok"
