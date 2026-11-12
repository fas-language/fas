#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
CC=${CC:-clang-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
DEFER_TMP=$(mktemp -d)
trap 'rm -rf "$DEFER_TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/defer_unwinding.fas" \
  >"$DEFER_TMP/defer-a.ll"
"$OCAML_FAS" --emit-llvm "$ROOT/test/defer_unwinding.fas" \
  >"$DEFER_TMP/defer-b.ll"
cmp "$DEFER_TMP/defer-a.ll" "$DEFER_TMP/defer-b.ll"
"$LLVM_OPT" -passes=verify "$DEFER_TMP/defer-a.ll" -disable-output

sed -n '/^define internal void @fallthrough(/,/^}/p' \
  "$DEFER_TMP/defer-a.ll" >"$DEFER_TMP/fallthrough.ll"
if [ "$(grep -c 'alloca ' "$DEFER_TMP/fallthrough.ll")" -ne 1 ] || \
  [ "$(grep -c 'call void @append' "$DEFER_TMP/fallthrough.ll")" -ne 2 ]; then
  echo "defer unwinding: cleanup did not lower directly" >&2
  exit 1
fi

if grep -Eiq 'defer|cleanup.stack|cleanup_stack|malloc|calloc|realloc' \
  "$DEFER_TMP/defer-a.ll"; then
  echo "defer unwinding: unexpected runtime cleanup machinery" >&2
  exit 1
fi

for level in 0 2; do
  FAS_OPT="$LLVM_OPT" FAS_LLC="$LLVM_LLC" FAS_CC="$CC" \
    "$OCAML_FAS" -O"$level" "$ROOT/test/defer_unwinding.fas" \
    -o "$DEFER_TMP/defer-$level"
  "$DEFER_TMP/defer-$level"
done

echo "defer unwinding: ok"
