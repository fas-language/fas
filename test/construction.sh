#!/bin/sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
CONSTRUCTION_TMP=$(mktemp -d)
trap 'rm -rf "$CONSTRUCTION_TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/construction.fas" >"$CONSTRUCTION_TMP/construction.ll"
"$LLVM_OPT" -passes=verify "$CONSTRUCTION_TMP/construction.ll" -disable-output

pair_ir=$(awk '/^define i32 @fas_construct_explicit\(/,/^}/' "$CONSTRUCTION_TMP/construction.ll")
pair_allocas=$(printf '%s\n' "$pair_ir" | grep -Ec 'alloca (%struct.Pair|\[8 x i8\])' || true)
if [ "$pair_allocas" -ne 1 ] || printf '%s\n' "$pair_ir" | grep -Eq 'load (%struct.Pair|\[)'; then
  echo "construction: expected stores directly into one destination" >&2
  exit 1
fi

ulimit -c 0 || true
for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$CONSTRUCTION_TMP/construction.ll" \
    -o "$CONSTRUCTION_TMP/construction-$level.ll"
  "$LLVM_OPT" -passes=verify "$CONSTRUCTION_TMP/construction-$level.ll" -disable-output
  "$CC" -Werror -Wno-override-module -std=c17 -O"$level" \
    "$CONSTRUCTION_TMP/construction-$level.ll" "$ROOT/test/construction.c" \
    -o "$CONSTRUCTION_TMP/construction-$level"
  set +e
  timeout 30 "$CONSTRUCTION_TMP/construction-$level"
  got=$?
  set -e
  if [ "$got" -ne 0 ]; then
    echo "construction: -O$level: behavior failed (exit $got)" >&2
    exit 1
  fi
done

echo "construction: ok"
