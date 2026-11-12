#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
SE_TMP=$(mktemp -d)
trap 'rm -rf "$SE_TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/shift_compound_effects.fas" >"$SE_TMP/effects.ll"
"$LLVM_OPT" -passes=verify "$SE_TMP/effects.ll" -disable-output

printf '%s\n' "index 1" "count 1" "note 1" "note 2" "note 3" >"$SE_TMP/expected.log"

ulimit -c 0 || true
for level in 0 2; do
  "$CC" -Werror -Wno-override-module -std=c17 -O"$level" "$SE_TMP/effects.ll" \
    "$ROOT/test/shift_compound_effects.c" -o "$SE_TMP/effects-$level"
  set +e
  timeout 30 "$SE_TMP/effects-$level" 2>"$SE_TMP/effects-$level.log"
  got=$?
  set -e
  if [ "$got" -ne 0 ]; then
    echo "shift compound effects: -O$level: want 0 got $got" >&2
    exit 1
  fi
  if ! cmp -s "$SE_TMP/expected.log" "$SE_TMP/effects-$level.log"; then
    echo "shift compound effects: -O$level: call order/count mismatch" >&2
    diff -u "$SE_TMP/expected.log" "$SE_TMP/effects-$level.log" >&2 || true
    exit 1
  fi
done

echo "shift compound effects: ok"
