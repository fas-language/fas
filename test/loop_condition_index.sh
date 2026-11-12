#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
LOOP_TMP=$(mktemp -d)
trap 'rm -rf "$LOOP_TMP"' EXIT HUP INT TERM

"$CC" -Werror -std=c17 -O2 "$ROOT/test/loop_condition_index.c" -o "$LOOP_TMP/oracle"
timeout 30 "$LOOP_TMP/oracle" >"$LOOP_TMP/expected.txt"

"$OCAML_FAS" --emit-llvm "$ROOT/test/loop_condition_index.fas" >"$LOOP_TMP/loop.ll"
"$LLVM_OPT" -passes=verify "$LOOP_TMP/loop.ll" -disable-output

ulimit -c 0 || true
for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$LOOP_TMP/loop.ll" \
    -o "$LOOP_TMP/loop-$level.ll"
  "$LLVM_OPT" -passes=verify "$LOOP_TMP/loop-$level.ll" -disable-output
  "$CC" -Werror -Wno-override-module -std=c17 -O"$level" \
    "$LOOP_TMP/loop-$level.ll" -o "$LOOP_TMP/loop-$level"
  set +e
  timeout 30 "$LOOP_TMP/loop-$level" >"$LOOP_TMP/actual-$level.txt"
  got=$?
  set -e
  if [ "$got" -ne 0 ]; then
    echo "loop_condition_index: -O$level: execution failed (exit $got)" >&2
    exit 1
  fi
  if ! cmp -s "$LOOP_TMP/expected.txt" "$LOOP_TMP/actual-$level.txt"; then
    echo "loop_condition_index: -O$level: output differs from the C oracle" >&2
    diff -u "$LOOP_TMP/expected.txt" "$LOOP_TMP/actual-$level.txt" >&2 || true
    exit 1
  fi
done

echo "loop_condition_index: ok"
