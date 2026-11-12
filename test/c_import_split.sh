#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
SPLIT_TMP=$(mktemp -d)
trap 'rm -rf "$SPLIT_TMP"' EXIT HUP INT TERM

fail() {
  echo "c_import_split: $*" >&2
  exit 1
}

"$OCAML_FAS" --emit-llvm "$ROOT/test/c_import/split_main.fas" >"$SPLIT_TMP/split.ll"
"$LLVM_OPT" -passes=verify "$SPLIT_TMP/split.ll" -disable-output
ulimit -c 0 || true
for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$SPLIT_TMP/split.ll" \
    -o "$SPLIT_TMP/split.O$level.ll"
  "$LLVM_OPT" -passes=verify "$SPLIT_TMP/split.O$level.ll" -disable-output
  "$OCAML_FAS" -O"$level" -o "$SPLIT_TMP/split.O$level" \
    "$ROOT/test/c_import/split_main.fas"
  status=0
  timeout 30 "$SPLIT_TMP/split.O$level" || status=$?
  [ "$status" -eq 2 ] \
    || fail "two-file pthread program exited $status at -O$level, expected 2"
done

echo "c_import_split: O0/O2 two-file pthread create and join: ok"
