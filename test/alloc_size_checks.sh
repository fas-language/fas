#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu
export DEBUGINFOD_URLS=
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM
fail() { echo "alloc_size_checks: $*" >&2; exit 1; }

"$OCAML_FAS" --emit-llvm "$ROOT/test/alloc_size_checks.fas" >"$TMP/program.ll"
"$LLVM_OPT" -passes=verify "$TMP/program.ll" -disable-output
for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$TMP/program.ll" \
    -o "$TMP/program.O$level.ll"
  "$LLVM_OPT" -passes=verify "$TMP/program.O$level.ll" -disable-output
  "$OCAML_FAS" -O"$level" -o "$TMP/fas.O$level" \
    "$ROOT/test/alloc_size_checks.fas"
  "$CC" -Werror -std=c17 -O"$level" \
    "$ROOT/test/alloc_size_checks_oracle.c" -o "$TMP/oracle.O$level"
  timeout 30 "$TMP/fas.O$level" || fail "Fas O$level behavior failed"
  timeout 30 "$TMP/oracle.O$level" || fail "C O$level oracle failed"
  "$OCAML_FAS" --sanitize=address -O"$level" -o "$TMP/fas.asan.O$level" \
    "$ROOT/test/alloc_size_checks.fas"
  (ulimit -Sv unlimited; ASAN_OPTIONS=detect_stack_use_after_return=1:detect_leaks=0 \
    timeout 30 "$TMP/fas.asan.O$level") \
    || fail "Fas address-sanitized O$level behavior failed"
done
echo "alloc_size_checks: malloc, calloc and realloc match C at O0/O2; LLVM and ASan: ok"
