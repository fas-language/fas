#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM
fail() { echo "bool_memory: $*" >&2; exit 1; }

"$OCAML_FAS" --emit-llvm "$ROOT/test/bool_memory.fas" >"$TMP/program.ll"
"$LLVM_OPT" -passes=verify "$TMP/program.ll" -disable-output
for forbidden in 'alloca i1' 'load i1, ptr' 'load volatile i1, ptr' 'store i1 ' 'store volatile i1 '; do
  if grep -Fq "$forbidden" "$TMP/program.ll"; then
    fail "pre-optimization LLVM contains forbidden bool memory form: $forbidden"
  fi
done
if grep -Eq '@[^=]+ = .* (global|constant) .*i1' "$TMP/program.ll"; then
  fail "pre-optimization LLVM contains an i1 global"
fi
for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$TMP/program.ll" \
    -o "$TMP/program.O$level.ll"
  "$LLVM_OPT" -passes=verify "$TMP/program.O$level.ll" -disable-output
  "$OCAML_FAS" -O"$level" -o "$TMP/program.O$level" \
    "$ROOT/test/bool_memory.fas" "$ROOT/test/bool_memory_oracle.c"
  "$TMP/program.O$level" >"$TMP/observed"
  printf 'expected=8 observed=8\n' >"$TMP/expected"
  cmp -s "$TMP/expected" "$TMP/observed" || fail "bool memory paths differed at O$level"
done
"$OCAML_FAS" --emit-llvm "$ROOT/test/tokenizer_loop.fas" >"$TMP/tokenizer.ll"
"$LLVM_OPT" -passes=verify "$TMP/tokenizer.ll" -disable-output
"$LLVM_OPT" -S -passes='default<O2>' "$TMP/tokenizer.ll" -o "$TMP/tokenizer.O2.ll"
"$LLVM_OPT" -passes=verify "$TMP/tokenizer.O2.ll" -disable-output
if grep -Fq 'alloca ' "$TMP/tokenizer.O2.ll"; then
  fail "tokenizer bool loop retained an alloca after O2"
fi
echo "bool_memory: byte stores and reads across locals, parameters, fields, arrays, globals, constants, copies, raw, view, volatile and C imports at O0/O2; tokenizer has no O2 allocas: ok"
