#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
case "$OCAML_FAS" in /*) ;; *) OCAML_FAS=$(pwd)/$OCAML_FAS ;; esac
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM
ulimit -c 0 || true

"$CC" -std=c11 -Wall -Wextra -Werror -I"$ROOT/test" \
  "$ROOT/test/c_block_include_oracle.c" -o "$TMP/oracle"
"$TMP/oracle" >"$TMP/expected"
cd "$TMP"
FAS_CC=$CC "$OCAML_FAS" check "$ROOT/test/c_block_include.fas"
FAS_CC=$CC "$OCAML_FAS" --emit-llvm "$ROOT/test/c_block_include.fas" >"$TMP/program.ll"
"$LLVM_OPT" -passes=verify "$TMP/program.ll" -disable-output
"$LLVM_OPT" -passes='default<O2>' -verify-each "$TMP/program.ll" -S \
  -o "$TMP/program-o2.ll"
"$LLVM_OPT" -passes=verify "$TMP/program-o2.ll" -disable-output
for level in 0 2; do
  FAS_CC=$CC "$OCAML_FAS" -O"$level" "$ROOT/test/c_block_include.fas" \
    -o "$TMP/program-$level"
  "$TMP/program-$level" >"$TMP/actual-$level"
  diff -u "$TMP/expected" "$TMP/actual-$level"
done
echo 'c_block_include: quoted include in a C block from another directory O0/O2: ok'
