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

"$OCAML_FAS" --emit-llvm "$ROOT/test/doom_newlines.fas" >"$TMP/program.ll"
"$LLVM_OPT" -passes=verify "$TMP/program.ll" -disable-output
"$CC" -Werror -std=c17 "$ROOT/test/doom_newlines_oracle.c" -o "$TMP/oracle"
"$TMP/oracle" >"$TMP/oracle.out"
for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$TMP/program.ll" -o "$TMP/program.O$level.ll"
  "$LLVM_OPT" -passes=verify "$TMP/program.O$level.ll" -disable-output
  "$OCAML_FAS" -O"$level" -o "$TMP/fas.O$level" "$ROOT/test/doom_newlines.fas"
  timeout 30 "$TMP/fas.O$level" >"$TMP/fas.O$level.out"
  cmp -s "$TMP/oracle.out" "$TMP/fas.O$level.out" || {
    echo "doom newlines: Fas and C output differ at O$level" >&2
    exit 1
  }
done

echo "doom newlines: aggregate, parameter, argument, type, selector and generic lists match C at O0/O2"
