#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM
ulimit -c 0 || true

for level in 0 2; do
  "$OCAML_FAS" --emit-llvm -O"$level" "$ROOT/test/c_system_header_keywords.fas" \
    >"$TMP/program.O$level.ll"
  "$LLVM_OPT" -passes=verify "$TMP/program.O$level.ll" -disable-output
  "$OCAML_FAS" -O"$level" "$ROOT/test/c_system_header_keywords.fas" \
    -o "$TMP/program.O$level"
  "$TMP/program.O$level"
done
echo "c_system_header_keywords: parsed and ran at O0/O2"
