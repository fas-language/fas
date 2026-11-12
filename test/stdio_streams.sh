#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
LLVM_OPT=${LLVM_OPT:-opt-22}
COMPONENT_TMP=$(mktemp -d)
trap 'rm -rf "$COMPONENT_TMP"' EXIT HUP INT TERM
"$OCAML_FAS" --emit-llvm "$ROOT/test/stdio_streams.fas" > "$COMPONENT_TMP/program.ll"
"$LLVM_OPT" -passes=verify "$COMPONENT_TMP/program.ll" -disable-output
for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$COMPONENT_TMP/program.ll" -o "$COMPONENT_TMP/optimized.ll"
  "$LLVM_OPT" -passes=verify "$COMPONENT_TMP/optimized.ll" -disable-output
  "$OCAML_FAS" -O"$level" "$ROOT/test/stdio_streams.fas" -o "$COMPONENT_TMP/program"
  "$COMPONENT_TMP/program" > "$COMPONENT_TMP/stdout" 2> "$COMPONENT_TMP/stderr"
  printf "output\n" > "$COMPONENT_TMP/expected-out"
  printf "error:37\n" > "$COMPONENT_TMP/expected-err"
  cmp "$COMPONENT_TMP/stdout" "$COMPONENT_TMP/expected-out"
  cmp "$COMPONENT_TMP/stderr" "$COMPONENT_TMP/expected-err"
done
echo 'stdio_streams: separate stdout and stderr at O0/O2: ok'
