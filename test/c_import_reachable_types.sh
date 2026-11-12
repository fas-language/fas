#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
ulimit -v 4194304
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM

for fixture in c_import_array_reachable c_import_nested_reachable; do
  source="$ROOT/test/$fixture.fas"
  "$OCAML_FAS" --emit-llvm "$source" >"$TMP/$fixture.ll"
  "$LLVM_OPT" -passes=verify "$TMP/$fixture.ll" -disable-output
  for level in 0 2; do
    "$OCAML_FAS" -O"$level" "$source" -o "$TMP/$fixture.O$level"
    timeout 30 "$TMP/$fixture.O$level"
  done
done

echo "c_import_reachable_types: declaration and nested record layouts match C at O0/O2"
