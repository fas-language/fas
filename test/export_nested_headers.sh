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
ulimit -c 0 || true

for level in 0 2; do
  "$OCAML_FAS" -I "$ROOT/test" --emit-llvm -O"$level" \
    "$ROOT/test/export_nested_headers.fas" \
    >"$TMP/program.O$level.ll"
  "$LLVM_OPT" -passes=verify "$TMP/program.O$level.ll" -disable-output
  "$OCAML_FAS" --emit-header -I "$ROOT/test" -o "$TMP/export_nested_headers_api.h" \
    "$ROOT/test/export_nested_headers.fas"
  "$CC" -std=c11 -Wall -Wextra -Werror -pedantic -I"$TMP" -I"$ROOT/test" \
    -fsyntax-only "$ROOT/test/export_nested_headers_oracle.c"
  "$OCAML_FAS" -I "$ROOT/test" -c -O"$level" \
    "$ROOT/test/export_nested_headers.fas" \
    -o "$TMP/program.o"
  "$CC" -std=c11 -Wall -Wextra -Werror -pedantic -I"$ROOT/test" \
    "$TMP/program.o" "$ROOT/test/export_nested_headers_runtime.c" -o "$TMP/runtime"
  "$TMP/runtime"
done
