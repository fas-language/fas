#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
COMPONENT_TMP=$(mktemp -d)
trap 'rm -rf "$COMPONENT_TMP"' EXIT HUP INT TERM
ulimit -c 0 || true

mkdir "$COMPONENT_TMP/include"
"$OCAML_FAS" --emit-header -o "$COMPONENT_TMP/include/add.h" \
  "$ROOT/test/export_header_guard.fas"
if grep -Fx '#ifndef FAS_ADD_H' "$COMPONENT_TMP/include/add.h"; then
  echo 'export_header_guard: include guard shadows exported identifier' >&2
  exit 1
fi
"$CC" -std=c11 -Wall -Wextra -Werror -pedantic \
  "$ROOT/test/export_header_guard_oracle.c" -o "$COMPONENT_TMP/oracle"
"$COMPONENT_TMP/oracle" >"$COMPONENT_TMP/expected"

for level in 0 2; do
  "$OCAML_FAS" --emit-llvm -O"$level" "$ROOT/test/export_header_guard.fas" \
    >"$COMPONENT_TMP/program-O$level.ll"
  "$LLVM_OPT" -passes=verify "$COMPONENT_TMP/program-O$level.ll" -disable-output
  "$LLVM_OPT" -S "-passes=default<O$level>" "$COMPONENT_TMP/program-O$level.ll" \
    -o "$COMPONENT_TMP/program-O$level.opt.ll"
  "$LLVM_OPT" -passes=verify "$COMPONENT_TMP/program-O$level.opt.ll" -disable-output
  "$OCAML_FAS" -O"$level" -c "$ROOT/test/export_header_guard.fas" \
    -o "$COMPONENT_TMP/program-O$level.o"
  "$CC" -std=c11 -Wall -Wextra -Werror -pedantic -O"$level" \
    -I"$COMPONENT_TMP/include" "$ROOT/test/export_header_guard_consumer.c" \
    "$COMPONENT_TMP/program-O$level.o" -o "$COMPONENT_TMP/consumer-O$level"
  "$COMPONENT_TMP/consumer-O$level" >"$COMPONENT_TMP/actual"
  diff -u "$COMPONENT_TMP/expected" "$COMPONENT_TMP/actual"
done

echo 'export_header_guard: add.h export collision, O0/O2 C oracle and LLVM verification passed'
