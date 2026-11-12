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

"$CC" -std=c11 -Wall -Wextra -Werror -I"$ROOT/test" \
  "$ROOT/test/c_record_values.c" "$ROOT/test/c_record_values_oracle.c" \
  "$ROOT/test/c_record_values_driver.c" -o "$TMP/oracle"
"$TMP/oracle" >"$TMP/expected"
"$OCAML_FAS" --emit-llvm "$ROOT/test/c_record_values.fas" >"$TMP/program.ll"
"$LLVM_OPT" -passes=verify "$TMP/program.ll" -disable-output
"$LLVM_OPT" -passes='default<O2>' -verify-each "$TMP/program.ll" -S \
  -o "$TMP/program-o2.ll"
"$LLVM_OPT" -passes=verify "$TMP/program-o2.ll" -disable-output
if grep -E 'byval|sret' "$TMP/program.ll" >/dev/null; then
  echo 'c_record_values: Fas IR passes records by value' >&2
  exit 1
fi
for level in 0 2; do
  "$OCAML_FAS" -O"$level" -c "$ROOT/test/c_record_values.fas" \
    -o "$TMP/program-$level.o"
  "$CC" -std=c11 -Wall -Wextra -Werror -O"$level" -I"$ROOT/test" \
    "$ROOT/test/c_record_values.c" "$ROOT/test/c_record_values_driver.c" \
    "$TMP/program-$level.o" -o "$TMP/program-$level"
  "$TMP/program-$level" >"$TMP/actual-$level"
  diff -u "$TMP/expected" "$TMP/actual-$level"
done
echo 'c_record_values: C struct and union arguments and results O0/O2: ok'
