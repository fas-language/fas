#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
FIXTURES="$ROOT/test/generated_header_cases"
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM
fail() { echo "generated_headers: $*" >&2; exit 1; }

for level in 0 2; do
  for scenario in vector_collision header_order required_headers; do
    source="$FIXTURES/$scenario.fas"
    oracle="$FIXTURES/${scenario}_oracle.c"
    if [ "$scenario" = vector_collision ]; then
      "$CC" -std=c11 -Wall -Wextra -Werror -pedantic -O"$level" \
        "$oracle" -o "$TMP/oracle-$scenario-$level"
    else
      "$CC" -std=c17 -Wall -Wextra -Werror -I"$FIXTURES" -O"$level" \
        "$oracle" -o "$TMP/oracle-$scenario-$level"
    fi
    "$TMP/oracle-$scenario-$level" || fail "C oracle $scenario failed at O$level"
    "$OCAML_FAS" --emit-llvm "$source" >"$TMP/$scenario.ll"
    "$LLVM_OPT" -passes=verify "$TMP/$scenario.ll" -disable-output
    "$LLVM_OPT" -S "-passes=default<O$level>" "$TMP/$scenario.ll" \
      -o "$TMP/$scenario.O$level.ll"
    "$LLVM_OPT" -passes=verify "$TMP/$scenario.O$level.ll" -disable-output
    "$OCAML_FAS" -O"$level" "$source" -o "$TMP/program-$scenario-$level"
    "$TMP/program-$scenario-$level" || fail "Fas $scenario failed at O$level"
  done
done
"$OCAML_FAS" --emit-header "$FIXTURES/required_headers.fas" >"$TMP/required_headers.h"
base_line=$(grep -n 'z_base.h' "$TMP/required_headers.h" | cut -d: -f1)
derived_line=$(grep -n 'a_derived.h' "$TMP/required_headers.h" | cut -d: -f1)
[ "$base_line" -lt "$derived_line" ] || fail "required headers were not emitted in source order"
echo "generated_headers: vector names, container macro order and required header order at O0/O2: ok"
