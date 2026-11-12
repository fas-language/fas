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
  "$ROOT/test/c_float_boundary.c" "$ROOT/test/c_float_boundary_oracle.c" \
  -o "$TMP/oracle"
"$TMP/oracle" >"$TMP/expected"
"$OCAML_FAS" --emit-header -o "$TMP/api.h" "$ROOT/test/c_float_boundary.fas"
grep -F 'float narrow;' "$TMP/api.h" >/dev/null
grep -Fx '  double wide;' "$TMP/api.h" >/dev/null
grep -Fx 'float fas_scale_f32(float value);' "$TMP/api.h" >/dev/null
grep -Fx 'double fas_scale_f64(double value);' "$TMP/api.h" >/dev/null
printf '#include "api.h"\n#include "c_float_boundary.h"\n' >"$TMP/header.c"
"$CC" -std=c11 -Wall -Wextra -Werror -I"$TMP" -I"$ROOT/test" \
  -fsyntax-only "$TMP/header.c"
"$OCAML_FAS" --emit-llvm "$ROOT/test/c_float_boundary.fas" >"$TMP/program.ll"
"$LLVM_OPT" -passes=verify "$TMP/program.ll" -disable-output
"$LLVM_OPT" -passes='default<O2>' -verify-each "$TMP/program.ll" -S \
  -o "$TMP/program-o2.ll"
"$LLVM_OPT" -passes=verify "$TMP/program-o2.ll" -disable-output
for level in 0 2; do
  "$OCAML_FAS" -O"$level" -c "$ROOT/test/c_float_boundary.fas" \
    -o "$TMP/program-$level.o"
  "$CC" -std=c11 -Wall -Wextra -Werror -O"$level" -I"$TMP" \
    -I"$ROOT/test" "$ROOT/test/c_float_boundary.c" \
    "$ROOT/test/c_float_boundary_driver.c" "$TMP/program-$level.o" \
    -o "$TMP/program-$level"
  "$TMP/program-$level" >"$TMP/actual-$level"
  diff -u "$TMP/expected" "$TMP/actual-$level"
done
echo 'c_float_boundary: import/export calls, fields, varargs, and stack spills O0/O2: ok'
