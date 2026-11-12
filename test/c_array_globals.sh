#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
ARRAY_TMP=$(mktemp -d)
trap 'rm -rf "$ARRAY_TMP"' EXIT HUP INT TERM
fail() { echo "c_array_globals: $*" >&2; exit 1; }
"$OCAML_FAS" --emit-llvm "$ROOT/test/c_import/arrays.fas" >"$ARRAY_TMP/program.ll"
"$LLVM_OPT" -passes=verify "$ARRAY_TMP/program.ll" -disable-output
for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$ARRAY_TMP/program.ll" -o "$ARRAY_TMP/optimized.ll"
  "$LLVM_OPT" -passes=verify "$ARRAY_TMP/optimized.ll" -disable-output
  "$OCAML_FAS" -O"$level" -o "$ARRAY_TMP/fas" "$ROOT/test/c_import/arrays.fas" "$ROOT/test/c_import/arrays_runtime.c"
  "$CC" -Werror -std=c17 -O"$level" "$ROOT/test/c_import/arrays_oracle.c" "$ROOT/test/c_import/arrays_runtime.c" -o "$ARRAY_TMP/oracle"
  timeout 30 "$ARRAY_TMP/fas" || fail "Fas O$level array behavior differs"
  timeout 30 "$ARRAY_TMP/oracle" || fail "C O$level oracle failed"
done
TMPDIR="$ARRAY_TMP" "$OCAML_FAS" --keep --emit-llvm "$ROOT/test/c_import/arrays.fas" >"$ARRAY_TMP/kept.ll" 2>"$ARRAY_TMP/kept.err"
find "$ARRAY_TMP" -name '*bindings*.txt' -exec cat {} \; >"$ARRAY_TMP/manifest"
grep -F 'arr[4, arr[8, i8]]' "$ARRAY_TMP/manifest" >/dev/null || fail "manifest omitted header array type"
grep -F 'arr[2, arr[3, i32]]' "$ARRAY_TMP/manifest" >/dev/null || fail "manifest omitted container array type"
echo "c_array_globals: header/container arrays, C oracle and manifest at O0/O2: ok"
