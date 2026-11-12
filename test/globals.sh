#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
GLOBALS_TMP=$(mktemp -d)
trap 'rm -rf "$GLOBALS_TMP"' EXIT HUP INT TERM

fail() {
  echo "globals: $*" >&2
  exit 1
}

"$OCAML_FAS" --emit-llvm "$ROOT/test/globals.fas" >"$GLOBALS_TMP/globals.ll"
"$LLVM_OPT" -passes=verify "$GLOBALS_TMP/globals.ll" -disable-output
grep -Fx '@ZeroScalar = internal global [4 x i8] zeroinitializer, align 4' \
  "$GLOBALS_TMP/globals.ll" >/dev/null || fail "zero scalar global form changed"
grep -Fx '@ZeroArray = internal global [12 x i8] zeroinitializer, align 4' \
  "$GLOBALS_TMP/globals.ll" >/dev/null || fail "zero array global form changed"
grep -Fx '@ZeroPair = internal global [8 x i8] zeroinitializer, align 4' \
  "$GLOBALS_TMP/globals.ll" >/dev/null || fail "zero struct global form changed"
grep -Fx '@Exported = global [4 x i8] c"\03\00\00\00", align 4' \
  "$GLOBALS_TMP/globals.ll" >/dev/null || fail "exported global form changed"
grep -Fx '@Imported = external global [4 x i8], align 4' \
  "$GLOBALS_TMP/globals.ll" >/dev/null || fail "imported global form changed"
if grep '^@' "$GLOBALS_TMP/globals.ll" | grep -E 'dso_local|constant|unnamed_addr|noalias|align 8' >/dev/null; then
  fail "global emitted an unproved attribute or stronger alignment"
fi

ulimit -c 0 || true
for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$GLOBALS_TMP/globals.ll" \
    -o "$GLOBALS_TMP/globals-$level.ll"
  "$LLVM_OPT" -passes=verify "$GLOBALS_TMP/globals-$level.ll" -disable-output
  "$CC" -Werror -Wno-override-module -std=c17 -O"$level" -fPIE -pie \
    "$GLOBALS_TMP/globals-$level.ll" "$ROOT/test/globals.c" \
    -o "$GLOBALS_TMP/globals-$level"
  timeout 30 "$GLOBALS_TMP/globals-$level" || fail "-O$level C oracle failed"
  "$CC" -Werror -Wno-override-module -std=c17 -O"$level" -fPIC -shared \
    -Wl,-z,text "$GLOBALS_TMP/globals-$level.ll" "$ROOT/test/globals.c" \
    -o "$GLOBALS_TMP/libglobals-$level.so"
  if readelf -d "$GLOBALS_TMP/libglobals-$level.so" | grep TEXTREL >/dev/null; then
    fail "-O$level shared object contains TEXTREL"
  fi
done

echo "globals: ok"
