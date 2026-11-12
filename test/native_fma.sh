#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
# Software fma of the native runtime vs libm: the runtime source compiled by
# both backends, linked to a C driver.
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
COUNT=${FMA_COUNT:-1000000}
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM

"$CC" -O2 -std=c17 -ffp-contract=off -c "$ROOT/test/native_fma/driver.c" -o "$TMP/driver.o"
for backend in llvm native; do
  "$OCAML_FAS" --backend=$backend -c "$ROOT/src/backend/native/runtime/fma.fas" \
    -o "$TMP/fma_$backend.o"
  "$CC" "$TMP/driver.o" "$TMP/fma_$backend.o" -lm -o "$TMP/check_$backend"
  "$TMP/check_$backend" "$COUNT" >"$TMP/out_$backend" || {
    cat "$TMP/out_$backend" >&2
    echo "native fma: $backend build disagrees with libm" >&2
    exit 1
  }
done
echo "native fma: software fma32/fma64 match libm on $COUNT cases (llvm and native builds): ok"
