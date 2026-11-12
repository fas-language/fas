#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
ADDRESS_TMP=$(mktemp -d)
trap 'rm -rf "$ADDRESS_TMP"' EXIT HUP INT TERM
fail() { echo "address_constants: $*" >&2; exit 1; }
"$OCAML_FAS" --emit-llvm "$ROOT/test/address_constants.fas" >"$ADDRESS_TMP/program.ll"
"$LLVM_OPT" -passes=verify "$ADDRESS_TMP/program.ll" -disable-output
if grep -E 'inbounds|dso_local|nonnull|noalias|llvm.global_ctors' "$ADDRESS_TMP/program.ll"; then
  fail "unexpected facts or runtime initialization"
fi
grep -F 'getelementptr (i8, ptr @Target, i64 8)' "$ADDRESS_TMP/program.ll" >/dev/null || fail "missing element relocation"
for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$ADDRESS_TMP/program.ll" -o "$ADDRESS_TMP/optimized.ll"
  "$LLVM_OPT" -passes=verify "$ADDRESS_TMP/optimized.ll" -disable-output
  "$OCAML_FAS" -O"$level" -c "$ROOT/test/address_constants.fas" -o "$ADDRESS_TMP/program.o"
  clang -O"$level" "$ADDRESS_TMP/program.o" "$ROOT/test/address_constants_runtime.c" -o "$ADDRESS_TMP/fas"
  "$CC" -std=c17 -Werror -O"$level" -DADDRESS_ORACLE "$ROOT/test/address_constants_oracle.c" "$ROOT/test/address_constants_runtime.c" -o "$ADDRESS_TMP/oracle"
  timeout 30 "$ADDRESS_TMP/fas" >"$ADDRESS_TMP/fas.out"
  timeout 30 "$ADDRESS_TMP/oracle" >"$ADDRESS_TMP/oracle.out"
  cmp "$ADDRESS_TMP/fas.out" "$ADDRESS_TMP/oracle.out" || fail "O$level differs from C oracle"
  clang -shared "$ADDRESS_TMP/program.o" "$ROOT/test/address_constants_runtime.c" -fPIC -o "$ADDRESS_TMP/program.so"
  clang "$ROOT/test/address_constants_runtime.c" "$ADDRESS_TMP/program.so" -Wl,-rpath,"$ADDRESS_TMP" -o "$ADDRESS_TMP/shared"
  timeout 30 "$ADDRESS_TMP/shared" >"$ADDRESS_TMP/shared.out"
  cmp "$ADDRESS_TMP/shared.out" "$ADDRESS_TMP/oracle.out" || fail "O$level shared object differs"
  for artifact in fas program.so; do
    readelf -d "$ADDRESS_TMP/$artifact" >"$ADDRESS_TMP/dynamic"
    readelf -r "$ADDRESS_TMP/$artifact" >"$ADDRESS_TMP/relocations"
    if grep -q TEXTREL "$ADDRESS_TMP/dynamic"; then fail "O$level $artifact has TEXTREL"; fi
    grep -q R_X86_64_RELATIVE "$ADDRESS_TMP/relocations" || fail "missing relative relocations"
  done
done
echo "address_constants: C oracle, O0/O2, verified LLVM, plain clang, PIE/shared relocations: ok"
