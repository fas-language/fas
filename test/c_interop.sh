#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM

fail() {
  echo "c_interop: $*" >&2
  exit 1
}

"$CC" -Werror -std=c17 -D_POSIX_C_SOURCE=200809L \
  "$ROOT/test/c_interop_oracle.c" -o "$TMP/oracle"

"$OCAML_FAS" --emit-llvm -D_POSIX_C_SOURCE=200809L \
  "$ROOT/test/c_interop.fas" >"$TMP/program.ll"
"$LLVM_OPT" -passes=verify "$TMP/program.ll" -disable-output
grep -Eq 'ptr @__fas_c_adapter_[[:xdigit:]]+_fas_interop_static_helper' "$TMP/program.ll" \
  || fail "static helper address did not relocate to its adapter"
grep -Eq 'ptr @__fas_c_adapter_[[:xdigit:]]+_fas_interop_address_only' "$TMP/program.ll" \
  || fail "address-only static function did not get an adapter relocation"
grep -Eq 'call ptr @__fas_c_adapter_[[:xdigit:]]+_fas_interop_raw_get' "$TMP/program.ll" \
  || fail "raw function-pointer result did not use its adapter"
grep -Eq 'call i32 @__fas_c_adapter_[[:xdigit:]]+_fas_interop_raw_apply' "$TMP/program.ll" \
  || fail "raw function-pointer parameter did not use its adapter"
if grep -q 'dso_local' "$TMP/program.ll"; then
  fail "static function address emitted dso_local"
fi

for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$TMP/program.ll" \
    -o "$TMP/program.O$level.ll"
  "$LLVM_OPT" -passes=verify "$TMP/program.O$level.ll" -disable-output
  "$OCAML_FAS" -O"$level" -D_POSIX_C_SOURCE=200809L \
    "$ROOT/test/c_interop.fas" -o "$TMP/fas.O$level"
  (cd "$TMP" && timeout 30 "$TMP/fas.O$level") >"$TMP/fas.O$level.out" \
    || fail "Fas O$level execution failed"
  (cd "$TMP" && timeout 30 "$TMP/oracle") >"$TMP/oracle.out" \
    || fail "C O$level oracle failed"
  cmp -s "$TMP/fas.O$level.out" "$TMP/oracle.out" \
    || fail "Fas and C oracle outputs differ at O$level"
done

if printf '#include <zlib.h>\nint main(void) { return 0; }\n' \
  | "$CC" -Werror -std=c17 -x c - -lz -o "$TMP/zlib-probe" >/dev/null 2>&1; then
  "$CC" -Werror -std=c17 "$ROOT/test/c_interop_zlib_oracle.c" -lz \
    -o "$TMP/zlib-oracle"
  "$OCAML_FAS" --emit-llvm "$ROOT/test/c_interop_zlib.fas" >"$TMP/zlib.ll"
  "$LLVM_OPT" -passes=verify "$TMP/zlib.ll" -disable-output
  for level in 0 2; do
    "$LLVM_OPT" -S "-passes=default<O$level>" "$TMP/zlib.ll" \
      -o "$TMP/zlib.O$level.ll"
    "$LLVM_OPT" -passes=verify "$TMP/zlib.O$level.ll" -disable-output
    "$OCAML_FAS" -O"$level" "$ROOT/test/c_interop_zlib.fas" -lz \
      -o "$TMP/zlib-fas.O$level"
    timeout 30 "$TMP/zlib-fas.O$level" >"$TMP/zlib-fas.O$level.out" \
      || fail "Fas zlib O$level round trip failed"
    timeout 30 "$TMP/zlib-oracle" >"$TMP/zlib-oracle.out" \
      || fail "C zlib O$level oracle failed"
    cmp -s "$TMP/zlib-fas.O$level.out" "$TMP/zlib-oracle.out" \
      || fail "Fas and C zlib outputs differ at O$level"
  done
else
  echo "c_interop: zlib.h/libz unavailable; zlib round trip skipped"
fi

echo "c_interop: O0/O2 verified, system C interop matches C oracle: ok"
