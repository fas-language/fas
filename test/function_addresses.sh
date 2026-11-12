#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=$(printenv CC 2>/dev/null || printf '%s' clang-22)
LLVM_OPT=$(printenv LLVM_OPT 2>/dev/null || printf '%s' opt-22)
LLVM_LLC=$(printenv LLVM_LLC 2>/dev/null || printf '%s' llc-22)
OCAML_FAS=$(printenv OCAML_FAS 2>/dev/null || printf '%s' "$ROOT/_build/default/bin/main.exe")
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM

fail() {
  echo "function_addresses: $*" >&2
  exit 1
}

"$CC" -Werror -std=c17 "$ROOT/test/function_addresses_oracle.c" -o "$TMP/oracle"
"$OCAML_FAS" --emit-llvm "$ROOT/test/function_addresses.fas" >"$TMP/program.ll"
"$LLVM_OPT" -passes=verify "$TMP/program.ll" -disable-output
grep -Eq 'define internal signext i8 @fas_callback_i8\(i8 signext ' "$TMP/program.ll" || fail "addressed native i8 callback lacks C ABI extensions"
grep -Eq 'define internal zeroext i1 @echo_bool\(i1 zeroext ' "$TMP/program.ll" || fail "addressed native bool function lacks C ABI extensions"
grep -Eq 'define internal signext i8 @echo_i8\(i8 signext ' "$TMP/program.ll" || fail "addressed native i8 function lacks C ABI extensions"
grep -Eq 'define internal i8 @untouched_i8\(i8 %a0\)' "$TMP/program.ll" || fail "unaddressed native i8 function changed ABI"
grep -Eq 'define internal i1 @untouched_bool\(i1 %a0\)' "$TMP/program.ll" || fail "unaddressed native bool function changed ABI"
grep -Eq 'call signext i8 @echo_i8\(i8 signext ' "$TMP/program.ll" || fail "direct call to addressed native i8 function lacks C ABI extensions"
grep -Eq 'call zeroext i1 @echo_bool\(i1 zeroext ' "$TMP/program.ll" || fail "direct call to addressed native bool function lacks C ABI extensions"
grep -Eq 'call signext i8 %v[0-9]+\(i8 signext ' "$TMP/program.ll" || fail "indirect call lacks narrow C ABI argument extensions"
grep -Eq 'call zeroext i1 %v[0-9]+\(i1 zeroext ' "$TMP/program.ll" || fail "indirect call lacks bool C ABI argument extensions"
grep -F 'ptr @fas_callback_i8' "$TMP/program.ll" >/dev/null || fail "native narrow callback address was not passed to C"
grep -F 'ptr @States' "$TMP/program.ll" >/dev/null || fail "state table function addresses were omitted"
grep -F '__fas_c_adapter_' "$TMP/program.ll" >/dev/null || fail "static inline C adapter was not emitted"
"$LLVM_OPT" -S '-passes=default<O2>' -verify-each "$TMP/program.ll" -o "$TMP/program.O2.ll"
"$LLVM_OPT" -passes=verify "$TMP/program.O2.ll" -disable-output
"$TMP/oracle" >"$TMP/oracle.out"

for level in 0 2; do
  FAS_OPT="$LLVM_OPT" FAS_LLC="$LLVM_LLC" FAS_CC="$CC" \
    "$OCAML_FAS" -O"$level" -c "$ROOT/test/function_addresses.fas" -o "$TMP/fas.O$level.o"
  "$CC" -Werror -std=c17 -O"$level" "$TMP/fas.O$level.o" -o "$TMP/fas.O$level"
  timeout 30 "$TMP/fas.O$level" >"$TMP/fas.O$level.out" || fail "Fas O$level execution failed"
  cmp -s "$TMP/fas.O$level.out" "$TMP/oracle.out" || fail "Fas and C oracle outputs differ at O$level"
done

echo "function_addresses: function dispatch, scalar and vector ABI, C callbacks, O0/O2: ok"
