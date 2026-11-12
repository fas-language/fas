#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang}
LLVM_OPT=${LLVM_OPT:-opt}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
ABI_TMP=$(mktemp -d)
trap 'rm -rf "$ABI_TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/abi_internal.fas" >"$ABI_TMP/internal.ll"
"$OCAML_FAS" --emit-llvm "$ROOT/test/abi_boundary.fas" >"$ABI_TMP/boundary.ll"
"$OCAML_FAS" --emit-llvm "$ROOT/test/abi_exports.fas" >"$ABI_TMP/exports.ll"
"$LLVM_OPT" -passes=verify "$ABI_TMP/internal.ll" -disable-output
"$LLVM_OPT" -passes=verify "$ABI_TMP/boundary.ll" -disable-output
"$LLVM_OPT" -passes=verify "$ABI_TMP/exports.ll" -disable-output

grep -F 'declare zeroext i1 @c_bool(i1 zeroext)' "$ABI_TMP/boundary.ll" >/dev/null
grep -F 'declare zeroext i8 @c_u8(i8 zeroext)' "$ABI_TMP/boundary.ll" >/dev/null
grep -F 'declare signext i8 @c_i8(i8 signext)' "$ABI_TMP/boundary.ll" >/dev/null
grep -F 'declare zeroext i16 @c_u16(i16 zeroext)' "$ABI_TMP/boundary.ll" >/dev/null
grep -F 'declare signext i16 @c_i16(i16 signext)' "$ABI_TMP/boundary.ll" >/dev/null
grep -F 'declare i64 @c_usize(i64)' "$ABI_TMP/boundary.ll" >/dev/null
grep -F 'declare i64 @c_isize(i64)' "$ABI_TMP/boundary.ll" >/dev/null
grep -F 'define zeroext i1 @fas_bool(i1 zeroext' "$ABI_TMP/exports.ll" >/dev/null
grep -F 'define zeroext i8 @fas_u8(i8 zeroext' "$ABI_TMP/exports.ll" >/dev/null
grep -F 'define signext i8 @fas_i8(i8 signext' "$ABI_TMP/exports.ll" >/dev/null
grep -F 'define zeroext i16 @fas_u16(i16 zeroext' "$ABI_TMP/exports.ll" >/dev/null
grep -F 'define signext i16 @fas_i16(i16 signext' "$ABI_TMP/exports.ll" >/dev/null
grep -F 'define i64 @fas_usize(i64' "$ABI_TMP/exports.ll" >/dev/null
grep -F 'define i64 @fas_isize(i64' "$ABI_TMP/exports.ll" >/dev/null

for level in 0 2; do
    "$CC" -Werror -Wno-override-module -O"$level" "$ABI_TMP/internal.ll" \
        -o "$ABI_TMP/internal-$level"
    "$ABI_TMP/internal-$level"
    "$CC" -Werror -Wno-override-module -std=c17 -O"$level" "$ABI_TMP/boundary.ll" \
        "$ROOT/test/abi_boundary.c" -o "$ABI_TMP/boundary-$level"
    "$ABI_TMP/boundary-$level"
    "$CC" -Werror -Wno-override-module -std=c17 -O"$level" "$ABI_TMP/exports.ll" \
        "$ROOT/test/abi_exports.c" -o "$ABI_TMP/exports-$level"
    "$ABI_TMP/exports-$level"
done

echo "ABI boundary: ok"
