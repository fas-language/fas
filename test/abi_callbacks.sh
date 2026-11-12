#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
ABI_CALLBACK_TMP=$(mktemp -d)
trap 'rm -rf "$ABI_CALLBACK_TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/abi_callbacks.fas" >"$ABI_CALLBACK_TMP/callbacks.ll"
"$LLVM_OPT" -passes=verify "$ABI_CALLBACK_TMP/callbacks.ll" -disable-output
"$LLVM_OPT" -passes='default<O2>' -verify-each "$ABI_CALLBACK_TMP/callbacks.ll" -S -o "$ABI_CALLBACK_TMP/callbacks-o2.ll"
"$LLVM_OPT" -passes=verify "$ABI_CALLBACK_TMP/callbacks-o2.ll" -disable-output

"$OCAML_FAS" --emit-llvm "$ROOT/test/abi_library_api.fas" >"$ABI_CALLBACK_TMP/library.ll"
"$LLVM_OPT" -passes=verify "$ABI_CALLBACK_TMP/library.ll" -disable-output
"$LLVM_OPT" -passes='default<O2>' -verify-each "$ABI_CALLBACK_TMP/library.ll" -S -o "$ABI_CALLBACK_TMP/library-o2.ll"
"$LLVM_OPT" -passes=verify "$ABI_CALLBACK_TMP/library-o2.ll" -disable-output

for level in 0 2; do
    FAS_OPT="$LLVM_OPT" FAS_LLC="$LLVM_LLC" FAS_CC="$CC" \
        "$OCAML_FAS" -O"$level" -c "$ROOT/test/abi_callbacks.fas" -o "$ABI_CALLBACK_TMP/callbacks-$level.o"
    "$CC" -Werror -std=c17 -O"$level" "$ABI_CALLBACK_TMP/callbacks-$level.o" \
        "$ROOT/test/abi_callbacks.c" -o "$ABI_CALLBACK_TMP/callbacks-$level"
    "$ABI_CALLBACK_TMP/callbacks-$level"

    FAS_OPT="$LLVM_OPT" FAS_LLC="$LLVM_LLC" FAS_CC="$CC" \
        "$OCAML_FAS" -O"$level" -c "$ROOT/test/abi_library_api.fas" \
        -o "$ABI_CALLBACK_TMP/library-$level.o"
    "$CC" -Werror -std=c17 -O"$level" "$ABI_CALLBACK_TMP/library-$level.o" \
        "$ROOT/test/abi_library.c" -o "$ABI_CALLBACK_TMP/library-$level"
    "$ABI_CALLBACK_TMP/library-$level"
done

echo "ABI callbacks and library: ok"
