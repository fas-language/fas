#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
BUILTIN_TMP=$(mktemp -d)
trap 'rm -rf "$BUILTIN_TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/builtin_if_const.fas" >"$BUILTIN_TMP/input.ll"
"$LLVM_OPT" -passes=verify "$BUILTIN_TMP/input.ll" -disable-output
for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$BUILTIN_TMP/input.ll" -o "$BUILTIN_TMP/opt.ll"
  "$LLVM_OPT" -passes=verify "$BUILTIN_TMP/opt.ll" -disable-output
  "$CC" -Werror -Wno-override-module -O"$level" "$BUILTIN_TMP/opt.ll" \
    "$ROOT/test/builtin_if_const.c" -o "$BUILTIN_TMP/run"
  "$BUILTIN_TMP/run"
done
echo "builtin if constant/runtime parity: ok"
