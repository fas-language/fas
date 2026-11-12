#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
IFE_TMP=$(mktemp -d)
trap 'rm -rf "$IFE_TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/if_expressions.fas" >"$IFE_TMP/input.ll"
"$LLVM_OPT" -passes=verify "$IFE_TMP/input.ll" -disable-output

for level in 0 2; do
  "$LLVM_OPT" -passes="default<O$level>" -S "$IFE_TMP/input.ll" -o "$IFE_TMP/opt-$level.ll"
  "$LLVM_OPT" -passes=verify "$IFE_TMP/opt-$level.ll" -disable-output
  "$CC" -Werror -Wno-override-module -O"$level" "$IFE_TMP/opt-$level.ll" \
    "$ROOT/test/if_expressions.c" -o "$IFE_TMP/if-expressions-$level"
  "$IFE_TMP/if-expressions-$level"
done

echo "if expressions: ok"
