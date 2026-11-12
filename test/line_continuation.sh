#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
LC_TMP=$(mktemp -d)
trap 'rm -rf "$LC_TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/line_continuation.fas" >"$LC_TMP/input.ll"
"$LLVM_OPT" -passes=verify "$LC_TMP/input.ll" -disable-output

for level in 0 2; do
  "$LLVM_OPT" -passes="default<O$level>" -S "$LC_TMP/input.ll" -o "$LC_TMP/opt-$level.ll"
  "$LLVM_OPT" -passes=verify "$LC_TMP/opt-$level.ll" -disable-output
  "$CC" -Werror -Wno-override-module -O"$level" "$LC_TMP/opt-$level.ll" \
    "$ROOT/test/line_continuation.c" -o "$LC_TMP/line-continuation-$level"
  "$LC_TMP/line-continuation-$level"
done

echo "line continuation: ok"
