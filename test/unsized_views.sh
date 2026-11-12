#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
UNSIZED_VIEWS_TMP=$(mktemp -d)
trap 'rm -rf "$UNSIZED_VIEWS_TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/unsized_views.fas" >"$UNSIZED_VIEWS_TMP/unsized.ll"
"$LLVM_OPT" -passes=verify "$UNSIZED_VIEWS_TMP/unsized.ll" -disable-output

ulimit -c 0 || true
for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$UNSIZED_VIEWS_TMP/unsized.ll" \
    -o "$UNSIZED_VIEWS_TMP/unsized-$level.ll"
  "$LLVM_OPT" -passes=verify "$UNSIZED_VIEWS_TMP/unsized-$level.ll" -disable-output
  "$CC" -Werror -Wno-override-module -std=c17 -O"$level" \
    "$UNSIZED_VIEWS_TMP/unsized-$level.ll" "$ROOT/test/unsized_views.c" \
    -o "$UNSIZED_VIEWS_TMP/unsized-$level"
  set +e
  timeout 30 "$UNSIZED_VIEWS_TMP/unsized-$level"
  got=$?
  set -e
  if [ "$got" -ne 0 ]; then
    echo "unsized views: -O$level: behavior failed (exit $got)" >&2
    exit 1
  fi
done

echo "unsized views: ok"
