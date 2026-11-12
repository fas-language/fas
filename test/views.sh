#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
VIEWS_TMP=$(mktemp -d)
trap 'rm -rf "$VIEWS_TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/views.fas" >"$VIEWS_TMP/views.ll"
"$LLVM_OPT" -passes=verify "$VIEWS_TMP/views.ll" -disable-output

ulimit -c 0 || true
for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$VIEWS_TMP/views.ll" \
    -o "$VIEWS_TMP/views-$level.ll"
  "$LLVM_OPT" -passes=verify "$VIEWS_TMP/views-$level.ll" -disable-output
  "$CC" -Werror -Wno-override-module -std=c17 -O"$level" \
    "$VIEWS_TMP/views-$level.ll" "$ROOT/test/views.c" -o "$VIEWS_TMP/views-$level"
  set +e
  timeout 30 "$VIEWS_TMP/views-$level"
  got=$?
  set -e
  if [ "$got" -ne 0 ]; then
    echo "views: -O$level: binding behavior failed (exit $got)" >&2
    exit 1
  fi
done

echo "views: ok"
