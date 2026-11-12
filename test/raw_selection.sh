#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
RS_TMP=$(mktemp -d)
trap 'rm -rf "$RS_TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/raw_selection.fas" >"$RS_TMP/raw.ll"
"$LLVM_OPT" -passes=verify "$RS_TMP/raw.ll" -disable-output

ulimit -c 0 || true
for level in 0 2; do
  "$CC" -Werror -Wno-override-module -std=c17 -O"$level" "$RS_TMP/raw.ll" \
    "$ROOT/test/raw_selection.c" -o "$RS_TMP/raw-$level"
  set +e
  timeout 30 "$RS_TMP/raw-$level"
  got=$?
  set -e
  if [ "$got" -ne 0 ]; then
    echo "raw selection: -O$level: round-trip failed (exit $got)" >&2
    exit 1
  fi
done

echo "raw selection: ok"
