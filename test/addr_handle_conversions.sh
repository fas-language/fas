#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
AHC_TMP=$(mktemp -d)
trap 'rm -rf "$AHC_TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/addr_handle_conversions.fas" >"$AHC_TMP/roundtrip.ll"
"$LLVM_OPT" -passes=verify "$AHC_TMP/roundtrip.ll" -disable-output

ulimit -c 0 || true
for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$AHC_TMP/roundtrip.ll" \
    -o "$AHC_TMP/roundtrip-$level.ll"
  "$LLVM_OPT" -passes=verify "$AHC_TMP/roundtrip-$level.ll" -disable-output
  "$CC" -Werror -Wno-override-module -std=c17 -O"$level" "$AHC_TMP/roundtrip-$level.ll" \
    "$ROOT/test/addr_handle_conversions.c" -o "$AHC_TMP/roundtrip-$level"
  set +e
  timeout 30 "$AHC_TMP/roundtrip-$level"
  got=$?
  set -e
  if [ "$got" -ne 0 ]; then
    echo "addr/handle conversions: -O$level: round-trip failed (exit $got)" >&2
    exit 1
  fi
done

echo "addr/handle conversions: ok"
