#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
VOLATILE_TMP=$(mktemp -d)
trap 'rm -rf "$VOLATILE_TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/volatile_memory.fas" >"$VOLATILE_TMP/volatile.ll"
"$LLVM_OPT" -passes=verify "$VOLATILE_TMP/volatile.ll" -disable-output

ulimit -c 0 || true
for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$VOLATILE_TMP/volatile.ll" \
    -o "$VOLATILE_TMP/volatile-$level.ll"
  "$LLVM_OPT" -passes=verify "$VOLATILE_TMP/volatile-$level.ll" -disable-output
  if [ "$level" -eq 2 ]; then
    if ! grep -Eq '^[[:space:]]*store volatile i32 .*align 1' "$VOLATILE_TMP/volatile-2.ll"; then
      echo "volatile memory: -O2: volatile stores were removed" >&2
      exit 1
    fi
    if grep -Eq '^[[:space:]]*store i32 ' "$VOLATILE_TMP/volatile-2.ll"; then
      echo "volatile memory: -O2: dead ordinary stores remain" >&2
      exit 1
    fi
  fi
  "$CC" -Werror -Wno-override-module -std=c17 -O"$level" \
    "$VOLATILE_TMP/volatile-$level.ll" "$ROOT/test/volatile_memory.c" \
    -o "$VOLATILE_TMP/volatile-$level"
  set +e
  timeout 30 "$VOLATILE_TMP/volatile-$level"
  got=$?
  set -e
  if [ "$got" -ne 0 ]; then
    echo "volatile memory: -O$level: behavior failed (exit $got)" >&2
    exit 1
  fi
done

echo "volatile memory: ok"
