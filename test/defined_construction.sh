#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
CC=${CC:-clang-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
CONST_TMP=$(mktemp -d)
trap 'rm -rf "$CONST_TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/defined_construction.fas" >"$CONST_TMP/defined.ll"
"$LLVM_OPT" -passes=verify "$CONST_TMP/defined.ll" -disable-output
if grep -E '\b(poison|undef)\b' "$CONST_TMP/defined.ll" >/dev/null; then
  echo "defined construction: undefined value in frontend IR" >&2
  exit 1
fi
for needle in \
  'shl <4 x i32>' \
  'icmp eq <4 x i32>' \
  'xor <4 x i1>' \
  'zext <4 x i32>' \
  'udiv <4 x i32>' \
  'sdiv <4 x i32>' \
  'urem <4 x i32>' \
  'srem <4 x i32>'; do
  if ! grep -F "$needle" "$CONST_TMP/defined.ll" >/dev/null; then
    echo "defined construction: missing family op $needle" >&2
    exit 1
  fi
done
"$LLVM_OPT" "-passes=default<O2>" "$CONST_TMP/defined.ll" -S -o "$CONST_TMP/defined.O2.ll"
"$LLVM_OPT" -passes=verify "$CONST_TMP/defined.O2.ll" -disable-output
"$LLVM_OPT" "-passes=default<O3>" "$CONST_TMP/defined.ll" -S -o "$CONST_TMP/defined.O3.ll"
"$LLVM_OPT" -passes=verify "$CONST_TMP/defined.O3.ll" -disable-output

ulimit -c 0 || true
for level in 0 2 3; do
  FAS_OPT="$LLVM_OPT" FAS_LLC="$LLVM_LLC" FAS_CC="$CC" \
    "$OCAML_FAS" -O"$level" "$ROOT/test/defined_construction.fas" -o "$CONST_TMP/defined-$level"
  set +e
  timeout 30 "$CONST_TMP/defined-$level"
  got=$?
  set -e
  if [ "$got" -ne 0 ]; then
    echo "defined construction: defined-$level: want 0 got $got" >&2
    exit 1
  fi
done

printf 'defined construction: ok\n'
