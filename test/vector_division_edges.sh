#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
CC=${CC:-clang-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
DIVISION_TMP=$(mktemp -d)
trap 'rm -rf "$DIVISION_TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/vector_division_edges.fas" \
  >"$DIVISION_TMP/division.ll"
"$LLVM_OPT" -passes=verify "$DIVISION_TMP/division.ll" -disable-output

ulimit -c 0 || true
for level in 0 2; do
  FAS_OPT="$LLVM_OPT" FAS_LLC="$LLVM_LLC" FAS_CC="$CC" \
    "$OCAML_FAS" -O"$level" "$ROOT/test/vector_division_edges.fas" \
    -o "$DIVISION_TMP/division-$level"
  for spec in 1:0 2:132 3:132 4:132 5:132 6:0 7:0 8:132; do
    want=${spec#*:}
    argc=${spec%%:*}
    set +e
    timeout 10 "$DIVISION_TMP/division-$level" $(seq 2 "$argc" 2>/dev/null)
    got=$?
    set -e
    if [ "$got" -ne "$want" ]; then
      echo "vector division edges: argc $argc at -O$level: want $want got $got" >&2
      exit 1
    fi
  done
done

echo "vector division edges: ok"
