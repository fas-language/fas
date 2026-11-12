#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
LAYOUT_TMP=$(mktemp -d)
trap 'rm -rf "$LAYOUT_TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/layout_oracle.fas" >"$LAYOUT_TMP/fas.ll"
"$LLVM_OPT" -S -passes=globalopt "$ROOT/test/layout_oracle.llvm" -o "$LAYOUT_TMP/llvm.ll"

awk '
  /^define .*i64 @/ {
    name = $0
    sub(/^.*@/, "", name)
    sub(/\(.*/, "", name)
  }
  /^  ret i64 [0-9]+$/ { print name "=" $3 }
' "$LAYOUT_TMP/fas.ll" | sort >"$LAYOUT_TMP/fas.values"

sed -n 's/^@\([^ ]*\) = .*constant i64 \([0-9][0-9]*\)$/\1=\2/p' \
  "$LAYOUT_TMP/llvm.ll" | sort >"$LAYOUT_TMP/llvm.values"

diff -u "$LAYOUT_TMP/llvm.values" "$LAYOUT_TMP/fas.values"
echo "layout oracle: ok"
