#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
LLVM_OPT=${LLVM_OPT:-opt-22}
CC=${CC:-clang-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
ALIGNMENT_TMP=$(mktemp -d)
trap 'rm -rf "$ALIGNMENT_TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/typed_pointer_alignment.fas" \
  >"$ALIGNMENT_TMP/alignment.ll"
"$LLVM_OPT" -passes=verify "$ALIGNMENT_TMP/alignment.ll" -disable-output

# A vec[8, u64] result is returned through sret (One IR, v0.6), so the raw
# access is the one <8 x i64> load from the argument pointer; the sret slot
# and the spill allocas legitimately carry align 64.
sed -n '/define internal void @read_lanes/,/^}/p' \
  "$ALIGNMENT_TMP/alignment.ll" | grep -E 'load <8 x i64>, ptr .*, align 1$' >/dev/null
sed -n '/define internal void @write_lanes/,/^}/p' \
  "$ALIGNMENT_TMP/alignment.ll" | grep -E 'store <8 x i64> .*, ptr .*, align 1$' >/dev/null
if sed -n '/define internal void @read_lanes/,/^}/p' \
  "$ALIGNMENT_TMP/alignment.ll" | grep -Eq 'load <8 x i64>, ptr .*, align (32|64)'; then
  echo "typed pointer alignment: raw access over-claimed alignment" >&2
  exit 1
fi

for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$ALIGNMENT_TMP/alignment.ll" \
    -o "$ALIGNMENT_TMP/alignment-$level.ll"
  "$CC" -Werror -Wno-override-module -std=c17 -O"$level" \
    "$ALIGNMENT_TMP/alignment-$level.ll" "$ROOT/test/typed_pointer_alignment.c" \
    -o "$ALIGNMENT_TMP/alignment-$level"
  "$ALIGNMENT_TMP/alignment-$level"
done

echo "typed pointer alignment: ok"
