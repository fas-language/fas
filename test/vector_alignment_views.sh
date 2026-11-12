#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM
fail() { echo "vector_alignment_views: $*" >&2; exit 1; }

"$OCAML_FAS" --emit-llvm "$ROOT/test/vector_alignment_views.fas" >"$TMP/program.ll"
"$LLVM_OPT" -passes=verify "$TMP/program.ll" -disable-output
for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$TMP/program.ll" \
    -o "$TMP/program.O$level.ll"
  "$LLVM_OPT" -passes=verify "$TMP/program.O$level.ll" -disable-output
  "$OCAML_FAS" -O"$level" -o "$TMP/program.O$level" \
    "$ROOT/test/vector_alignment_views.fas" "$ROOT/test/vector_alignment_views_oracle.c"
  "$TMP/program.O$level" >"$TMP/observed" || fail "misaligned access failed at O$level: $(cat "$TMP/observed")"
  printf 'vector=110/110 lane=77/77 record=26/26 field=88/88\n' >"$TMP/expected"
  cmp -s "$TMP/expected" "$TMP/observed" || fail "C oracle differed at O$level"
done
echo "vector_alignment_views: raw vector and record field view reads and writes at O0/O2: ok"
