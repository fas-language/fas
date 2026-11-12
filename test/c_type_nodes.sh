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
TMPDIR=$TMP
export TMPDIR
fail() { echo "c_type_nodes: $*" >&2; exit 1; }

cleanup_kept() {
  sed -n 's/^fas: kept [^:]*: //p' "$1" | tr ', ' '\n' |
    while IFS= read -r path; do
      [ -n "$path" ] || continue
      case "$path" in
        "$TMPDIR"/*) rm -f "$path" ;;
        *) fail "kept path escaped the test temporary directory: $path" ;;
      esac
    done
}

"$OCAML_FAS" --keep --emit-llvm -O0 "$ROOT/test/c_type_nodes.fas" \
  >"$TMP/kept.ll" 2>"$TMP/keep.log"
BINDINGS=$(sed -n 's/^fas: kept C bindings: //p' "$TMP/keep.log")
test -n "$BINDINGS" || fail "kept C bindings manifest was not reported"
cp "$BINDINGS" "$TMP/bindings.txt"
TAB=$(printf '\t')
grep -F "node_qualified${TAB}NodeQualifiedAlias node_qualified${TAB}addr${TAB}__restrict,const,volatile${TAB}" \
  "$TMP/bindings.txt" >/dev/null \
  || fail "qualifiers from all pointer layers are missing from the manifest"
cleanup_kept "$TMP/keep.log"

for level in 0 2; do
  "$OCAML_FAS" --emit-llvm -O"$level" "$ROOT/test/c_type_nodes.fas" \
    >"$TMP/program.O$level.ll"
  "$LLVM_OPT" -passes=verify "$TMP/program.O$level.ll" -disable-output
  "$LLVM_OPT" -S "-passes=default<O$level>" "$TMP/program.O$level.ll" \
    -o "$TMP/program.O$level.opt.ll"
  "$LLVM_OPT" -passes=verify "$TMP/program.O$level.opt.ll" -disable-output
  "$OCAML_FAS" -O"$level" -o "$TMP/fas.O$level" \
    "$ROOT/test/c_type_nodes.fas" "$ROOT/test/c_type_nodes_runtime.c"
  "$CC" -Werror -std=gnu17 -O"$level" "$ROOT/test/c_type_nodes_oracle.c" \
    "$ROOT/test/c_type_nodes_runtime.c" -o "$TMP/oracle.O$level"
  "$TMP/fas.O$level" >"$TMP/fas.O$level.out" \
    || fail "Fas O$level structured type behavior failed"
  "$TMP/oracle.O$level" >"$TMP/oracle.O$level.out" \
    || fail "C O$level structured type oracle failed"
  cmp -s "$TMP/fas.O$level.out" "$TMP/oracle.O$level.out" \
    || fail "Fas and C results differ at O$level"
done
echo "c_type_nodes: nested typedef classification and C oracle at O0/O2: ok"
