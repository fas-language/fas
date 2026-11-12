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
fail() { echo "c_records: $*" >&2; exit 1; }

"$OCAML_FAS" --emit-llvm "$ROOT/test/c_records.fas" >"$TMP/program.ll"
"$LLVM_OPT" -passes=verify "$TMP/program.ll" -disable-output
for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$TMP/program.ll" \
    -o "$TMP/program.O$level.ll"
  "$LLVM_OPT" -passes=verify "$TMP/program.O$level.ll" -disable-output
  "$OCAML_FAS" -O"$level" -o "$TMP/fas.O$level" \
    "$ROOT/test/c_records.fas" "$ROOT/test/c_import/c_records_runtime.c"
  "$CC" -Werror -std=c17 -O"$level" "$ROOT/test/c_records_oracle.c" \
    "$ROOT/test/c_import/c_records_runtime.c" -o "$TMP/oracle.O$level"
  timeout 30 "$TMP/fas.O$level" >"$TMP/fas.O$level.out" \
    || fail "Fas O$level record behavior failed"
  timeout 30 "$TMP/oracle.O$level" >"$TMP/oracle.out" \
    || fail "C O$level oracle failed"
  cmp -s "$TMP/fas.O$level.out" "$TMP/oracle.out" \
    || fail "Fas and C record results differ at O$level"
  # x86-64 psABI: array variables of 16+ bytes are 16-aligned; C code
  # (and clang's UBSan alignment check) assumes it for these externs.
  for sym in fas_exported_records fas_link_table; do
    addr=$(nm "$TMP/fas.O$level" | awk -v s="$sym" '$3 == s { print $1 }')
    [ -n "$addr" ] || fail "missing $sym at O$level"
    case "$addr" in
      *0) ;;
      *) fail "$sym is not 16-aligned at O$level ($addr)" ;;
    esac
  done
done
"$OCAML_FAS" --emit-header -o "$TMP/fas_records.h" \
  "$ROOT/test/c_records.fas"
cat >"$TMP/header_probe.c" <<'EOF'
#include "fas_records.h"
int fas_records_header_probe(void) {
  return (int)sizeof(FasRecord) + (int)sizeof(FasAddressEntry);
}
EOF
"$CC" -Werror -std=c17 -I"$TMP" -fsyntax-only "$TMP/header_probe.c"
"$CC" -Werror -Wno-main -std=c++17 -x c++ -I"$TMP" -fsyntax-only \
  "$TMP/header_probe.c"
echo "c_records: checked layouts, globals, pointers, address constants and C oracle at O0/O2: ok"
