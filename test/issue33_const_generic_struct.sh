#!/bin/sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
ISSUE_TMP=$(mktemp -d)
trap 'rm -rf "$ISSUE_TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/issue33_const_generic_struct.fas" >"$ISSUE_TMP/issue33.ll"
# Fir has no named struct types (roadmap v0.6), so the layout of
# Bytes[3] is checked by the program's exit status (its size) below.
"$LLVM_OPT" -passes=verify "$ISSUE_TMP/issue33.ll" -disable-output

"$OCAML_FAS" "$ROOT/test/issue33_const_generic_struct.fas" -o "$ISSUE_TMP/issue33"
if "$ISSUE_TMP/issue33"; then
  status=0
else
  status=$?
fi
[ "$status" -eq 3 ]
echo "issue 33: ok"
