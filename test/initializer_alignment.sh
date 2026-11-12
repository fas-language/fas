#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ulimit -Sv 4194304
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/initializer_alignment.fas" >"$TMP/program.ll"
"$LLVM_OPT" -passes=verify "$TMP/program.ll" -disable-output
grep -E 'store i64 [^,]+, ptr [^,]+, align 1' "$TMP/program.ll" >/dev/null
grep -E 'store double [^,]+, ptr [^,]+, align 1' "$TMP/program.ll" >/dev/null
grep -E 'store float [^,]+, ptr [^,]+, align 1' "$TMP/program.ll" >/dev/null
grep -E 'store i64 [^,]+, ptr [^,]+, align 8' "$TMP/program.ll" >/dev/null
grep -E 'store double [^,]+, ptr [^,]+, align 8' "$TMP/program.ll" >/dev/null
grep -E 'store float [^,]+, ptr [^,]+, align 4' "$TMP/program.ll" >/dev/null

echo 'initializer_alignment: packed and ordinary integer and float stores: ok'
