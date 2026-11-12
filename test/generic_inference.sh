#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
LLVM_OPT=${LLVM_OPT:-opt-22}
COMPONENT_TMP=$(mktemp -d)
trap 'rm -rf "$COMPONENT_TMP"' EXIT HUP INT TERM
sed -e 's/max(small, 12)/max[u8](small, 12)/' \
  -e 's/total(values)/total[4,u32](values)/' \
  -e 's/ring_value(value)/ring_value[4](value)/' \
  -e 's/relay(narrow)/relay[u8](narrow)/' \
  -e 's/relay(wide)/relay[u32](wide)/' \
  "$ROOT/test/generic_inference.fas" > "$COMPONENT_TMP/explicit.fas"
cp "$ROOT/test/generic_inference.fas" "$COMPONENT_TMP/source.fas"
"$OCAML_FAS" --emit-llvm "$COMPONENT_TMP/source.fas" > "$COMPONENT_TMP/inferred.ll"
cp "$COMPONENT_TMP/explicit.fas" "$COMPONENT_TMP/source.fas"
"$OCAML_FAS" --emit-llvm "$COMPONENT_TMP/source.fas" > "$COMPONENT_TMP/explicit.ll"
cmp "$COMPONENT_TMP/inferred.ll" "$COMPONENT_TMP/explicit.ll"
cp "$ROOT/test/generic_inference.fas" "$COMPONENT_TMP/source.fas"
"$LLVM_OPT" -passes=verify "$COMPONENT_TMP/inferred.ll" -disable-output
for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$COMPONENT_TMP/inferred.ll" -o "$COMPONENT_TMP/optimized.ll"
  "$LLVM_OPT" -passes=verify "$COMPONENT_TMP/optimized.ll" -disable-output
  "$OCAML_FAS" -O"$level" "$COMPONENT_TMP/source.fas" -o "$COMPONENT_TMP/program"
  "$COMPONENT_TMP/program"
done
echo 'generic_inference: inferred specializations against C oracle at O0/O2: ok'
