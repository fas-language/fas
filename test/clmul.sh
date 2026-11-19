#!/bin/sh
# clmul/clmul_hi: test/clmul.fas checks itself against a reference loop on
# the native backend, on LLVM's expansion (portable) and, where the CPU has
# it, on pclmulqdq (the oracle's force-native), at O0 and O2.
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
COMPONENT_TMP=$(mktemp -d)
trap 'rm -rf "$COMPONENT_TMP"' EXIT HUP INT TERM
run() {
  label=$1
  shift
  "$@" -o "$COMPONENT_TMP/program" "$ROOT/test/clmul.fas"
  "$COMPONENT_TMP/program" >"$COMPONENT_TMP/out" ||
    { cat "$COMPONENT_TMP/out" >&2; echo "clmul: $label failed" >&2; exit 1; }
  grep -qx "clmul bad 0" "$COMPONENT_TMP/out"
}
for level in 0 2; do
  run "native O$level" env -u FAS_ORACLE "$OCAML_FAS" -O$level
  run "llvm O$level" env FAS_ORACLE=llvm "$OCAML_FAS" -O$level
  if grep -qw pclmulqdq /proc/cpuinfo 2>/dev/null; then
    run "pclmulqdq O$level" env FAS_ORACLE=llvm "$OCAML_FAS" -O$level --target=force-native
  fi
done
echo 'clmul: native, llvm and pclmulqdq at O0/O2 against the reference: ok'
