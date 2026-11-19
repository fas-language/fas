#!/bin/sh
# The library core (lib/*.fas): test/lib/lib.fas checks every module against
# reference implementations; its path results must match OCaml's Filename
# (test/lib/paths.out, made by OCaml) and its realpath the system's. Native
# and LLVM, at O0 and O2.
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
COMPONENT_TMP=$(mktemp -d)
trap 'rm -rf "$COMPONENT_TMP"' EXIT HUP INT TERM
run() {
  label=$1
  shift
  "$@" -o "$COMPONENT_TMP/program" "$ROOT/test/lib/lib.fas"
  dir=$COMPONENT_TMP/dir
  rm -rf "$dir" "$COMPONENT_TMP/cwd"
  mkdir -p "$dir/sub" "$COMPONENT_TMP/cwd"
  ln -s sub "$dir/link"
  (cd "$COMPONENT_TMP/cwd" && LIB_TEST_ENV=a=b "$COMPONENT_TMP/program" "$dir") \
    >"$COMPONENT_TMP/out" ||
    { cat "$COMPONENT_TMP/out" >&2; echo "lib: $label failed" >&2; exit 1; }
  grep -qx "lib bad 0" "$COMPONENT_TMP/out"
  head -n 22 "$COMPONENT_TMP/out" | diff -u "$ROOT/test/lib/paths.out" - ||
    { echo "lib: $label: paths differ from OCaml's Filename" >&2; exit 1; }
  grep -qx "realpath=$(realpath "$dir/sub")" "$COMPONENT_TMP/out" ||
    { echo "lib: $label: realpath differs" >&2; exit 1; }
  # atomic writes leave no temporary files behind
  [ "$(ls -A "$dir" | tr '\n' ' ')" = "a.bin link rel sub " ] ||
    { ls -A "$dir" >&2; echo "lib: $label: stray files" >&2; exit 1; }
  [ -z "$(ls -A "$COMPONENT_TMP/cwd")" ] ||
    { echo "lib: $label: stray files in the current directory" >&2; exit 1; }
}
for level in 0 2; do
  run "native O$level" env -u FAS_ORACLE "$OCAML_FAS" -O$level
  run "llvm O$level" env FAS_ORACLE=llvm "$OCAML_FAS" -O$level
done
echo 'lib: library core on native and llvm at O0/O2: ok'
