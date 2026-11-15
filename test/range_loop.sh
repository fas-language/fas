#!/bin/sh
# range loops through the built-in backend and the LLVM oracle at O0 and O2.
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
RANGE_TMP=$(mktemp -d)
trap 'rm -rf "$RANGE_TMP"' EXIT HUP INT TERM

for oracle in native llvm; do
  for level in 0 2; do
    if [ "$oracle" = llvm ]; then
      FAS_ORACLE=llvm "$OCAML_FAS" -O"$level" "$ROOT/test/range_loop.fas" -o "$RANGE_TMP/b"
    else
      env -u FAS_ORACLE "$OCAML_FAS" -O"$level" "$ROOT/test/range_loop.fas" -o "$RANGE_TMP/b"
    fi
    status=0
    "$RANGE_TMP/b" || status=$?
    [ "$status" = 0 ] || {
      echo "range_loop: $oracle -O$level failed check $status" >&2
      exit 1
    }
  done
done

echo "range_loop: ok"
