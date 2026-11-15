#!/bin/sh
# D12 plain string address constants, built-in backend and LLVM oracle, O0 and O2.
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
PSC_TMP=$(mktemp -d)
trap 'rm -rf "$PSC_TMP"' EXIT HUP INT TERM

for oracle in native llvm; do
  for level in 0 2; do
    if [ "$oracle" = llvm ]; then
      FAS_ORACLE=llvm "$OCAML_FAS" -O"$level" "$ROOT/test/plain_string_constants.fas" -o "$PSC_TMP/b"
    else
      env -u FAS_ORACLE "$OCAML_FAS" -O"$level" "$ROOT/test/plain_string_constants.fas" -o "$PSC_TMP/b"
    fi
    status=0
    "$PSC_TMP/b" || status=$?
    [ "$status" = 0 ] || {
      echo "plain_string_constants: $oracle -O$level failed check $status" >&2
      exit 1
    }
  done
done

echo "plain_string_constants: ok"
