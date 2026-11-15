#!/bin/sh
# Evaluation order through the built-in backend and the LLVM oracle at O0 and O2.
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
ORDER_TMP=$(mktemp -d)
trap 'rm -rf "$ORDER_TMP"' EXIT HUP INT TERM

for oracle in native llvm; do
  for level in 0 2; do
    if [ "$oracle" = llvm ]; then
      FAS_ORACLE=llvm "$OCAML_FAS" -O"$level" "$ROOT/test/eval_order.fas" -o "$ORDER_TMP/b"
    else
      env -u FAS_ORACLE "$OCAML_FAS" -O"$level" "$ROOT/test/eval_order.fas" -o "$ORDER_TMP/b"
    fi
    status=0
    "$ORDER_TMP/b" > "$ORDER_TMP/out" || status=$?
    [ "$status" = 0 ] || {
      echo "eval_order: $oracle -O$level exited $status" >&2
      exit 1
    }
    diff -u "$ROOT/test/eval_order.expected" "$ORDER_TMP/out" >&2 || {
      echo "eval_order: $oracle -O$level evaluated out of order" >&2
      exit 1
    }
  done
done

echo "eval_order: ok"
