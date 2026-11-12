#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
FP_TMP=$(mktemp -d)
trap 'rm -rf "$FP_TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/footprints.fas" >"$FP_TMP/footprints.ll"
"$LLVM_OPT" -passes=verify "$FP_TMP/footprints.ll" -disable-output

for symbol in fas_write_footprints fas_guard_store fas_guard_bool_store; do
  body=$(awk -v name="$symbol" '$0 ~ "^define .*@" name "\\(" { in_fn = 1 } in_fn { print } in_fn && /^}/ { exit }' "$FP_TMP/footprints.ll")
  if printf '%s\n' "$body" | grep -Eq ' = load (i[0-9]+|<[0-9]+ x i[0-9]+|\[[^]]+\])'; then
    echo "footprints: $symbol reads a stored value" >&2
    exit 1
  fi
done

ulimit -c 0 || true
for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$FP_TMP/footprints.ll" \
    -o "$FP_TMP/footprints-$level.ll"
  "$LLVM_OPT" -passes=verify "$FP_TMP/footprints-$level.ll" -disable-output
  "$CC" -Werror -Wno-override-module -std=c17 -O"$level" \
    "$FP_TMP/footprints-$level.ll" "$ROOT/test/footprints.c" \
    -o "$FP_TMP/footprints-$level"
  set +e
  timeout 30 "$FP_TMP/footprints-$level"
  got=$?
  set -e
  if [ "$got" -ne 0 ]; then
    echo "footprints: -O$level: behavior failed (exit $got)" >&2
    exit 1
  fi
done

echo "footprints: ok"
