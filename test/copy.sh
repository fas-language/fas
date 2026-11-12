#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
COPY_TMP=$(mktemp -d)
trap 'rm -rf "$COPY_TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/copy.fas" >"$COPY_TMP/copy.ll"
"$LLVM_OPT" -passes=verify "$COPY_TMP/copy.ll" -disable-output

for source in copy_compile_65536 copy_compile_1048576 copy_compile_struct; do
  timeout 1 "$OCAML_FAS" --emit-llvm "$ROOT/test/$source.fas" \
    >"$COPY_TMP/$source.ll"
  bytes=$(wc -c <"$COPY_TMP/$source.ll")
  if [ "$bytes" -ge 20000 ]; then
    echo "copy: $source rendered $bytes bytes, expected under 20000" >&2
    exit 1
  fi
  "$LLVM_OPT" -passes=verify "$COPY_TMP/$source.ll" -disable-output
done

loop_ir=$(awk '/^define void @fas_copy_loop\(/,/^}/' "$COPY_TMP/copy.ll")
scratch_count=$(printf '%s\n' "$loop_ir" | grep -Ec 'alloca \[(8 x i32|32 x i8)\]' || true)
if [ "$scratch_count" -ne 1 ]; then
  echo "copy: expected one loop scratch alloca, found $scratch_count" >&2
  exit 1
fi
if ! printf '%s\n' "$loop_ir" | awk '
  /^define / { in_function = 1; next }
  in_function && /^}/ { exit (alloca_count == 1 ? 0 : 1) }
  in_function && /^  br / { branched = 1 }
  in_function && /alloca \[(8 x i32|32 x i8)\]/ {
    if (branched) exit 1
    alloca_count++
  }
  END { if (in_function && !alloca_count) exit 1 }
'; then
  echo "copy: loop scratch alloca was not in the entry block" >&2
  exit 1
fi

ulimit -c 0 || true
for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$COPY_TMP/copy.ll" \
    -o "$COPY_TMP/copy-$level.ll"
  "$LLVM_OPT" -passes=verify "$COPY_TMP/copy-$level.ll" -disable-output
  "$CC" -Werror -Wno-override-module -std=c17 -O"$level" \
    "$COPY_TMP/copy-$level.ll" "$ROOT/test/copy.c" -o "$COPY_TMP/copy-$level"
  set +e
  if [ "$level" -eq 2 ]; then
    timeout 30 "$COPY_TMP/copy-$level" --measure
  else
    timeout 30 "$COPY_TMP/copy-$level"
  fi
  got=$?
  set -e
  if [ "$got" -ne 0 ]; then
    echo "copy: -O$level: behavior failed (exit $got)" >&2
    exit 1
  fi
done

echo "copy: ok"
