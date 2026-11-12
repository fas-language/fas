#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
VV_TMP=$(mktemp -d)
trap 'rm -rf "$VV_TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/volatile_vectors.fas" >"$VV_TMP/volatile.ll"
"$LLVM_OPT" -passes=verify "$VV_TMP/volatile.ll" -disable-output

function_body() {
  awk -v name="$1" '$0 ~ "^define .*@" name "\\(" { in_fn = 1 } in_fn { print } in_fn && /^}/ { exit }' "$VV_TMP/volatile.ll"
}

assert_count() {
  count=$(printf '%s\n' "$1" | grep -c "$2" || true)
  if [ "$count" -ne "$3" ]; then
    echo "volatile vectors: expected $3 occurrences of $2, found $count" >&2
    exit 1
  fi
}

int_load=$(function_body volatile_load_int)
int_store=$(function_body volatile_store_int)
bool_load=$(function_body volatile_load_bool)
bool_store=$(function_body volatile_store_bool)
assert_count "$int_load" 'load volatile i32' 3
assert_count "$int_store" 'store volatile i32' 3
assert_count "$bool_load" 'load volatile i16' 1
assert_count "$bool_store" 'store volatile i16' 1
assert_count "$int_load" 'load volatile i32,.*align 1' 3
assert_count "$int_store" 'store volatile i32 .*align 1' 3
assert_count "$bool_load" 'load volatile i16,.*align 1' 1
assert_count "$bool_store" 'store volatile i16 .*align 1' 1
assert_count "$int_load" 'volatile ' 3
assert_count "$int_store" 'volatile ' 3
assert_count "$bool_load" 'volatile ' 1
assert_count "$bool_store" 'volatile ' 1
# %sN are the backend's own slots: a vector result crosses the call in its
# C form (an agg), so it is stored to one and read back as eightbytes
if printf '%s\n' "$int_load" "$bool_load" | grep -E ' = load (i[0-9]+|<)' | grep -Evq ', ptr %s[0-9]+,'; then
  echo "volatile vectors: a load used a nonvolatile access" >&2
  exit 1
fi
for body in "$int_store" "$bool_store"; do
  destinations=$(printf '%s\n' "$body" | sed -nE 's/.*store volatile [^,]+, ptr (%[^,]+), align.*/\1/p')
  for destination in $destinations; do
    if printf '%s\n' "$body" | grep ' = load ' | grep -F "ptr $destination," >/dev/null; then
      echo "volatile vectors: a store reads its destination" >&2
      exit 1
    fi
    if printf '%s\n' "$body" | grep -E '^[[:space:]]*store (i[0-9]+|<)' | grep -F "ptr $destination," >/dev/null; then
      echo "volatile vectors: a store used a nonvolatile access" >&2
      exit 1
    fi
  done
done
for body in "$int_load" "$int_store"; do
  access1=$(printf '%s\n' "$body" | grep -n 'volatile i32' | sed -n '1p' | cut -d: -f1)
  access2=$(printf '%s\n' "$body" | grep -n 'volatile i32' | sed -n '2p' | cut -d: -f1)
  access3=$(printf '%s\n' "$body" | grep -n 'volatile i32' | sed -n '3p' | cut -d: -f1)
  offset4=$(printf '%s\n' "$body" | grep -n 'getelementptr i8, ptr .*i64 4' | cut -d: -f1)
  offset8=$(printf '%s\n' "$body" | grep -n 'getelementptr i8, ptr .*i64 8' | cut -d: -f1)
  if [ -z "$offset4" ] || [ -z "$offset8" ] || [ "$access1" -ge "$offset4" ] ||
     [ "$offset4" -ge "$access2" ] || [ "$access2" -ge "$offset8" ] ||
     [ "$offset8" -ge "$access3" ]; then
    echo "volatile vectors: i32 lane addresses are not ascending" >&2
    exit 1
  fi
done

ulimit -c 0 || true
for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$VV_TMP/volatile.ll" \
    -o "$VV_TMP/volatile-$level.ll"
  "$LLVM_OPT" -passes=verify "$VV_TMP/volatile-$level.ll" -disable-output
  if [ "$level" -eq 2 ]; then
    loop_body=$(awk '$0 ~ "^define .*@fas_volatile_loop\\(" { in_fn = 1 } in_fn { print } in_fn && /^}/ { exit }' "$VV_TMP/volatile-2.ll")
    if ! printf '%s\n' "$loop_body" | grep -q 'store volatile i32'; then
      echo "volatile vectors: -O2 removed loop stores" >&2
      exit 1
    fi
    if ! printf '%s\n' "$loop_body" | grep -Eq 'br label %'; then
      echo "volatile vectors: -O2 removed the dynamic loop" >&2
      exit 1
    fi
  fi
  "$CC" -Werror -Wno-override-module -std=c17 -O"$level" \
    "$VV_TMP/volatile-$level.ll" "$ROOT/test/volatile_vectors.c" \
    -o "$VV_TMP/volatile-$level"
  set +e
  timeout 30 "$VV_TMP/volatile-$level"
  got=$?
  set -e
  if [ "$got" -ne 0 ]; then
    echo "volatile vectors: -O$level: behavior failed (exit $got)" >&2
    exit 1
  fi
done

echo "volatile vectors: ok"
