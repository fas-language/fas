#!/bin/sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
CC=${CC:-clang-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
NATIVE_TMP=$(mktemp -d)
trap 'rm -rf "$NATIVE_TMP"' EXIT HUP INT TERM

FAS_OPT="$LLVM_OPT" FAS_LLC="$LLVM_LLC" FAS_CC="$CC" \
  "$OCAML_FAS" -O2 "$ROOT/test/native_backend.fas" -o "$NATIVE_TMP/llvm"
"$OCAML_FAS" --backend=native "$ROOT/test/native_backend.fas" -o "$NATIVE_TMP/native"

set +e
"$NATIVE_TMP/llvm" >"$NATIVE_TMP/llvm.out"
llvm_status=$?
"$NATIVE_TMP/native" >"$NATIVE_TMP/native.out"
native_status=$?
set -e
test "$llvm_status" = 3
test "$native_status" = 3
cmp "$NATIVE_TMP/llvm.out" "$NATIVE_TMP/native.out"
grep -q '^cmp ok$' "$NATIVE_TMP/native.out"

# A static executable with no interpreter and a named `main`
if command -v readelf >/dev/null 2>&1; then
  if readelf -l "$NATIVE_TMP/native" | grep -q INTERP; then
    echo "native executable has an interpreter" >&2; exit 1
  fi
  readelf -s "$NATIVE_TMP/native" | grep -q ' main$'
fi

# Scalar floats: the native and LLVM builds print the same bits
FAS_OPT="$LLVM_OPT" FAS_LLC="$LLVM_LLC" FAS_CC="$CC" \
  "$OCAML_FAS" -O2 "$ROOT/test/native_floats.fas" -o "$NATIVE_TMP/floats_llvm"
"$OCAML_FAS" --backend=native "$ROOT/test/native_floats.fas" -o "$NATIVE_TMP/floats"
"$NATIVE_TMP/floats_llvm" >"$NATIVE_TMP/floats_llvm.out"
"$NATIVE_TMP/floats" >"$NATIVE_TMP/floats.out"
cmp "$NATIVE_TMP/floats_llvm.out" "$NATIVE_TMP/floats.out"

# Vectors (scalarized): the native and LLVM builds print the same bits
FAS_OPT="$LLVM_OPT" FAS_LLC="$LLVM_LLC" FAS_CC="$CC" \
  "$OCAML_FAS" -O2 "$ROOT/test/native_vectors.fas" -o "$NATIVE_TMP/vectors_llvm"
"$OCAML_FAS" --backend=native "$ROOT/test/native_vectors.fas" -o "$NATIVE_TMP/vectors"
"$NATIVE_TMP/vectors_llvm" >"$NATIVE_TMP/vectors_llvm.out"
"$NATIVE_TMP/vectors" >"$NATIVE_TMP/vectors.out"
cmp "$NATIVE_TMP/vectors_llvm.out" "$NATIVE_TMP/vectors.out"
"$OCAML_FAS" --backend=native --target=force-native "$ROOT/test/native_vectors.fas" \
  -o "$NATIVE_TMP/vectors_masked"
"$NATIVE_TMP/vectors_masked" >"$NATIVE_TMP/vectors_masked.out"
cmp "$NATIVE_TMP/vectors_llvm.out" "$NATIVE_TMP/vectors_masked.out"
FAS_NATIVE_NOSIMD=1 "$OCAML_FAS" --backend=native "$ROOT/test/native_vectors.fas" \
  -o "$NATIVE_TMP/vectors_lanes"
"$NATIVE_TMP/vectors_lanes" >"$NATIVE_TMP/vectors_lanes.out"
cmp "$NATIVE_TMP/vectors_llvm.out" "$NATIVE_TMP/vectors_lanes.out"

# C imports select hosted startup; explicit freestanding keeps libc unresolved.
printf 'use "C" <stdio.h>\nfn main() i32 {\n  puts(c"hi")\n  return 0\n}\n' \
  >"$NATIVE_TMP/unsupported.fas"
FAS_CC="$CC" "$OCAML_FAS" --backend=native "$NATIVE_TMP/unsupported.fas" \
  -o "$NATIVE_TMP/hosted"
"$NATIVE_TMP/hosted" >"$NATIVE_TMP/hosted.out"
grep -qx 'hi' "$NATIVE_TMP/hosted.out"
if "$OCAML_FAS" --backend=native --freestanding "$NATIVE_TMP/unsupported.fas" -o "$NATIVE_TMP/unsupported" \
  2>"$NATIVE_TMP/unsupported.err"; then
  echo "freestanding native link unexpectedly resolved libc" >&2; exit 1
fi
grep -q 'native backend:' "$NATIVE_TMP/unsupported.err"

echo "native backend: ok"
