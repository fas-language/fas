#!/bin/sh
# Native backend <-> C across object boundaries: SysV scalar arguments and
# results (ints, narrow ints, f32, f64, stack arguments) in both directions,
# linked by the Fas linker without libc.
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
ABI_TMP=$(mktemp -d)
trap 'rm -rf "$ABI_TMP"' EXIT HUP INT TERM

"$CC" -O2 -ffreestanding -fno-stack-protector -c "$ROOT/test/native_c_abi/c.c" \
  -o "$ABI_TMP/c.o"
"$OCAML_FAS" --backend=native "$ROOT/test/native_c_abi/main.fas" "$ABI_TMP/c.o" \
  -o "$ABI_TMP/main"
"$ABI_TMP/main"
echo "native C ABI: ok"
