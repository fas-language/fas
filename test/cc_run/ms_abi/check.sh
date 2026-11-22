#!/usr/bin/env bash
# Compile each side of the call boundary independently and mix the objects.
set -euo pipefail
clang=${CLANG:-clang-22}
src=$(cd -- "$(dirname -- "$0")" && pwd)
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT

for side in caller callee; do
    "$clang" -std=gnu23 -c "$src/$side.c" -o "$work/$side.clang.o"
    "$FCC" -c "$src/$side.c" -o "$work/$side.native.o"
    "$FCC" -S "$src/$side.c" -o "$work/$side.fir"
    "$FCC_FIR" --llvm -c "$work/$side.fir" -o "$work/$side.llvm.o"
done
"$clang" -c "$src/registers.S" -o "$work/registers.o"
for pair in clang.clang native.clang clang.native native.native llvm.clang clang.llvm llvm.llvm; do
    "$clang" "$work/caller.${pair%.*}.o" "$work/callee.${pair#*.}.o" "$work/registers.o" -o "$work/test"
    "$work/test" > "$work/$pair.stdout"
    cmp "$work/clang.clang.stdout" "$work/$pair.stdout"
done
echo "PASS: Microsoft x64 ABI (6 exact-output links against clang-22)"

# Clang 22 passes a short vector indirectly but reads its va_arg slot
# directly. Use the expected lane values for this separate regression.
printf '17 19\n' > "$work/vector.expected"
"$FCC" "$src/vector_vararg.c" -o "$work/vector-native"
"$FCC" -S "$src/vector_vararg.c" -o "$work/vector.fir"
"$FCC_FIR" --llvm -c "$work/vector.fir" -o "$work/vector.o"
"$clang" "$work/vector.o" -o "$work/vector-llvm"
for backend in native llvm; do
    "$work/vector-$backend" > "$work/vector.stdout"
    cmp "$work/vector.expected" "$work/vector.stdout"
done
echo "PASS: short SIMD vector through Microsoft va_arg (native and LLVM)"
