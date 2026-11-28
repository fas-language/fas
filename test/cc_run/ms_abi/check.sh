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

for side in caller callee; do
    file="$src/wide_$side.c"
    "$clang" -std=gnu23 -DCLANG_RECORD_ABI -c "$file" -o "$work/wide_$side.clang.o"
    "${GCC:-gcc}" -std=gnu23 -Wno-psabi -c "$file" -o "$work/wide_$side.gcc.o"
    "$FCC" -c "$file" -o "$work/wide_$side.native.o"
    "$FCC" -S "$file" -o "$work/wide_$side.fir"
    "$FCC_FIR" --llvm -c "$work/wide_$side.fir" -o "$work/wide_$side.llvm.o"
done
for pair in clang.clang gcc.gcc gcc.clang clang.gcc native.clang clang.native \
            native.gcc gcc.native llvm.clang clang.llvm llvm.gcc gcc.llvm native.llvm llvm.native; do
    "$clang" "$work/wide_caller.${pair%.*}.o" "$work/wide_callee.${pair#*.}.o" -o "$work/test"
    "$work/test" fixed > "$work/wide.$pair.fixed"
    cmp "$work/wide.clang.clang.fixed" "$work/wide.$pair.fixed"
    # GCC passes wide vectors through ellipsis indirectly but reads them
    # inline in va_arg, so skip the variadic call with a GCC callee.
    case $pair in
        *.gcc) ;;
        *)
            "$work/test" > "$work/wide.$pair.stdout"
            cmp "$work/wide.clang.clang.stdout" "$work/wide.$pair.stdout"
            ;;
    esac
done
echo "PASS: wide vectors through Microsoft x64 ABI (13 fixed, 9 variadic exact-output checks against Clang records)"
