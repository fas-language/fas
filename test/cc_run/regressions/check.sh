#!/usr/bin/env bash
set -euo pipefail
ulimit -f 524288

src=$(cd -- "$(dirname -- "$0")" && pwd)
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT

for file in "$src"/*.c; do
    flags=()
    if [ "${file##*/}" = export_dynamic.c ]; then
        flags=(-Xlinker -export-dynamic -ldl)
    fi
    "${CLANG:-clang-22}" -std=gnu23 "$file" "${flags[@]}" -o "$work/clang"
    "$work/clang" > "$work/clang.out"
    "$FCC" "$file" "${flags[@]}" -o "$work/fcc"
    "$work/fcc" > "$work/fcc.out"
    cmp "$work/clang.out" "$work/fcc.out"
    echo "PASS: ${file##*/}"
done

# A single process must release each translation unit before reading the next.
mkdir "$work/include"
printf '#define WIDTH 4\n' > "$work/include/width.h"
{
    printf '#include <width.h>\n'
    printf 'static_assert(sizeof(int) == WIDTH);%.0s\n' {1..5000}
} > "$work/multi-input.c"
inputs=()
for i in {1..24}; do inputs+=("$work/multi-input.c"); done
(ulimit -v 98304 && "$FCC" -I"$work/include" -S -o /dev/null "${inputs[@]}")
echo "PASS: multi-input translation-unit lifetime"
