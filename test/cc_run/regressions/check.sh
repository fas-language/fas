#!/usr/bin/env bash
set -euo pipefail
ulimit -f 524288

src=$(cd -- "$(dirname -- "$0")" && pwd)
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT

for file in "$src"/*.c; do
    flags=()
    runner=()
    case "${file##*/}" in
        asm_goto_inline.c) flags=(-O2 -fno-pie -no-pie) ;;
        asm_symbolic_immediate.c) flags=(-fno-pie -no-pie) ;;
        char_pointer_sign.c|void_function_pointer.c) flags=(-std=c11) ;;
        export_dynamic.c) flags=(-Xlinker -export-dynamic -ldl) ;;
        flt_rounds.c) flags=(-lm) ;;
        fp_rounding.c) flags=(-frounding-math -lm) ;;
        fp_exceptions.c) flags=(-lm) ;;
        pthread_cancel_cleanup.c) flags=(-fexceptions -pthread); runner=(timeout 15) ;;
        short_wchar.c) flags=(-fshort-wchar) ;;
        unprototyped.c) flags=(-std=c99 -Wno-deprecated-non-prototype) ;;
    esac
    "${CLANG:-clang-22}" -std=gnu23 "$file" "${flags[@]}" -o "$work/clang"
    "${runner[@]}" "$work/clang" > "$work/clang.out"
    "$FCC" "$file" "${flags[@]}" -o "$work/fcc"
    "${runner[@]}" "$work/fcc" > "$work/fcc.out"
    cmp "$work/clang.out" "$work/fcc.out"
    if [[ ${file##*/} == asm_extra_sections.c ]]; then
        for exe in "$work/clang" "$work/fcc"; do
            readelf -SW "$exe" > "$work/sections"
            grep -qF ' __ex_table ' "$work/sections"
            grep -qF ' .altinstructions ' "$work/sections"
        done
    fi
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
