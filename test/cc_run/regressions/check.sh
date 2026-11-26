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
        asm_bool_immediate.c|dead_constant_branches.c|goto_into_constant_if_deferred.c|inline_asm_forward_static.c|inline_calls.c|inline_stack_branch.c) flags=(-O2) ;;
        export_dynamic.c) flags=(-Xlinker -export-dynamic -ldl) ;;
        flt_rounds.c) flags=(-lm) ;;
        function_sections.c) flags=(-g) ;;
        fp_rounding.c) flags=(-frounding-math -lm) ;;
        fp_exceptions.c) flags=(-lm) ;;
        kernel_constant_calls.c) flags=(-O2 -ffreestanding -fno-pie -no-pie -mno-red-zone -mcmodel=kernel) ;;
        kernel_auto_init_zero.c) flags=(-O2 -ffreestanding -fno-pie -no-pie -mno-red-zone -mcmodel=kernel
                                        -ftrivial-auto-var-init=zero) ;;
        kernel_stack_canary.c) flags=(-fno-pie -no-pie -mcmodel=kernel -fstack-protector-all) ;;
        kernel_symbol_address.c) flags=(-ffreestanding -fno-pie -no-pie -mcmodel=kernel) ;;
        no_sse_ms_abi.c|no_sse_variadic.c) flags=(-mgeneral-regs-only) ;;
        libm_min_max.c) flags=(-lm) ;;
        pthread_cancel_cleanup.c) flags=(-fexceptions -pthread); runner=(timeout 15) ;;
        auto_var_init_zero.c) flags=(-ftrivial-auto-var-init=zero) ;;
        short_wchar.c) flags=(-fshort-wchar) ;;
        unprototyped.c) flags=(-std=c99 -Wno-deprecated-non-prototype) ;;
    esac
    "${CLANG:-clang-22}" -std=gnu23 "$file" "${flags[@]}" -o "$work/clang"
    "${runner[@]}" "$work/clang" > "$work/clang.out"
    "$FCC" "$file" "${flags[@]}" -o "$work/fcc"
    "${runner[@]}" "$work/fcc" > "$work/fcc.out"
    cmp "$work/clang.out" "$work/fcc.out"
    if [[ ${file##*/} == inline_calls.c ]]; then
        for opt in -O0 -O2; do
            "$FCC" "$opt" --emit-fir "$file" -o "$work/inline.fir"
            grep -q '^func @step ' "$work/inline.fir"
            if grep -Eq 'call .* @(step|required_step|pointer_home|wide_value)\(' "$work/inline.fir"; then
                echo "a direct always_inline call survived at $opt" >&2
                exit 1
            fi
            for sym in recurse varargs with_vla with_setjmp; do
                grep -q "call .* @$sym(" "$work/inline.fir"
            done
            if [[ $opt == -O0 ]]; then
                grep -q 'call .* @small(' "$work/inline.fir"
                "$FCC" -O0 "$file" -o "$work/fcc-debug"
                "$work/fcc-debug" > "$work/fcc-debug.out"
                cmp "$work/clang.out" "$work/fcc-debug.out"
            elif grep -q 'call .* @small(' "$work/inline.fir"; then
                echo "small static inline call survived at $opt" >&2
                exit 1
            fi
        done
    fi
    if [[ ${file##*/} == asm_string_immediate.c ]]; then
        "$FCC" --emit-fir "$file" -o "$work/string.fir"
        grep -Fq 'in("i" ptr @.Lstr.' "$work/string.fir"
    fi
    if [[ ${file##*/} == asm_extra_sections.c ]]; then
        for exe in "$work/clang" "$work/fcc"; do
            readelf -SW "$exe" > "$work/sections"
            grep -qF ' __ex_table ' "$work/sections"
            grep -qF ' .altinstructions ' "$work/sections"
        done
    fi
    if [[ ${file##*/} == asm_discard_sections.c ]]; then
        "$FCC" -c "$file" -o "$work/discard.o"
        readelf -SW "$work/discard.o" > "$work/sections"
        [[ $(grep -c ' \.discard\.noendbr ' "$work/sections") == 1 ]]
        [[ $(grep -c ' \.fas_data ' "$work/sections") == 1 ]]
        readelf -rW "$work/discard.o" | sed -n "/'.rela.discard.noendbr'/,/^$/p" > "$work/relocs"
        grep -q 'contains 2 entries' "$work/relocs"
        grep -q '\.static_call\.text + 0' "$work/relocs"
    fi
    if [[ ${file##*/} == kernel_constant_calls.c ]]; then
        llvm-nm-22 "$work/fcc" > "$work/symbols"
        grep -q ' retained_setting$' "$work/symbols"
    fi
    if [[ ${file##*/} == kernel_stack_canary.c ]]; then
        for exe in "$work/clang" "$work/fcc"; do
            objdump --disassemble=protected_function "$exe" > "$work/canary"
            grep -qF '%gs:0x28' "$work/canary"
        done
    fi
    if [[ ${file##*/} == kernel_symbol_address.c ]]; then
        for compiler in "${CLANG:-clang-22}" "$FCC"; do
            "$compiler" -ffreestanding -fno-pie -mcmodel=kernel -c "$file" -o "$work/address.o"
            readelf -Wr "$work/address.o" > "$work/relocs"
            grep -E -q 'R_X86_64_32S +[0-9a-f]+ kernel_data ' "$work/relocs"
            grep -E -q 'R_X86_64_PC32 +[0-9a-f]+ kernel_data ' "$work/relocs"
        done
    fi
    if [[ ${file##*/} == no_sse_variadic.c || ${file##*/} == no_sse_ms_abi.c ]]; then
        symbol=sum
        if [[ ${file##*/} == no_sse_ms_abi.c ]]; then symbol=firmware_entry; fi
        for exe in "$work/clang" "$work/fcc"; do
            llvm-objdump-22 --disassemble-symbols="$symbol" "$exe" > "$work/disassembly"
            grep -q "<$symbol>:" "$work/disassembly"
            if grep -Eiq '%(xmm|ymm|zmm|st\()' "$work/disassembly"; then
                echo "unexpected FP or vector register in $exe" >&2
                exit 1
            fi
        done
    fi
    if [[ ${file##*/} == function_sections.c ]]; then
        for exe in "$work/clang" "$work/fcc"; do
            readelf -SW "$exe" > "$work/sections"
            grep -qE ' \.fas_test_exec1 .* AX ' "$work/sections"
            grep -qE ' \.fas_test_exec2 .* AX ' "$work/sections"
            grep -qE ' \.fas_test_init_record .* WA ' "$work/sections"
        done
        if command -v llvm-dwarfdump-22 >/dev/null; then
            llvm-dwarfdump-22 --verify "$work/fcc" > "$work/dwarf"
        fi
    fi
    echo "PASS: ${file##*/}"
done

check_inline_error() {
    local expected=$1 code=$2
    printf '%s\n' "$code" > "$work/inline-error.c"
    if "$FCC" -O0 --emit-fir "$work/inline-error.c" -o "$work/inline-error.fir" 2>"$work/inline-error.txt"; then
        echo "expected an always_inline error for: $code" >&2
        exit 1
    fi
    grep -F "cannot inline always_inline function 'helper': $expected" "$work/inline-error.txt"
}

check_inline_error 'definition is unavailable' \
    '__attribute__((always_inline)) int helper(int); int f(int n) { return helper(n); }'
check_inline_error 'recursive call' \
    'static inline __attribute__((always_inline)) int helper(int n) { return n ? helper(n-1) : 1; } int f(int n) { return helper(n); }'
check_inline_error 'function is variadic' \
    'static inline __attribute__((always_inline)) int helper(int n, ...) { return n; } int f(int n) { return helper(n, 0); }'
check_inline_error 'body declares a variably modified type' \
    'static inline __attribute__((always_inline)) int helper(int n) { int a[n]; return sizeof a; } int f(int n) { return helper(n); }'
check_inline_error 'body calls a returns_twice function' \
    '__attribute__((returns_twice)) int twice(void); static inline __attribute__((always_inline)) int helper(void) { return twice(); } int f(void) { return helper(); }'
echo "PASS: required-inline failure diagnostics"

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
