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
        asm_bool_immediate.c|dead_constant_branches.c|goto_into_constant_if_deferred.c|inline_asm_forward_static.c|inline_branch_edges.c|inline_calls.c|inline_known_callee.c|inline_stack_branch.c) flags=(-O2) ;;
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
        noreturn_control_flow.c) flags=(-O0 -fstack-protector-all) ;;
        noreturn_wrappers.c) flags=(-O2 -ffreestanding -fno-pie -no-pie -mno-red-zone -mcmodel=kernel) ;;
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
            if grep -Eq 'call .* @(step|required_step|pointer_home|required_vla|wide_value)\(' "$work/inline.fir"; then
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
    if [[ ${file##*/} == asm_local_label_reloc.c ]]; then
        "$FCC" -c "$file" -o "$work/local-label.o"
        readelf -sWr "$work/local-label.o" > "$work/elf"
        sed -n "/'.rela.discard.func_stack_frame_non_standard'/,/^$/p" "$work/elf" > "$work/relocs"
        grep -Eq 'R_X86_64_64 .* \.rodata \+ 4$' "$work/relocs"
        grep -Eq 'R_X86_64_64 .* asm_global_template \+ 0$' "$work/relocs"
        grep -Eq 'NOTYPE +LOCAL .* asm_local_template$' "$work/elf"
    fi
    if [[ ${file##*/} == inline_branch_edges.c ]]; then
        "$FCC" -O2 -DCHECK_SMAP -c "$file" -o "$work/smap.o"
        objdump --no-show-raw-insn -d --disassemble=smap_guard "$work/smap.o" > "$work/smap"
        grep -Eq '[[:space:]]stac$' "$work/smap"
        grep -Eq '[[:space:]]clac$' "$work/smap"
        [[ $(awk '$2 ~ /^j[a-z]+$/ && $2 != "jmp" { n++ } END { print n+0 }' "$work/smap") == 1 ]]
        objdump --no-show-raw-insn -d --disassemble=smap_range "$work/smap.o" > "$work/smap"
        grep -Eq '[[:space:]]stac$' "$work/smap"
        grep -Eq '[[:space:]]clac$' "$work/smap"
        if grep -Eq '[[:space:]]callq?[[:space:]]' "$work/smap"; then
            echo "an out-of-line range check runs with user access enabled" >&2
            exit 1
        fi
    fi
    if [[ ${file##*/} == inline_known_callee.c ]]; then
        "$FCC" -O2 --emit-fir "$file" -o "$work/callees.fir"
        sed -n '/^func @known_callback_path /,/^}/p' "$work/callees.fir" > "$work/callbacks.fir"
        grep -q '^func @known_callback_path ' "$work/callbacks.fir"
        if grep -Eq ' = call ' "$work/callbacks.fir"; then
            echo "the known always_inline callback was not inlined" >&2
            exit 1
        fi
        sed -n '/^func @mutable_callback_path /,/^}/p' "$work/callees.fir" > "$work/callbacks.fir"
        grep -Eq ' = call .* %v[0-9]+\(' "$work/callbacks.fir"
        "$FCC" -O2 -mretpoline-external-thunk -c "$file" -o "$work/static-call.o"
        objdump -dr --disassemble=static_call_path "$work/static-call.o" > "$work/static-call"
        grep -Eq 'R_X86_64_PLT32[[:space:]]+__SCT__step([+-]|$)' "$work/static-call"
        if grep -q '__x86_indirect_thunk_' "$work/static-call"; then
            echo "the static call uses an indirect thunk" >&2
            exit 1
        fi
    fi
    if [[ ${file##*/} == noreturn_control_flow.c ]]; then
        "$FCC" "${flags[@]}" --emit-fir "$file" -o "$work/noreturn.fir"
        awk '
            /call noreturn .* @terminate(_again)?\(/ {
                if ((getline) != 1 || $0 !~ /^  unreachable$/) exit 1
                count++
            }
            END { if (count != 2) exit 1 }
        ' "$work/noreturn.fir"
        sed -n '/^func @dispatch /,/^}/p' "$work/noreturn.fir" > "$work/dispatch.fir"
        grep -q 'asm volatile "ud2"' "$work/dispatch.fir"
        grep -q '^  unreachable$' "$work/dispatch.fir"
        [[ $(grep -c '^  indirect_br ' "$work/dispatch.fir") == 1 ]]
        if grep -q '^  br ' "$work/dispatch.fir"; then
            echo "a conditional backedge survived do ... while (0)" >&2
            exit 1
        fi
        for exe in "$work/clang" "$work/fcc"; do
            status=0
            "$exe" exit || status=$?
            [[ $status == 19 ]]
            status=0
            "$exe" exit again || status=$?
            [[ $status == 20 ]]
        done
    fi
    if [[ ${file##*/} == noreturn_wrappers.c ]]; then
        for wrapper in stop_local stop_wrapper still_returns; do
            objdump --no-show-raw-insn -d --disassemble="$wrapper" "$work/fcc" > "$work/wrapper"
            expected=1
            if [[ $wrapper == still_returns ]]; then expected=2; fi
            awk -v expected="$expected" '
                $2 == "call" && /<stop_/ {
                    if ((getline) != 1 || $2 != "ud2") exit 1
                    count++
                }
                END { if (count != expected) exit 1 }
            ' "$work/wrapper"
        done
        for exe in "$work/clang" "$work/fcc"; do
            status=0
            "$exe" exit || status=$?
            [[ $status == 29 ]]
        done
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
