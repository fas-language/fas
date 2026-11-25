#!/usr/bin/env bash
set -euo pipefail
ulimit -f 524288
ulimit -c 0

clang=${CLANG:-clang-22}
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
cd "$work"

run_diff() {
    "$clang" -std=gnu23 "$@" -o clang.out
    "$FCC" "$@" -o fcc.out
    ./clang.out > clang.txt
    ./fcc.out > fcc.txt
    cmp clang.txt fcc.txt
}

cat > types.c <<'C'
#include <stdio.h>
enum small { A = 0, B = 255 };
enum signed_short { C = -129, D = 127 };
enum fixed : unsigned long { E = 0 };
static int byte(char x) { return x; }
int main(void) {
    char c = (char)-1;
    printf("%d %d %zu %zu %zu\n", byte(c), __builtin_types_compatible_p(char, unsigned char),
           sizeof(enum small), sizeof(enum signed_short), sizeof(enum fixed));
}
C
run_diff -funsigned-char -fshort-enums types.c
run_diff -funsigned-char -fsigned-char -fshort-enums -fno-short-enums types.c
echo "PASS: C char signedness, enum layout, and option overrides"

cat > wchar.c <<'C'
#include <stddef.h>
static_assert(sizeof(wchar_t) == 4);
static_assert(_Generic(L'x', int: 1, default: 0));
static_assert(sizeof(L"x") == 8);
C
"$clang" -std=gnu23 -fshort-wchar -fno-short-wchar -fsyntax-only wchar.c
"$FCC" -fshort-wchar -fno-short-wchar -fsyntax-only wchar.c
echo "PASS: override short wchar mode"

cat > standard.c <<'C'
#include <stddef.h>
#if __STDC_VERSION__ < 202311L
int bool = 21, true = 7, constexpr = 5, nullptr = 9, thread_local = 3;
#else
bool b = true;
static_assert(true);
nullptr_t pn = nullptr;
#endif
int main(void) { return NULL != (void *)0; }
C
for standard in c99 c11 c17 c23 gnu99 gnu11 gnu17 gnu23; do
    "$clang" "-std=$standard" -fsyntax-only standard.c
    "$FCC" "-std=$standard" -fsyntax-only standard.c
done
echo "PASS: C language modes and keyword spelling"

cat > macros.c <<'C'
#if __has_builtin(memcpy)
builtin_memcpy
#endif
#if __has_builtin(strlen)
builtin_strlen
#endif
#if __has_builtin(__builtin_memcpy)
explicit_builtin_memcpy
#endif
#ifdef __CHAR_UNSIGNED__
char_unsigned
#endif
__STDC_VERSION__ __STDC_HOSTED__
#ifdef __STRICT_ANSI__
strict
#endif
#ifdef __PIE__
pie __PIE__
#endif
#ifdef __PIC__
pic __PIC__
#endif
#ifdef __SSP_STRONG__
ssp_strong __SSP_STRONG__
#endif
#ifdef __AVX2__
avx2
#endif
#ifdef __SSE4_2__
sse42
#endif
#ifdef __SSE2__
sse2
#endif
#ifdef __SSE__
sse
#endif
#ifdef __BMI2__
bmi2
#endif
C
macro_diff() {
    "$clang" -std=gnu23 -E -P "$@" macros.c | sed '/^[[:space:]]*$/d' > clang.txt
    "$FCC" -E -P "$@" macros.c | sed '/^[[:space:]]*$/d' > fcc.txt
    diff -u clang.txt fcc.txt
}
macro_diff -std=c99 -fpie -funsigned-char -fstack-protector-strong
macro_diff -std=c11 -fpic -fno-builtin-memcpy
macro_diff -std=c17 -fno-pic -fno-builtin
macro_diff -std=gnu17 -ffreestanding
macro_diff -march=x86-64-v3 -mno-avx2
macro_diff -march=native
macro_diff -march=native -mno-avx2
macro_diff -mavx2
macro_diff -msse4.2
macro_diff -mno-sse
echo "PASS: predefined macros, libc builtin availability, and x86 features"

cat > intrinsic.c <<'C'
typedef char bytes32 __attribute__((vector_size(32)));
#ifdef WITH_ATTRIBUTE
__attribute__((target("avx2")))
#endif
int mask(void) {
    bytes32 a = {-1, 0, -1, 0};
    return __builtin_ia32_pmovmskb256(a);
}
int main(void) { return mask() != 5; }
C
"$clang" -std=gnu23 -mavx2 -c intrinsic.c -o clang.o
"$FCC" -mavx2 -c intrinsic.c -o fcc.o
if grep -qw avx2 /proc/cpuinfo; then
    run_diff -mavx2 intrinsic.c
    run_diff -DWITH_ATTRIBUTE intrinsic.c
else
    "$clang" -std=gnu23 -DWITH_ATTRIBUTE -c intrinsic.c -o clang.o
    "$FCC" -DWITH_ATTRIBUTE -c intrinsic.c -o fcc.o
fi
if "$FCC" -mavx2 -mno-avx2 -c intrinsic.c -o fcc.o 2> fcc.err; then
    echo "FAIL: AVX2 intrinsic accepted after disabling AVX2" >&2
    exit 1
fi
grep -qF "intrinsic requires x86 target feature 'avx2'" fcc.err
echo "PASS: intrinsic target feature checking and GNU target attribute"

cat > wrap.c <<'C'
#include <limits.h>
#include <stdio.h>
_Static_assert(INT_MAX + 1 == INT_MIN, "signed wrap");
int main(void) {
    volatile int a = INT_MAX;
    printf("%d\n", a + 1);
}
C
run_diff -fwrapv -Wno-integer-overflow wrap.c
cat > trap.c <<'C'
#include <limits.h>
int main(int argc, char **argv) {
    volatile int a = argc == 1 ? INT_MAX : 6;
    volatile int b = a + 1;
    return b == 7 ? 0 : 1;
}
C
"$clang" -std=gnu23 -ftrapv trap.c -o clang.out
"$FCC" -ftrapv trap.c -o fcc.out
python3 - <<'PY'
import signal
import subprocess
for name in ("clang", "fcc"):
    assert subprocess.run([f"./{name}.out", "safe"]).returncode == 0
    assert subprocess.run([f"./{name}.out"]).returncode == -signal.SIGILL
PY
echo "PASS: signed wrap and runtime trap"

cat > common_a.c <<'C'
int merged[8];
int initialized = 19;
int read_array(void) { return merged[7] + initialized; }
C
cat > common_b.c <<'C'
int merged[16];
int initialized;
int read_array(void);
int main(void) {
    merged[7] = 5;
    return read_array() != 24;
}
C
run_diff -fcommon common_a.c common_b.c
for compiler in "$clang" "$FCC"; do
    "$compiler" -fcommon -fno-common -c common_a.c -o strong.o
    if LC_ALL=C readelf -sW strong.o | grep -E ' COM .* merged$'; then
        echo "FAIL: -fno-common left a common symbol" >&2
        exit 1
    fi
done
echo "PASS: ELF common resolution and -fno-common override"

cat > visibility.c <<'C'
int data = 3;
int hidden_by_default(void) { return data; }
extern int private_to_host(void);
static inline int unused(void) { return private_to_host(); }
static inline int used_inline(void) { return hidden_by_default(); }
static inline int used_through_alias(void) { return 5; }
__attribute__((alias("used_through_alias")))
__attribute__((visibility("default"))) int alias_to_inline(void);
__attribute__((visibility("default"))) int exported(void) { return used_inline(); }
C
for compiler in "$clang" "$FCC"; do
    "$compiler" -fPIC -fvisibility=hidden -shared visibility.c -o visibility.so
    readelf --dyn-syms -W visibility.so > symbols.txt
    grep -qE ' GLOBAL +DEFAULT .* exported$' symbols.txt
    grep -qE ' GLOBAL +DEFAULT .* alias_to_inline$' symbols.txt
    if grep -Eq ' (hidden_by_default|data|private_to_host)$' symbols.txt; then
        echo "FAIL: private definitions exported from visibility.so" >&2
        exit 1
    fi
done
echo "PASS: default visibility and explicit symbol export"

cat > pic.c <<'C'
extern int external;
int address(void) { return external; }
C
for compiler in "$clang" "$FCC"; do
    "$compiler" -fPIC -c pic.c -o pic.o
    readelf -rW pic.o > relocations.txt
    grep -qE 'GOTPCREL.* external ' relocations.txt
    "$compiler" -fPIC -fno-pic -c pic.c -o pic.o
    readelf -rW pic.o > relocations.txt
    grep -qE 'R_X86_64_(PC32|32S?).* external ' relocations.txt
done
echo "PASS: position-independent and position-dependent ELF relocations"

cat > guard.c <<'C'
int guarded(int x) { volatile char bytes[12]; bytes[0] = x; return bytes[0]; }
int plain(int x) { return x; }
__attribute__((no_stack_protector))
int unprotected(int x) { volatile char bytes[12]; bytes[0] = x; return bytes[0]; }
int main(void) { return guarded(7) != plain(7) || unprotected(14) != 14; }
C
for mode in -fstack-protector -fstack-protector-strong -fstack-protector-all; do
    run_diff "$mode" guard.c
    for bin in clang.out fcc.out; do
        objdump -d --disassemble=guarded "$bin" | grep -q '__stack_chk_fail'
        if objdump -d --disassemble=unprotected "$bin" | grep -q '__stack_chk_fail'; then
            echo "FAIL: no_stack_protector was ignored" >&2
            exit 1
        fi
        if [[ $mode == -fstack-protector-all ]]; then
            objdump -d --disassemble=plain "$bin" | grep -q '__stack_chk_fail'
        fi
    done
done
echo "PASS: stack guards and no_stack_protector"

cat > explicit.c <<'C'
#if __SSP_EXPLICIT__ != 4
#error wrong stack protector mode
#endif
__attribute__((stack_protect))
int marked(int x) { return x; }
int unmarked(int x) { volatile char bytes[16]; bytes[0] = x; return bytes[0]; }
__attribute__((no_stack_protector))
int opted_out(int x) { volatile char bytes[16]; bytes[0] = x; return bytes[0]; }
int main(void) { return marked(7) != 7 || unmarked(14) != 14 || opted_out(9) != 9; }
C
# GCC defines the explicit mode and the stack_protect attribute.
for compiler in "${GCC:-gcc}" "$FCC"; do
    "$compiler" -fstack-protector-explicit explicit.c -o explicit.out
    ./explicit.out
    objdump -d --disassemble=marked explicit.out | grep -q '__stack_chk_fail'
    for function in unmarked opted_out; do
        if objdump -d --disassemble="$function" explicit.out | grep -q '__stack_chk_fail'; then
            echo "FAIL: unexpected stack guard in $function" >&2
            exit 1
        fi
    done
done
echo "PASS: explicit stack guards"

cat > bare.c <<'C'
#include <flag-test-header.h>
void _start(void) { __asm__ volatile("syscall" : : "a"(60L), "D"(status) : "rcx", "r11", "memory"); __builtin_unreachable(); }
C
mkdir include
echo 'static const long status = 0;' > include/flag-test-header.h
run_diff -nostdinc -nostdlib -ffreestanding -Iinclude bare.c
printf '#include <stdio.h>\n' > include.c
if "$FCC" -nostdinc -E include.c > /dev/null 2> fcc.err; then
    echo "FAIL: -nostdinc searched a system include directory" >&2
    exit 1
fi
for mode in -m16 -m32 -mx32; do
    if "$FCC" -fsyntax-only "$mode" bare.c 2> fcc.err; then exit 1; fi
    grep -qF 'only the x86-64 LP64 target is supported' fcc.err
done
if "$FCC" -fsyntax-only -ffast-math bare.c 2> fcc.err; then exit 1; fi
grep -qF 'fast-math semantics are unsupported' fcc.err
"$FCC" -fsyntax-only -Iinclude -fmade-up -mno-made-up bare.c 2> fcc.err
grep -qF "warning: unsupported option '-fmade-up'" fcc.err
grep -qF "warning: unsupported option '-mno-made-up'" fcc.err
echo "PASS: freestanding link, include paths, and command-line diagnostics"

# Kbuild uses an attached language switch, preprocessor depfiles and -r links.
mkdir -p kbuild/system
echo '#define SUM(head, tail...) ((head) + (tail))' > kbuild/system/args.h
cat > kbuild/main.input <<'C'
#include <args.h>
int value(void);
int main(void) { return __extension__ ({ int n = value(); SUM(n, 2); }) != 42; }
C
echo 'int value(void) { return 40; }' > kbuild/value.c
for compiler in "${CLANG:-clang-22}" "$FCC"; do
    "$compiler" -std=c11 -isystem kbuild/system -xc -Wp,-MD,kbuild/main.d \
        -c kbuild/main.input -o kbuild/main.o
    grep -qF 'kbuild/main.input' kbuild/main.d
    grep -qF 'kbuild/system/args.h' kbuild/main.d
    rm kbuild/main.d
    "$compiler" -c kbuild/value.c -o kbuild/value.o
    "$compiler" -r -no-pie kbuild/main.o kbuild/value.o -o kbuild/partial.o
    readelf -h kbuild/partial.o | grep -qE 'Type:[[:space:]]+REL'
    "$compiler" kbuild/partial.o -o kbuild/partial
    ./kbuild/partial
done
echo "PASS: attached language, system macro, dependency file and relocatable link"

# The archives need rescanning in order: main -> A -> B -> A.
echo 'int f(void); int main(void) { return f() != 26; }' > kbuild/start.c
echo 'int g(void); int f(void) { return g() + 6; }' > kbuild/a.c
echo 'int h(void); int g(void) { return h() + 4; }' > kbuild/b.c
echo 'int h(void) { return 16; }' > kbuild/end.c
for compiler in "${CLANG:-clang-22}" "$FCC"; do
    for file in a b end; do
        "$compiler" -c "kbuild/$file.c" -o "kbuild/$file.o"
    done
    ar rcs kbuild/a.a kbuild/a.o kbuild/end.o
    ar rcs kbuild/b.a kbuild/b.o
    "$compiler" kbuild/start.c -Wl,--start-group kbuild/a.a kbuild/b.a \
        -Wl,--end-group -o kbuild/grouped
    ./kbuild/grouped
done
echo "PASS: archive group option ordering"

# The high-half link must resolve ordinary external code and data references.
cat > kbuild/kernel-model.c <<'C'
#if !__code_model_kernel__ || defined(__code_model_small__) || defined(__PIC__)
#error wrong kernel code-model macros
#endif
extern int data;
extern int next(int);
int entry(void) { return next(data); }
/* Early boot code runs from the identity map: direct and constant-offset
 * accesses must be rip-relative, while pointer values stay absolute. */
extern int early_flag, early_table[];
extern void take(int *);
int read_flag(void) { return early_flag; }
int read_slot(void) { return early_table[3]; }
void pass_flag(void) { take(&early_flag); }
C
cat > kbuild/kernel-data.c <<'C'
int data = 42;
int next(int n) { return n + 1; }
int early_flag, early_table[4];
void take(int *p) { (void)p; }
C
for compiler in "${CLANG:-clang-22}" "$FCC"; do
    "$compiler" -Werror -ffreestanding -mno-red-zone -mcmodel=kernel -fno-pie \
        -c kbuild/kernel-model.c -o kbuild/kernel-model.o
    "$compiler" -ffreestanding -mno-red-zone -mcmodel=kernel -fno-pie \
        -c kbuild/kernel-data.c -o kbuild/kernel-data.o
    ld.lld-22 -e entry -Ttext 0xffffffff81000000 \
        kbuild/kernel-model.o kbuild/kernel-data.o -o kbuild/high-half
    readelf -sW kbuild/high-half > kbuild/high-half.symbols
    grep -qE 'ffffffff81000000 .* entry$' kbuild/high-half.symbols
    readelf -rW kbuild/kernel-model.o > kbuild/kernel-model.relocs
    grep -qE 'R_X86_64_PC32 .* early_flag - 4$' kbuild/kernel-model.relocs
    grep -qE 'R_X86_64_PC32 .* early_table \+ 8$' kbuild/kernel-model.relocs
    grep -qE 'R_X86_64_32S .* early_flag \+ 0$' kbuild/kernel-model.relocs
done
echo 'PASS: freestanding high-half kernel code model'

cat > unwind_a.c <<'C'
int unwind_a(int x) { return x + 1; }
C
cat > unwind_b.c <<'C'
int unwind_b(int x) { return x - 1; }
C
for compiler in "$clang" "$FCC"; do
    "$compiler" -fno-asynchronous-unwind-tables -c unwind_a.c -o unwind.o
    readelf -SW unwind.o > unwind.sections
    if grep -qE ' \.eh_frame ' unwind.sections; then
        echo "$compiler emitted unwind tables with asynchronous unwind tables disabled" >&2
        exit 1
    fi
    "$compiler" -fno-asynchronous-unwind-tables -funwind-tables -c unwind_a.c -o unwind.o
    readelf -SW unwind.o > unwind.sections
    grep -qE ' \.eh_frame ' unwind.sections
    "$compiler" -fno-asynchronous-unwind-tables -funwind-tables -fno-unwind-tables -c unwind_a.c -o unwind.o
    readelf -SW unwind.o > unwind.sections
    if grep -qE ' \.eh_frame ' unwind.sections; then
        echo "$compiler emitted unwind tables after -fno-unwind-tables" >&2
        exit 1
    fi
    # On x86-64 the asynchronous default is independent of -fno-unwind-tables.
    "$compiler" -fno-unwind-tables -c unwind_a.c -o unwind.o
    readelf -SW unwind.o > unwind.sections
    grep -qE ' \.eh_frame ' unwind.sections
    "$compiler" -fno-asynchronous-unwind-tables -fasynchronous-unwind-tables -c unwind_a.c -o unwind.o
    readelf -SW unwind.o > unwind.sections
    grep -qE ' \.eh_frame ' unwind.sections
    "$compiler" -fPIC -c unwind_a.c -o unwind_a.o
    "$compiler" -fPIC -c unwind_b.c -o unwind_b.o
    readelf --debug-dump=frames unwind_a.o > unwind.frames
    if grep -q 'ZERO terminator' unwind.frames; then
        echo "$compiler put an unwind terminator in a relocatable object" >&2
        exit 1
    fi
    for linker in ld.bfd ld.lld-22; do
        "$linker" -r unwind_a.o unwind_b.o -o unwind_partial.o
        "$linker" -shared unwind_partial.o -o unwind.so
        readelf --debug-dump=frames unwind.so > unwind.frames
        if [[ $(grep -c 'FDE cie=' unwind.frames) != 2 ]]; then
            echo "$compiler and $linker lost unwind entries after a partial link" >&2
            exit 1
        fi
    done
done
echo 'PASS: unwind-table flags and partial links'
