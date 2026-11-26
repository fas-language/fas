#!/usr/bin/env bash
# Compare fcc's i386 front end with Clang, including the real-mode data model.
set -euo pipefail
ulimit -f 524288

root=$(cd -- "$(dirname -- "$0")/.." && pwd)
fcc=${FCC:-$root/_build/fcc}
clang=${CLANG:-clang-22}
export FCC_INCLUDE=${FCC_INCLUDE:-$root/cc/include}
tmp=$(mktemp -d "${TMPDIR:-/tmp}/fcc-i386.XXXXXX")
trap 'rm -rf -- "$tmp"' EXIT

cat > "$tmp/layout.c" <<'C'
#if !defined(__i386__) || !defined(__ILP32__) || defined(__LP64__) || defined(__SIZEOF_INT128__)
#error wrong predefined target
#endif
#include <stddef.h>
#include <stdint.h>
#include <stdarg.h>

#define TYPE(e, t) _Generic((e), t: 1, default: 0)
_Static_assert(sizeof(void *) == 4 && sizeof(long) == 4 && sizeof(long long) == 8, "ILP32 sizes");
_Static_assert(sizeof(long double) == 12 && _Alignof(long double) == 4, "extended precision layout");
_Static_assert(sizeof(va_list) == 4 && TYPE((va_list)0, char *), "stack va_list");
_Static_assert(TYPE(sizeof(int), unsigned int), "sizeof result");
_Static_assert(TYPE((size_t)0, unsigned int), "size_t");
_Static_assert(TYPE((ptrdiff_t)0, int), "ptrdiff_t");
_Static_assert(TYPE((int *)0 - (int *)0, int), "pointer subtraction result");
_Static_assert(TYPE((uint64_t)0, unsigned long long), "uint64_t");
_Static_assert(TYPE(2147483648, long long), "large decimal literal");
_Static_assert(TYPE(0x80000000L, unsigned long), "hex long literal");
_Static_assert(TYPE(0x100000000UL, unsigned long long), "large unsigned literal");
_Static_assert(TYPE(1L + 1U, unsigned long), "usual arithmetic conversions");

struct layout { char a; double b; long long c; void *d; };
_Static_assert(offsetof(struct layout, b) == 4, "double member alignment");
_Static_assert(offsetof(struct layout, c) == 12, "long long member alignment");
_Static_assert(offsetof(struct layout, d) == 20 && sizeof(struct layout) == 24, "record layout");
_Static_assert(TYPE(offsetof(struct layout, c), size_t), "offsetof result");
C

for mode in -m32 -m16; do
    "$clang" "$mode" -march=i386 -std=gnu23 -ffreestanding -fsyntax-only "$tmp/layout.c"
    "$fcc" "$mode" -march=i386 -std=gnu23 -ffreestanding -fsyntax-only "$tmp/layout.c"
done

printf 'fcc i386 and real-mode data model matches Clang\n'

lld=${LLD:-ld.lld-22}
readelf=${READELF:-readelf}
objdump=${OBJDUMP:-objdump}
flags=(-march=i386 -std=gnu23 -ffreestanding -fno-pic -fno-stack-protector
       -fno-asynchronous-unwind-tables -fno-unwind-tables -mno-sse -msoft-float)

cat > "$tmp/impl.c" <<'C'
#include <stdarg.h>
#include <stdint.h>

extern unsigned adjust(unsigned x);
extern const unsigned bias;
const unsigned * const bias_ptr = &bias;

unsigned long update_long(long value, unsigned long mask)
{
    return (unsigned long)(value + adjust(3)) ^ mask;
}

uint64_t product(unsigned a, unsigned b)
{
    uint64_t result;
    __asm__("mull %2" : "=A"(result) : "a"(a), "rm"(b));
    __asm__("" : "=rm"(result) : "0"(result));
    return result;
}

uint64_t mix(uint64_t a, uint64_t b, unsigned shift, unsigned mask)
{
    uint64_t x = (a * b + mask) ^ (b >> shift);
    if (x < a)
        x += (uint64_t)adjust(mask) << (shift & 31);
    return x + *bias_ptr;
}

static unsigned call_with_tail(unsigned (*fn)(unsigned), unsigned x)
{
    return fn(x ^ 3);
}

unsigned dispatch(unsigned (*fn)(unsigned), unsigned x)
{
    return call_with_tail(fn, x) + 7;
}

unsigned collect(unsigned n, ...)
{
    va_list args, copy;
    va_start(args, n);
    va_copy(copy, args);
    unsigned s = 0;
    for (unsigned i = 0; i < n; i++)
        s += va_arg(args, unsigned);
    if (n != 0)
        s += va_arg(copy, unsigned);
    va_end(copy);
    va_end(args);
    return s;
}
C

cat > "$tmp/entry.c" <<'C'
#include <stdint.h>
extern uint64_t mix(uint64_t, uint64_t, unsigned, unsigned);
extern unsigned dispatch(unsigned (*)(unsigned), unsigned);
extern unsigned collect(unsigned, ...);
extern unsigned long update_long(long, unsigned long);
extern uint64_t product(unsigned, unsigned);
extern const unsigned * const bias_ptr;

const unsigned bias = 19;
unsigned adjust(unsigned value) { return value * 5 + 11; }

static uint64_t reference(uint64_t a, uint64_t b, unsigned s, unsigned mask)
{
    uint64_t expected = (a * b + mask) ^ (b >> s);
    return expected + (expected < a ? (uint64_t)(mask * 5 + 11) << (s & 31) : 0) + 19;
}

int run(void)
{
    for (unsigned i = 0; i < 64; i++) {
        uint64_t a = UINT64_C(0xfedcba984634de12) + i;
        uint64_t b = UINT64_C(0x876543212963180f) - i;
        if (mix(a, b, i, i * 9) != reference(a, b, i, i * 9))
            return i + 1;
    }
    unsigned values[] = { 3, 16, 714, 2 };
    if (dispatch(adjust, 145) != adjust(145 ^ 3) + 7)
        return 65;
    if (collect(0) != 0 || collect(4, values[0], values[1], values[2], values[3]) != 738)
        return 66;
    if (bias_ptr != &bias || values + 1 - (values + 3) != -2)
        return 67;
    if (update_long(-32, 0x80000000UL) != 0x7ffffffaUL)
        return 68;
    if (product(0x12345678, 0xf0123456) != UINT64_C(0x11125c7753956c50))
        return 69;
    return 0;
}
C

cat > "$tmp/start.s" <<'ASM'
.text
.globl _start
_start:
    andl $-16, %esp
    call run
    movl %eax, %ebx
    movl $1, %eax
    int $0x80
.section .note.GNU-stack,"",@progbits
ASM

"$clang" -m32 -c "$tmp/start.s" -o "$tmp/start.o"
# Verify execution support with a dedicated exit syscall, separate from test failures.
printf '.text\n.globl _start\n_start: xorl %%ebx, %%ebx; movl $1, %%eax; int $0x80\n' > "$tmp/probe.s"
"$clang" -m32 -c "$tmp/probe.s" -o "$tmp/probe.o"
"$lld" -m elf_i386 -o "$tmp/host-probe" "$tmp/probe.o"
runner=()
if ! "$tmp/host-probe"; then
    runner=("${QEMU_I386:-qemu-i386}")
fi

for regparm in 0 3; do
    options=(-m32 "-mregparm=$regparm" "${flags[@]}")
    "$clang" "${options[@]}" -O2 -c "$tmp/entry.c" -o "$tmp/clang-entry.o"
    "$clang" "${options[@]}" -O2 -c "$tmp/impl.c" -o "$tmp/clang-impl.o"
    "$fcc" "${options[@]}" -c "$tmp/entry.c" -o "$tmp/fcc-entry.o"
    "$fcc" "${options[@]}" -c "$tmp/impl.c" -o "$tmp/fcc-impl.o"
    for pair in 'clang clang' 'clang fcc' 'fcc clang' 'fcc fcc'; do
        read -r entry impl <<< "$pair"
        "$lld" -m elf_i386 -o "$tmp/run" "$tmp/start.o" "$tmp/$entry-entry.o" "$tmp/$impl-impl.o"
        "${runner[@]}" "$tmp/run" || { status=$?; echo "regparm=$regparm, entry=$entry, implementation=$impl: exit $status" >&2; exit "$status"; }
    done
done
printf 'Eight i386 executable comparisons passed (cdecl and regparm=3)\n'

for compiler in "$clang" "$fcc"; do
    "$compiler" -m16 -mstack-alignment=4 -mregparm=3 "${flags[@]}" -c "$tmp/impl.c" -o "$tmp/real.o"
    "$readelf" -h "$tmp/real.o" > "$tmp/real.header"
    "$readelf" -rW "$tmp/real.o" > "$tmp/real.relocs"
    "$objdump" -dr -M i8086 "$tmp/real.o" > "$tmp/real.dis"
    grep -q ELF32 "$tmp/real.header"
    grep -q 'Intel 80386' "$tmp/real.header"
    grep -q 'R_386_32.*bias' "$tmp/real.relocs"
    grep -Eq '\b66 c3[[:space:]]+retl\b' "$tmp/real.dis"
done
printf 'Both -m16 objects use ELF32, i386 relocations, and 32-bit returns\n'

qemu_system=${QEMU_SYSTEM_I386:-qemu-system-i386}
if command -v "$qemu_system" >/dev/null; then
    cat > "$tmp/step.c" <<'C'
unsigned step(unsigned a, unsigned b, unsigned c)
{
    unsigned scaled;
    __asm__("leal (%1,%1,8), %0" : "=r"(scaled) : "r"(a));
    return scaled + b - c;
}
C
    cat > "$tmp/boot.s" <<'ASM'
.section .init,"ax"
.code16gcc
.globl _start
_start:
    cli
    xorw %ax, %ax
    movw %ax, %ds
    movw %ax, %ss
    movl $0x7c00, %esp
    movl $7, %eax
    movl $9, %edx
    movl $5, %ecx
    calll step
    cmpl $67, %eax
    jne fail
    movb $0x21, %al
    jmp exit
fail:
    movb $0x22, %al
exit:
    outb %al, $0xf4
    hlt
.section .note.GNU-stack,"",@progbits
ASM
    cat > "$tmp/boot.lds" <<'LDS'
ENTRY(_start)
SECTIONS {
  . = 0x7c00;
  .text : { *(.init) *(.text) }
  . = 0x7c00 + 510;
  .signature : { SHORT(0xaa55) }
  /DISCARD/ : { *(.note*) }
}
LDS
    "$clang" -m32 -c "$tmp/boot.s" -o "$tmp/boot.o"
    for compiler in "$clang" "$fcc"; do
        "$compiler" -m16 -mstack-alignment=4 -mregparm=3 "${flags[@]}" -c "$tmp/step.c" -o "$tmp/step.o"
        "$lld" -m elf_i386 -T "$tmp/boot.lds" --oformat=binary "$tmp/boot.o" "$tmp/step.o" -o "$tmp/boot.img"
        [ "$(wc -c < "$tmp/boot.img")" -eq 512 ]
        status=0
        timeout 12 "$qemu_system" -m 16 -display none -nodefaults -no-reboot \
            -device isa-debug-exit,iobase=0xf4,iosize=4 -drive "file=$tmp/boot.img,format=raw" || status=$?
        if [ "$status" -ne 67 ]; then
            echo "$compiler: expected QEMU status 67, got $status" >&2
            exit 1
        fi
    done
    printf 'Both -m16 objects executed with regparm=3 under QEMU BIOS\n'
fi
