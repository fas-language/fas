#include <stdbool.h>
#include <stdio.h>

static inline __attribute__((always_inline)) bool access_begin(int allowed)
{
    if (!allowed)
        return false;
#ifdef CHECK_SMAP
    asm volatile("stac" ::: "memory");
#else
    asm volatile("" ::: "memory");
#endif
    return true;
}

__attribute__((noinline)) int smap_guard(int allowed)
{
    if (!access_begin(allowed))
        return -1;
#ifdef CHECK_SMAP
    asm volatile("clac" ::: "memory");
#else
    asm volatile("" ::: "memory");
#endif
    return 7;
}

#define address_limit() ({ \
    unsigned long limit; \
    asm("mov %1,%0\n1:\n" \
        ".pushsection .fcc_smap_limit, \"a\"\n" \
        ".long 1b - %c2 - .\n" \
        ".popsection" : "=r"(limit) : "i"(4096UL), "i"(sizeof(long))); \
    limit; \
})

static inline bool range_ok(const void *address, unsigned long size)
{
    if (__builtin_constant_p(size <= (1UL << 4)) && size <= (1UL << 4))
        return (unsigned long)address <= address_limit();
    unsigned long end = (unsigned long)address + size;
    return end <= address_limit() && end >= (unsigned long)address;
}

__attribute__((noinline)) int smap_range(int allowed, unsigned long address)
{
    if (!access_begin(allowed))
        return -1;
    bool ok = range_ok((const void *)address, 8);
#ifdef CHECK_SMAP
    asm volatile("clac" ::: "memory");
#else
    asm volatile("" ::: "memory");
#endif
    return ok ? 7 : -2;
}

#ifndef CHECK_SMAP
static volatile int checks;

static int branch_with_effect(int allowed)
{
    bool enabled = access_begin(allowed);
    if (++checks, !enabled)
        return -1;
    return 23;
}

int main(void)
{
    int a = branch_with_effect(0);
    int b = branch_with_effect(1);
    printf("%d %d %d %d %d\n", smap_guard(0), smap_guard(1), a, b, checks);
    printf("%d %d %d %d\n", smap_range(0, 0), smap_range(1, 9),
           smap_range(1, 5000), smap_range(1, -2UL));
}
#endif
