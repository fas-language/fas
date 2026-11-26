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
}
#endif
