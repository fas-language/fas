#include <stdio.h>

static long probe(long x, long y)
{
    register long r10 __asm__("r10") = x;
    register long r8 __asm__("r8") = y;
    __asm__ volatile ("addq %%r8, %%r10" : "+r"(r10) : "r"(r8) : "cc");
    return r10;
}

int main(void)
{
    printf("%ld\n", probe(120, 23));
}
