#include <stdio.h>

#define ORDER(N) ((N) < 2 ? 0 : 64 - __builtin_clzll((N) - 1))
static_assert(ORDER(64) == 6);
static_assert(__builtin_clz(1) == 31);
static_assert(__builtin_clzl(1ul << 53) == 10);

int main(void)
{
    unsigned value;
    asm("movl %1, %0" : "=r"(value) : "i"(ORDER(64)));
    printf("%u\n", value);
    return value != 6;
}
