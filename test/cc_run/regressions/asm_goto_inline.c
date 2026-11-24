#include <stdio.h>

static unsigned char bits[] = { 0x80, 0x02 };

static __attribute__((always_inline)) inline int test_bit(unsigned short bit)
{
    asm goto ("testb %[mask], %c[where](%%rip); jnz %l[set]"
              : : [mask] "i"(1 << (bit & 7)),
                  [where] "i"(&bits[bit >> 3])
              : "cc" : set);
    return 0;
set:
    return 1;
}

static __attribute__((always_inline)) inline int test_next(unsigned short bit, int *count)
{
    ++*count;
    return test_bit(bit + 1);
}

static __attribute__((always_inline)) inline void *rip_address(void *p)
{
    asm("leaq %c1(%%rip), %0" : "=r"(p) : "i"(p));
    return p;
}

static int check_address(void)
{
    void *local = &bits[1];
    return rip_address(local) == &bits[1];
}

__attribute__((noinline)) static int check_dynamic_bit(int bit)
{
    return __builtin_constant_p(bit) ? test_bit((unsigned short)bit) : bit == 1;
}

int main(void)
{
    int count = 0;
    int a = test_bit(7);
    int b = test_bit(8);
    int c = test_next(8, &count);
    printf("%d %d %d %d\n", a, b, c, count);
    return !(a == 1 && b == 0 && c == 1 && count == 1 && check_address() && check_dynamic_bit(count));
}
