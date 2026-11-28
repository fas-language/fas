#include <stdio.h>

static struct {
    int prefix;
    int values[3];
} object = { 3, { 11, 22, 33 } };

static void anchor(void) {}

int main(void)
{
    static int local[] = { 7, 13 };
    const int *positive, *negative;
    void (*address)(void);
    __asm__("leaq %c1(%%rip), %0" : "=r"(positive) : "i"(&object.values[2]));
    __asm__("leaq %c1+4(%%rip), %0" : "=r"(negative) : "i"((unsigned long)local - 4));
    __asm__("leaq %c1(%%rip), %0" : "=r"(address) : "i"(anchor));
    if (positive != &object.values[2] || negative != local || address != anchor)
        return 1;
    printf("%d %d\n", *positive, *negative);
}
