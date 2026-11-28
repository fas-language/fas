#include <stdio.h>

register unsigned long stack_pointer __asm__("rsp");

int main(void)
{
    unsigned long actual, bound;
    __asm__ volatile ("movq %%rsp, %0; movq %2, %1" : "=r"(actual), "=r"(bound) : "r"(stack_pointer));
    if (actual != bound)
        return 1;
    __asm__ volatile ("movq %1, %0" : "=r"(bound), "+r"(stack_pointer));

    // Compare inside the asm because Clang can adjust rsp between statements.
    unsigned char valid;
    __asm__ volatile ("cmpq %%rsp, %1; sete %b0" : "=q"(valid) : "r"(stack_pointer) : "cc");
    if (!valid || !bound)
        return 2;
    puts("stack pointer operands agree");
}
