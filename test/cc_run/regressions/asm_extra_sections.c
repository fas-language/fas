#include <stdio.h>

static int read_record(void)
{
    int value;
    __asm__ volatile (
        "1: leaq 2f(%%rip), %%rax\n\t"
        "movslq (%%rax), %%rdx\n\t"
        "addq %%rax, %%rdx\n\t"
        "leaq 1b(%%rip), %%rax\n\t"
        "cmpq %%rax, %%rdx\n\t"
        "sete %b0\n\t"
        "movzbl %b0, %k0\n\t"
        ".pushsection __ex_table,\"a\"\n\t"
        ".balign 4\n\t"
        "2: .long 1b - .\n\t"
        ".popsection\n\t"
        ".pushsection .altinstructions,\"a\"\n\t"
        ".long 2b - .\n\t"
        ".long 3f - .\n\t"
        ".popsection\n\t"
        ".pushsection .altinstr_replacement,\"ax\"\n\t"
        "3:\n\t"
        ".popsection"
        : "=q"(value) : : "rax", "rdx", "cc");
    return value;
}

int main(void)
{
    if (!read_record())
        return 1;
    puts("inline asm section record points at its instruction");
}
