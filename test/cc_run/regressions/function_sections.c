#include <stdio.h>

static int ordinary(int n)
{
    return n + 3;
}

__attribute__((section(".fas_" "test_exec2"), noinline))
static int in_other_section(int n)
{
    static int (*record)(int) __attribute__((section(".fas_" "test_init_" "record"))) = ordinary;
    typedef unsigned int vector __attribute__((vector_size(32)));
    vector lanes = { record(n), 1, 2, 3, 4, 5, 6, 7 };
    vector doubled = lanes + lanes;
    unsigned _BitInt(192) wide = doubled[0];
    return (int)(((wide << 100) + doubled[1]) >> 100);
}

__attribute__((section(".fas_test_" "exec1"), noinline))
static int in_executable_section(int n)
{
    int same_label;
    asm("1: leaq 2f(%%rip), %%rax\n"
        "movslq (%%rax), %%rdx\n"
        "addq %%rax, %%rdx\n"
        "leaq 1b(%%rip), %%rax\n"
        "cmpq %%rax, %%rdx\n"
        "sete %b0\n"
        "movzbl %b0, %k0\n"
        ".pushsection .fas_test_record,\"a\"\n"
        "2: .long 1b - .\n"
        ".popsection"
        : "=q"(same_label) : : "rax", "rdx", "cc");
    return same_label ? in_other_section(n) : 0;
}

int main(void)
{
    int actual = in_executable_section(18);
    printf("%d\n", actual);
    return actual != 42;
}
