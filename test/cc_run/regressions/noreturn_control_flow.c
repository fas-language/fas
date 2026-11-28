#include <stdio.h>
#include <stdlib.h>

extern int after_unreachable(void);

static __attribute__((noinline, noreturn)) void terminate(int status)
{
    _Exit(status);
}

static _Noreturn __attribute__((noinline)) void terminate_again(void)
{
    _Exit(20);
}

static int call_or_continue(int mode)
{
    if (mode == 0)
        goto resume;
    if (mode == 1)
        return (terminate(19), after_unreachable());
    terminate_again();
resume:
    return 17;
}

static __attribute__((no_stack_protector)) int dispatch(int fail)
{
    static void *const labels[] = { &&resume, &&trap };
    goto *labels[fail];

trap:
    do {
        asm volatile("ud2");
    } while (0);
    goto *(__builtin_unreachable(), after_unreachable(), &&resume);
resume:
    return 23;
}

int main(int argc, char **argv)
{
    (void)argv;
    printf("%d %d\n", call_or_continue(argc - 1), dispatch(0));
}
