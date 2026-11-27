#include <stdio.h>
#include <stdlib.h>

static __attribute__((noinline, noreturn)) long stop_process(int status)
{
    _Exit(status);
}

static __attribute__((noinline)) long stop_local(int status)
{
    asm volatile("" ::: "memory");
    return stop_process(status);
}

static __attribute__((noinline)) long stop_wrapper(int status)
{
    return stop_local(status);
}

static __attribute__((noinline)) long stop_trap(void)
{
    asm volatile("ud2");
    __builtin_unreachable();
}

static __attribute__((noinline)) long still_returns(int argc)
{
    if (argc > 2)
        return stop_trap();
    if (argc > 1)
        return stop_wrapper(27 + argc);
    return 17;
}

int main(int argc, char **argv)
{
    (void)argv;
    printf("%ld\n", still_returns(argc));
    return 0;
}
