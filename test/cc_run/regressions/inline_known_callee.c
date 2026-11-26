#include <stdio.h>

typedef int (*callback)(int);

static inline __attribute__((always_inline)) int apply(callback fn, int x)
{
    return (*fn)(x);
}

static inline __attribute__((always_inline)) int required_step(int x)
{
    return x * 5 + 7;
}

static int other_step(int x)
{
    return x - 2;
}

int known_callback_path(int x)
{
    return apply(&required_step, x);
}

static inline __attribute__((always_inline)) int apply_mutable(callback fn, int x, int change)
{
    if (change)
        fn = &other_step;
    return fn(x);
}

int mutable_callback_path(int x, int change)
{
    return apply_mutable(&required_step, x, change);
}

static inline __attribute__((always_inline)) int countdown(int x)
{
    return x ? 1 + apply(&countdown, x - 1) : 0;
}

static int entered;

__attribute__((noinline, section(".static_call.text"))) int __SCT__step(int x)
{
    return x + entered;
}

int static_call_path(int x)
{
    return ({ ++entered; &__SCT__step; })(x);
}

int main(void)
{
    int a = static_call_path(10);
    int b = static_call_path(10);
    printf("%d %d %d %d %d %d\n", known_callback_path(10),
           mutable_callback_path(10, 0), mutable_callback_path(10, 1), a, b,
           countdown(a));
}
