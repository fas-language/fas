#include <setjmp.h>
#include <stdarg.h>
#include <stdio.h>

static inline __attribute__((always_inline)) int step(int, int);

static int forward(int x)
{
    return (*&step)(x, 3);
}

static inline __attribute__((always_inline)) int step(int x, int y)
{
    static int count;
    return x + y + count++;
}

static int (*volatile escaped)(int, int) = step;

static inline int small(int x)
{
    return x + 4;
}

static inline int recurse(int x)
{
    return x ? x + recurse(x - 1) : 0;
}

static inline __attribute__((always_inline)) int required_step(int);

static inline int optional_step(int x)
{
    return x ? required_step(x - 1) : 0;
}

static inline __attribute__((always_inline)) int required_step(int x)
{
    return x + optional_step(x);
}

static inline int varargs(int x, ...)
{
    va_list ap;
    va_start(ap, x);
    int y = va_arg(ap, int);
    va_end(ap);
    return x + y;
}

static inline int with_vla(int n)
{
    int a[n];
    a[n - 1] = n;
    return a[n - 1] + sizeof a;
}

static inline int with_setjmp(void)
{
    jmp_buf buf;
    volatile int n = 2;
    if (!setjmp(buf)) {
        n = 3;
        longjmp(buf, 1);
    }
    return n;
}

static inline __attribute__((always_inline)) int pointer_home(void *p)
{
    unsigned char result;
    asm("cmpq $0, %1; setne %0" : "=q"(result) : "m"(p) : "cc");
    return result;
}

int main(int argc, char **argv)
{
    int first = forward(argc);
    int nested = step(2, step(3, 4));
    int indirect = escaped(1, 5);
    printf("%d %d %d %d %d %d %d %d %d %d %d\n", first, nested, indirect, small(argc),
           recurse(5), varargs(1, 4), with_vla(2), with_setjmp(), required_step(argc + 2),
           pointer_home(argv), pointer_home(NULL));
    return 0;
}
