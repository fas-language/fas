#include <stdarg.h>

#ifdef __SSE__
#error SSE must be disabled
#endif

__attribute__((noinline))
static long sum(int count, ...)
{
    va_list args;
    va_start(args, count);
    long total = 0;
    for (int k = 0; k < count; k++)
        total += va_arg(args, int);
    va_end(args);
    return total;
}

int main(void)
{
    return sum(8, 1, 2, 3, 4, 5, 6, 7, 8) != 36;
}
