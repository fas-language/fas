#include <stdio.h>
typedef int pair __attribute__((vector_size(8)));
__attribute__((ms_abi,noinline)) static void check(int tag, ...) {
    __builtin_ms_va_list ap;
    __builtin_ms_va_start(ap, tag);
    pair x = __builtin_va_arg(ap, pair);
    __builtin_ms_va_end(ap);
    printf("%d %d\n", x[0], x[1]);
}
int main(void) { check(0, (pair){17,19}); }
