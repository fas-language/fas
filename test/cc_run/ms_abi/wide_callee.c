#include "wide.h"

MS wide64 wide_stack(wide32 a, double b, int c, int d, wide64 e) {
    LANES(a) += (int)b;
    for (int i = 0; i < 16; ++i)
        LANES(e)[i] += LANES(a)[i % 8] + c * i + d;
    return e;
}

MS wide32 wide_varargs(int fixed, ...) {
    __builtin_ms_va_list ap;
    __builtin_ms_va_start(ap, fixed);
    wide64 a = __builtin_va_arg(ap, wide64);
    wide32 b = __builtin_va_arg(ap, wide32);
    int c = __builtin_va_arg(ap, int);
    int d = __builtin_va_arg(ap, int);
    wide32 e = __builtin_va_arg(ap, wide32);
    __builtin_ms_va_end(ap);
    for (int i = 0; i < 8; ++i)
        LANES(e)[i] += fixed + LANES(a)[15-i] + LANES(b)[i] + c * i + d;
    return e;
}
