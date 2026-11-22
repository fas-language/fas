#include "calls.h"

MS double mixed(int a, double b, int c, float d, int e, double f) {
    return a + 2*b + 3*c + 4*d + 5*e + 6*f;
}

MS struct tiny tiny(struct tiny s) { return (struct tiny){s.byte + 1}; }
MS struct halfword halfword(struct halfword s) { return (struct halfword){s.value + 2}; }
MS union word word(union word w) { return (union word){.f = w.f + 3.0f}; }

MS struct small small(struct small a, struct small b, struct small c,
                      struct small d, struct small e) {
    return (struct small){a.x + b.y + c.x, d.y + e.x};
}

MS struct large large(float a, struct large b, int c, double d, int e) {
    b.values[0] += (long)a + c;
    b.values[1] += (long)d;
    b.values[2] += e;
    return b;
}

MS struct aligned aligned(struct aligned a) {
    return (struct aligned){a.byte + 1};
}

MS long double extended(long double x, int y) { return x * 2 + y; }
MS vec4 vector(vec4 x) { return x + 3; }
MS vec2 short_vector(vec2 x) { return x + 7; }
MS __int128 wide(__int128 x) { return x + 2; }
MS __float128 quad(__float128 x) { return x + 3; }

/* A SysV helper can consume an explicitly passed Microsoft cursor. */
static long read_long(__builtin_ms_va_list ap) {
    return __builtin_va_arg(ap, long);
}

MS struct large varargs(int fixed, ...) {
    __builtin_ms_va_list ap, copy;
    __builtin_ms_va_start(ap, fixed);
    __builtin_ms_va_copy(copy, ap);
    double x = __builtin_va_arg(ap, double);
    double again = __builtin_va_arg(copy, double);
    struct large result = __builtin_va_arg(ap, struct large);
    struct small s = __builtin_va_arg(ap, struct small);
    long double y = __builtin_va_arg(ap, long double);
    result.values[0] += (long)(x + again + s.x) + fixed;
    result.values[1] += (long)(y + s.y);
    result.values[2] += read_long(ap);
    __builtin_ms_va_end(ap);
    __builtin_ms_va_end(copy);
    return result;
}

MS double stack_varargs(int a, int b, int c, int d, int e, ...) {
    __builtin_ms_va_list ap;
    __builtin_ms_va_start(ap, e);
    double x = __builtin_va_arg(ap, double);
    __builtin_ms_va_end(ap);
    return a + b + c + d + e + x;
}

MS int preserve(void) {
    clobber_sysv();
    return 42;
}
