#include <fenv.h>
#include <stdio.h>

// A static initializer uses the translation-time rounding mode. The same
// expression in a function must see the mode set by the running program.
static const double translated = 1.0 + 0x1p-53;

static_assert(_Generic(__builtin_nextafter(1, 2), double: 1, default: 0));
static_assert(_Generic(__builtin_nextafterf(1, 2), float: 1, default: 0));
static_assert(_Generic(__builtin_nextafterl(1, 2), long double: 1, default: 0));
static_assert(_Generic(__builtin_fma(1, 2, 3), double: 1, default: 0));

int main(void)
{
    if (fesetround(FE_UPWARD) || feclearexcept(FE_ALL_EXCEPT))
        return 1;
    volatile double rounded = 1.0 + 0x1p-53;
    int inexact = !!fetestexcept(FE_INEXACT);
    double next = __builtin_nextafter(1, 2);
    float nextf = __builtin_nextafterf(1, 2);
    long double nextl = __builtin_nextafterl(1, 2);
    double fused = -__builtin_fma(-1.0, 1.0, -0x1p-53);
    printf("%a %a %a %La inexact=%d\n", rounded, next, nextf, nextl, inexact);
    return translated != 1.0 || rounded != next || nextf != 0x1.000002p0f ||
           nextl != 0x1.0000000000000002p0L || fused != 1.0 || !inexact;
}
