#include <fenv.h>
#include <math.h>
#include <stdio.h>

int main(void)
{
#pragma STDC FENV_ACCESS ON
    volatile double nan = NAN;
    volatile long double nanl = __builtin_nanl("");
    if (feclearexcept(FE_ALL_EXCEPT))
        return 1;
    // Ordered C operators signal on a quiet NaN. The <math.h> predicate
    // and C equality comparisons are quiet.
    volatile bool result = nan < 0.0;
    int ordered = !!fetestexcept(FE_INVALID);
    feclearexcept(FE_ALL_EXCEPT);
    result |= nanl >= 0.0L;
    int extended = !!fetestexcept(FE_INVALID);
    feclearexcept(FE_ALL_EXCEPT);
    result |= isless(nan, 0.0) || nan == 0.0;
    int quiet = !fetestexcept(FE_INVALID);
    (void)(1.0 / 0.0);
    int divzero = !!fetestexcept(FE_DIVBYZERO);
    printf("ordered=%d extended=%d quiet=%d divzero=%d result=%d\n",
           ordered, extended, quiet, divzero, result);
    return !ordered || !extended || !quiet || !divzero || result;
}
