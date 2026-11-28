#include <fenv.h>
#include <float.h>
#include <stdio.h>

int main(void)
{
    const int modes[] = {FE_TONEAREST, FE_DOWNWARD, FE_UPWARD, FE_TOWARDZERO};
    const int flt_values[] = {1, 3, 2, 0};
    for (unsigned i = 0; i < sizeof(modes) / sizeof(modes[0]); ++i) {
        if (fesetround(modes[i]) != 0)
            return 1;
        int value = FLT_ROUNDS;
        printf("%d\n", value);
        if (value != flt_values[i])
            return 1;
    }
    return 0;
}
