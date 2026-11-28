#include <float.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

static const float f = -FLT_SNAN;
static const double d = (double)DBL_SNAN;
static const long double ld = 1 ? -LDBL_SNAN : 0.0L;
static const _Float16 h = __builtin_nansf16("");

int main(void) {
    uint16_t half;
    uint32_t single;
    uint64_t dbl, mantissa;
    uint16_t sign_exp;
    memcpy(&half, &h, sizeof(half));
    memcpy(&single, &f, sizeof(single));
    memcpy(&dbl, &d, sizeof(dbl));
    memcpy(&mantissa, &ld, sizeof(mantissa));
    memcpy(&sign_exp, (const char *)&ld + 8, sizeof(sign_exp));
    printf("%04x %08x %016llx %04x %016llx\n", (unsigned)half, (unsigned)single,
        (unsigned long long)dbl, (unsigned)sign_exp, (unsigned long long)mantissa);
}
