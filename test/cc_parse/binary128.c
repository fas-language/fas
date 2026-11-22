typedef __float128 Q;
static_assert(sizeof(Q) == 16 && alignof(Q) == 16);
static_assert(!__builtin_types_compatible_p(Q, long double));
static Q min = 0x1p-16494Q;
static Q huge = 0x1.ffffffffffffffffffffffffffffp+16383Q;
static Q small = -0x1.0000000000000000000000000001p0Q;
static Q ten = 10;
static Q half = 0.5;
static Q third_from_x87 = 0x1.0000000000000002p-2L;
static Q mone = -1;
static _Bool nonzero = 0x1p-16494Q;
Q multiply(Q a, Q b) { return a * b; }
int main(void) {
    Q one = ten * half / 5;
    Q sum = one + min;
    Q choice = (int)(-mone) == 1 ? one : sum;
    Q next = -small;
    if (sum != one || min == 0 || !nonzero || !min || next <= choice || ++choice != 2) return 1;
    if ((double)half != 0.5 || (long double)third_from_x87 != 0x1.0000000000000002p-2L) return 2;
    if (multiply(huge, min) == 0 || __builtin_isinf(huge) || !__builtin_isfinite(huge)) return 3;
    volatile long double v = 1.0L;
    if ((Q)v != one) return 4;
    return 0;
}
