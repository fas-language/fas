static_assert(__builtin_types_compatible_p(typeof(__builtin_copysignf(2., -1.)), float));
static_assert(__builtin_types_compatible_p(typeof(__builtin_creall(2.f)), long double));
static_assert(__builtin_types_compatible_p(typeof(__builtin_conjf(2.0Li)), _Complex float));
static_assert(__builtin_types_compatible_p(typeof(__builtin_nansf16("")), _Float16));

int main(void) {
    volatile _Complex long double a = 2.0L + 3.0Li;
    _Complex float b = __builtin_conjf(a);
    if (__builtin_creall(b) != 2 || __builtin_cimag(b) != -3) return 1;
    return __builtin_copysignf(2., -1.) != -2;
}
