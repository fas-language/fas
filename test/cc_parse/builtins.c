#include <stdarg.h>
#include <stdckdint.h>

int sum_and_check(int count, ...) {
    va_list ap, saved;
    va_start(ap, count);
    va_copy(saved, ap);
    int a = va_arg(ap, int);
    int b = va_arg(saved, int);
    va_end(saved);
    va_end(ap);

    int result;
    static_assert(_Generic(ckd_add(&result, a, b), bool: 1, default: 0));
    return ckd_add(&result, a, b) || __builtin_isgreater(1.0f, result);
}
