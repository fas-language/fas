#include <stdarg.h>
#include <stdckdint.h>

typedef void no_parameters;
void declaration(no_parameters);
void declaration(void);

static int array[2];
static_assert(__builtin_classify_type(array) == 5);
static_assert(__builtin_classify_type((float _Complex)1) == 9);

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

long atomic_number;
long atomic_operations(void)
{
    long expected = 0;
    static_assert(__builtin_types_compatible_p(typeof(
        __atomic_compare_exchange_n(&atomic_number, &expected, 1, 0, 0, 0)), bool));
    return __sync_val_compare_and_swap(&atomic_number, expected, 2) +
           __atomic_add_fetch(&atomic_number, 1, __ATOMIC_RELAXED);
}
