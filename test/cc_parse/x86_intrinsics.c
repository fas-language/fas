#include <x86intrin.h>

typedef float f32x4 __attribute__((vector_size(16)));
typedef int i32x2 __attribute__((vector_size(8)));
typedef int i32x4 __attribute__((vector_size(16)));

static_assert(__builtin_types_compatible_p(typeof(__builtin_ia32_vec_init_v2si(1, 2)), i32x2));
static_assert(__builtin_types_compatible_p(typeof(0.0F16), _Float16));
static_assert(!__builtin_types_compatible_p(__bf16, _Float16));
static_assert(sizeof(__bf16) == 2 && alignof(__bf16) == 2);

f32x4 shuffle(f32x4 a, f32x4 b) {
    static_assert(__builtin_types_compatible_p(typeof(__builtin_shufflevector(a, b, 0, 1)),
                                               float __attribute__((vector_size(8)))));
    return __builtin_shuffle(a, b, (i32x4){4, 1, 2, 3});
}
