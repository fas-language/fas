#include <float.h>

struct S { char empty[0]; };
static struct S s = { {1, {2}} };
static char text[1] = "ab";
static char braced[2] = {"a",};
static_assert(sizeof(s.empty) == 0 && sizeof(text) == 1 && sizeof(braced) == 2);
static_assert(__builtin_types_compatible_p(typeof(INFINITY), float));
static_assert(__builtin_types_compatible_p(typeof(NAN), float));
static_assert(__builtin_types_compatible_p(typeof(__builtin_calloc(1, 2)), void *));
static_assert(__builtin_types_compatible_p(typeof(__builtin___strcpy_chk(text, "a", 1)), char *));
static_assert(__builtin_types_compatible_p(typeof(__builtin_strncmp(text, "a", 1)), int));

typedef float floats __attribute__((vector_size(16)));
typedef int masks __attribute__((vector_size(16)));
masks compare(floats a, floats b) { return a < b; }
floats comparison_as_floats(floats a, floats b) { return a < b; }

int wide_cases(_BitInt(129) v) {
    switch (v) {
    case 1: return 1;
    case (((_BitInt(129))1) << 100) + 1: return 2;
    case -(((_BitInt(129))1) << 100) ... -(((_BitInt(129))1) << 99): return 3;
    default: return 0;
    }
}

static int gnu_negative_shift = -2 << 7;
static _BitInt(75) gnu_negative_wide_shift = (_BitInt(75))-1 << 74;
static_assert((-2 << 7) == -256);
static_assert(((_BitInt(75))-2 << 71) == -((_BitInt(75))1 << 72));
