typedef int int4 __attribute__((vector_size(16)));
struct __attribute__((packed)) Packed { char c; long x; };
#pragma pack(push, 1)
struct Aligned { char c; long long q; };
#pragma pack(pop)
static_assert(sizeof(struct Packed) == 9);
static_assert(sizeof(struct Aligned) == 9);
static_assert(sizeof(int4) == 16);
static_assert(__builtin_offsetof(struct Packed, x) == 1);
static_assert(__builtin_types_compatible_p(__typeof__(0UL), unsigned long));
static_assert(__builtin_choose_expr(1, 5, 1 / 0) == 5);
static_assert(__builtin_constant_p(1 + 2));
int g(int x) __asm__("g_asm") __attribute__((noinline));
int g(int x)
{
    __label__ local;
    asm volatile goto ("" : "+r"(x) : "r"(x) : "memory" : local);
    if (x) goto *&&local;
    switch (x) {
        case 2 ... 4: return ({ int y = 5; y; });
        default: break;
    }
local:
    return x ?: 7;
}
typedef long lowered_alignment __attribute__((aligned(1)));
static_assert(alignof(lowered_alignment) == 1);
static_assert(sizeof(lowered_alignment) == sizeof(long));
struct WithLoweredAlignment { char c; lowered_alignment value; };
static_assert(__builtin_offsetof(struct WithLoweredAlignment, value) == 1);

static const int (*qualified_array(const struct Matrix { int row[3]; } *m))[3] {
    const int (*row)[3] = &m->row;
    return row;
}
