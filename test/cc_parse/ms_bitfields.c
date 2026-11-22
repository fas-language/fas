struct __attribute__((__ms_struct__)) Shared;
struct Shared {
    char prefix;
    unsigned first : 3;
    int second : 7;
    unsigned short third : 5;
    char tail;
};
static_assert(sizeof(struct Shared) == 12);
static_assert(alignof(struct Shared) == 4);
static_assert(__builtin_offsetof(struct Shared, tail) == 10);

#pragma pack(push, 2)
struct __attribute__((ms_struct)) ZeroWidth {
    char prefix;
    unsigned first : 1;
    unsigned long : 0;
    char tail;
};
#pragma pack(pop)
static_assert(sizeof(struct ZeroWidth) == 16);
static_assert(alignof(struct ZeroWidth) == 8);
static_assert(__builtin_offsetof(struct ZeroWidth, tail) == 8);

struct __attribute__((ms_struct)) AlignedZeroWidth {
    char prefix;
    unsigned : 0 __attribute__((aligned(8)));
    char tail;
};
static_assert(alignof(struct AlignedZeroWidth) == 8);
static_assert(__builtin_offsetof(struct AlignedZeroWidth, tail) == 8);

// Match Clang on x86-64 when packed and ms_struct occur together.
struct __attribute__((packed, ms_struct)) Packed {
    char prefix;
    unsigned value : 1;
    char tail;
};
static_assert(sizeof(struct Packed) == 12);
static_assert(__builtin_offsetof(struct Packed, tail) == 8);

union __attribute__((ms_struct)) Union {
    unsigned value : 1;
    char byte;
};
static_assert(sizeof(union Union) == 4);
static_assert(alignof(union Union) == 1);
