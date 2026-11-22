struct Tag { int ignored; };
typedef struct { int also_ignored; } Alias;

struct GNU {
    const;
    int;
    struct Tag;
    Alias;
    struct { char promoted; };
};

static_assert(sizeof(struct GNU) == sizeof(char));
static_assert(__builtin_offsetof(struct GNU, promoted) == 0);

struct Bits { char c; int : 20; };
union UnionBits { char c; int : 20; };
static_assert(sizeof(struct Bits) == 4 && alignof(struct Bits) == 1);
static_assert(sizeof(union UnionBits) == 3 && alignof(union UnionBits) == 1);

#pragma pack(1)
struct Packed { char c; int i; };
#pragma pack(0)
struct Reset { char c; int i; };
static_assert(__builtin_offsetof(struct Packed, i) == 1);
static_assert(__builtin_offsetof(struct Reset, i) == 4);

struct Flexible { int data[]; };
union FlexibleUnion { int data[]; long ordinary; };
static_assert(sizeof(struct Flexible) == 0);
static_assert(alignof(struct Flexible) == alignof(int));
static_assert(sizeof(union FlexibleUnion) == sizeof(long));

#pragma pack(push, byte_pack, 1)
#pragma pack(push, half_pack, 2)
struct HalfPacked { char c; int i; };
#pragma pack(pop, byte_pack)
struct Unpacked { char c; int i; };
static_assert(__builtin_offsetof(struct HalfPacked, i) == 2);
static_assert(__builtin_offsetof(struct Unpacked, i) == 4);

// A GNU-foldable address calculation can supply a fixed member bound.
static_assert(sizeof(struct { char a[((unsigned long)(char *)3) + 1]; }) == 4);

int alignment_override __attribute__((aligned(2)));
static_assert(alignof(alignment_override) == 2);
static_assert(alignof(typeof(alignment_override)) == alignof(int));

struct {
    int ordinary __attribute__((aligned(2)));
    int packed __attribute__((packed, aligned(2)));
} alignment_members;
static_assert(alignof(alignment_members.ordinary) == alignof(int));
static_assert(alignof(alignment_members.packed) == 2);
