#include <stddef.h>
#include <stdarg.h>

typedef unsigned long T;
typedef int (*Callback)(int);

enum E : unsigned short { E1 = 17, E2 };
struct Record {
    char tag;
    alignas(8) int data;
    unsigned low : 3;
    union { int n; short s; };
};
static_assert(sizeof(enum E) == 2);
static_assert(offsetof(struct Record, data) == 8);
static_assert(offsetof(struct Record, n) == 16);
static_assert(_Generic(E1, unsigned short: true, default: false));
static_assert(_Generic((unsigned _BitInt(7))125 + (unsigned _BitInt(7))4,
                      unsigned _BitInt(7): true, default: false));
static_assert((unsigned _BitInt(7))125 + (unsigned _BitInt(7))4 == 1);

constexpr int answer = 42;
static_assert(answer == 42);
static int target;
static int *addr1 = &target;
static const char text[] = "a" "b";
static int matrix[2][2] = {1, 2, 3, 4};
static int designated[] = {[3] = 1, 2};
static_assert(sizeof text == 3);
static_assert(sizeof designated == 5 * sizeof(int));
extern int tentative[];
int tentative[3];
int add(int x) { return x + 1; }
Callback callback = add;
[[nodiscard]] int test(int n, ...)
{
    va_list ap;
    va_start(ap, n);
    const T t = (T)va_arg(ap, int);
    va_end(ap);
    typeof_unqual(t) u = t;
    int dynamic[n][2];
    auto bytes = sizeof dynamic;
    struct Record r = { .n = 3, .data = 5 };
    int *p = nullptr;
    if (p == nullptr) goto done;
    return dynamic[0][1];
done:
    int T = 1;
    return r.n + (int)u + (int)bytes + T;
}
static_assert((constexpr int){42} == 42);
constexpr struct {
    int row[4];
    struct { int x, y; } inner;
    unsigned bit : 3;
} constant_record = {.row[2] = 7, .inner = {1, 2}, .inner.y = 9, .bit = 5};
static_assert(constant_record.row[2] == 7 && constant_record.row[0] == 0);
static_assert(constant_record.inner.x == 1 && constant_record.inner.y == 9);
static_assert(constant_record.bit == 5);
static_assert(sizeof (static int){2} == sizeof(int));
static int *compound_address(void) { return &(static int){4}; }

static int vla_switch(int n) {
    int values[n];
    switch (n) {
    case 1: return sizeof values;
    default: return 0;
    }
}
static_assert(0x0'e-0xe == 0);

struct ConstRecord { int head; int member[2]; };
constexpr struct ConstRecord source = {2, {4, 6}};
constexpr struct ConstRecord copy = source;
struct ConstRecord static_copy = copy;
static_assert(copy.head == 2);

extern int length;
void prototype(int a[(int){length}]);

#line 1'23
static_assert(__LINE__ == 123);
