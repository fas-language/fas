/* -ftrivial-auto-var-init=zero (the kernel's CONFIG_INIT_STACK_ALL_ZERO)
 * zeroes `bool dax;` with a memset before its real store. mm/truncate.c's
 * dax_mapping() is constant false without CONFIG_FS_DAX, so the call to the
 * undefined dax_delete_mapping_entry must still fold away: the zeroing
 * memset is a known store, not an escape. */
#include <stdbool.h>

extern int disabled_delete(void *mapping, long index);

static volatile int visits;

static bool disabled_mapping(void *mapping)
{
    visits++;
    return mapping && (visits & 0);
}

__attribute__((noinline)) static int batch(void *mapping, int n)
{
    bool dax;
    int cleared = 0;
    dax = disabled_mapping(mapping);
    for (int i = 0; i < n; i++) {
        if (__builtin_expect(dax, 0)) {
            disabled_delete(mapping, i);
            continue;
        }
        cleared++;
    }
    return cleared;
}

/* Zeroed, then written on one path only: the load must not become 0. */
__attribute__((noinline)) static int maybe(int c)
{
    int v;
    if (c)
        v = 7;
    else
        v = 0;
    long w;
    if (c > 1)
        w = c;
    return v + (c > 1 ? (int)w : 0);
}

/* Only part of a struct is written after the zeroing. */
struct pair { int a, b; };
__attribute__((noinline)) static int partial(int x)
{
    struct pair p;
    p.b = x;
    return p.a + p.b;
}

int main(void)
{
    int probe = 0;
    return batch(&probe, 5) != 5 || maybe(1) != 7 || maybe(0) != 0 || maybe(3) != 10 ||
           partial(4) != 4 || visits != 1;
}
