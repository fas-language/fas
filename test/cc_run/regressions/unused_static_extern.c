#include <stdio.h>

/* Header-style statics that nothing references. Their calls name functions
 * the program never defines, so emitting them would break the link. */
extern void never_defined(void *p);

static void plain_static(void *p)
{
    never_defined(p);
}

static inline int unused_inline(void)
{
    plain_static(0);
    return 1;
}

static int unused_table[] = { 1, 2, 3 };
static void *unused_ref = &unused_table;

__attribute__((used)) static int retained = 7;

int main(void)
{
    printf("%d\n", retained);
    return 0;
}
