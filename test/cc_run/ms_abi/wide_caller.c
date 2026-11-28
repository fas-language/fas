#include <stdio.h>
#include "wide.h"

int main(int argc, char **argv) {
    wide32 a = {0};
    wide64 b = {0};
    for (int i = 0; i < 8; ++i)
        LANES(a)[i] = 10 + i;
    for (int i = 0; i < 16; ++i)
        LANES(b)[i] = 30 + 2 * i;

    wide_fn *volatile fp = wide_stack;
    wide64 r = fp(a, 2.5, 3, 4, b);
    printf("%d %d\n", LANES(a)[0], LANES(b)[15]);
    for (int i = 0; i < 16; ++i)
        printf("%d\n", LANES(r)[i]);
    if (argc == 1) {
        wide32 s = wide_varargs(9, b, a, 8, 7, a);
        for (int i = 0; i < 8; ++i)
            printf("%d\n", LANES(s)[i]);
    }
}
