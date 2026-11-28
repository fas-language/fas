#include <stdio.h>
#include "calls.h"

int main(void) {
    mixed_fn *volatile fp = mixed;
    printf("%.2f %.2f\n", mixed(1, 2.25, 3, 4.5f, 5, 6.25),
                          fp(10, 20.5, 30, 40.25f, 50, 60.5));

    struct small s = small((struct small){1, 2}, (struct small){3, 4},
                           (struct small){5, 6}, (struct small){7, 8},
                           (struct small){9, 10});
    printf("%u %u %.2f %.2f %.2f\n", (unsigned)tiny((struct tiny){250}).byte,
           (unsigned)halfword((struct halfword){999}).value,
           word((union word){.f = 1.25f}).f, s.x, s.y);

    const struct large original = {{1, 2, 3}};
    struct large r = large(10.5f, original, 11, 20.75, 30);
    struct aligned a = aligned((struct aligned){19});
    printf("%ld %ld %ld %ld %u %.2Lf\n", r.values[0], r.values[1], r.values[2],
           original.values[0], (unsigned)a.byte, extended(10.25L, 2));

    vec4 v = vector((vec4){1, 2, 3, 4});
    vec2 w = short_vector((vec2){17, 19});
    unsigned __int128 p = wide(((__int128)9 << 64) | 18446744073709551615ULL);
    printf("%d %d %d %d %d %d %lu %lu %.2f\n", v[0], v[1], v[2], v[3], w[0], w[1],
           (unsigned long)(p >> 64), (unsigned long)p, (double)quad(5.25Q));

    r = varargs(10, 6.75, original, (struct small){3.25f, 4.5f}, 7.5L, 31L);
    printf("%ld %ld %ld %.2f\n", r.values[0], r.values[1], r.values[2],
           stack_varargs(1, 2, 3, 4, 5, 6.5));

    int preserved = check_preservation();
    printf("callee-saved: %d\n", preserved);
    return preserved != 1;
}
