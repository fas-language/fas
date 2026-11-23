#include <stdio.h>

typedef unsigned char bytes16 __attribute__((vector_size(16)));

static bytes16 average(bytes16 a, bytes16 b) {
    return (bytes16)__builtin_ia32_pavgb128(a, b);
}

int main(void) {
    union { bytes16 v; unsigned char u[16]; } result;
    result.v = average((bytes16){255, 0, 254, 3, 4}, (bytes16){255, 255, 0, 4, 4});
    for (unsigned i = 0; i < 16; ++i)
        printf("%u\n", result.u[i]);
}
