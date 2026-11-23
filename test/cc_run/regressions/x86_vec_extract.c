#include <stdio.h>

typedef int v4si __attribute__((vector_size(16)));

static int last(v4si v) {
    return __builtin_ia32_vec_ext_v4si(v, 3);
}

int main(void) {
    v4si v = {11, -22, 33, -44};
    printf("%d %d\n", __builtin_ia32_vec_ext_v4si(v, 0), last(v));
}
