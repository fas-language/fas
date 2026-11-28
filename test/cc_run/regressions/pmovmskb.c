#include <emmintrin.h>
#include <stdio.h>

__attribute__((noinline)) static int mask(__m128i v) {
    return _mm_movemask_epi8(v);
}

int main(void) {
    int actual = mask(_mm_setr_epi8(-128, 1, 2, -3, -4, 5, 6, 7,
                                   8, -9, 10, 11, 12, 13, 14, -1));
    printf("%x\n", actual);
    return actual != 0x8219;
}
