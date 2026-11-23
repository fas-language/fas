#include <emmintrin.h>
#include <stdio.h>

int main(void)
{
    __m128i lhs = _mm_setr_epi8(1, -56, 3, -1, 5, 6, 7, 8,
                                9, 10, 11, 12, 13, 14, 15, 16);
    __m128i rhs = _mm_setr_epi8(-56, 20, 30, -128, 50, 60, 70, 80,
                                90, 100, 110, 120, 13, 14, 0, 16);
    unsigned char bytes[16];
    _mm_storeu_si128((__m128i_u *)bytes, _mm_min_epu8(lhs, rhs));
    for (int i = 0; i < 16; i++)
        printf("%u%c", (unsigned)bytes[i], i == 15 ? '\n' : ' ');
    _mm_storeu_si128((__m128i_u *)bytes, _mm_subs_epu8(lhs, rhs));
    for (int i = 0; i < 16; i++)
        printf("%u%c", (unsigned)bytes[i], i == 15 ? '\n' : ' ');
}
