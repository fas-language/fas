#include <stdckdint.h>

bool overflow(char a, int b) {
    return ckd_add(&b, a, b);
}
