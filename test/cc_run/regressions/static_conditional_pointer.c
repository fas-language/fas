#include <stdint.h>
#include <stdio.h>

static const int ascii[] = {10, 20, 30};
static const int extended[] = {100, 200, 300};
#define CODEPOINT(i) ((i) < 3 ? &ascii[i] : &extended[(i) - 3])

static const int *const table[] = {
    CODEPOINT(1), CODEPOINT(4), 0 ? &ascii[0] : 0,
    1 ? (const int *)(uintptr_t)16 : &ascii[0],
};

int main(void) {
    if (table[0] != &ascii[1] || table[1] != &extended[1] || table[2] ||
        (uintptr_t)table[3] != 16)
        return 1;
    printf("%d %d\n", *table[0], *table[1]);
}
