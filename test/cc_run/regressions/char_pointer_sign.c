#include <stdint.h>
#include <stdio.h>

int main(void) {
    const char s[] = "abc";
    const uint8_t *u = s;
    printf("%u %u\n", (unsigned)u[0], (unsigned)u[2]);
}
