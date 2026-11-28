#include <stdio.h>

/* -ftrivial-auto-var-init=zero (the kernel builds with it): locals without
   an initializer start as zero, scalars, aggregates and VLAs alike, and
   the padding of a partly initialized struct is zero too. */
struct pad { char c; long l; };

__attribute__((noinline)) static void dirty(void) {
    volatile unsigned char junk[1024];
    for (int i = 0; i < 1024; i++) junk[i] = 0xa5;
}

__attribute__((noinline)) static long check(int n) {
    int i;
    long l;
    struct pad p;
    struct pad q = {.c = 1};
    char buf[40];
    int vla[n];
    long sum = i + l + p.c + p.l;
    for (int k = 0; k < 40; k++) sum += buf[k];
    for (int k = 0; k < n; k++) sum += vla[k];
    const unsigned char *bytes = (const unsigned char *)&q;
    for (unsigned k = 1; k < sizeof q; k++) sum += bytes[k];
    return sum;
}

int main(void) {
    dirty();
    printf("%ld\n", check(9));
}
