#include <stdbool.h>
#include <stdio.h>

/* The kernel's static branches emit "%c0 + %c1" with a bool branch: true
 * must print as 1, or the key address picks up the LINKED flag bits. */
static inline __attribute__((always_inline)) long tagged(const char *key, const bool branch)
{
    long r;
    asm("lea %c1 + %c2(%%rip), %0" : "=r"(r) : "i"(key), "i"(branch));
    return r;
}

static const char keys[2];

int main(void)
{
    long yes = tagged(keys, true), no = tagged(keys, false);
    printf("%ld %ld\n", yes - (long)keys, no - (long)keys);
    return 0;
}
