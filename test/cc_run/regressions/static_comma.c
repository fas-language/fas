// GNU constant folding used by CPython's Py_ARRAY_LENGTH macro.
#include <stdio.h>

static char letters[] = "abc";
static const unsigned count = sizeof(letters) +
    ((void)sizeof(struct {
         int dummy;
         static_assert(sizeof(letters) == 4);
     }), 0);
static const double fraction = (0, 2.5);

int main(void) {
    printf("%u %.1f\n", count, fraction);
    return count != 4 || fraction != 2.5;
}
