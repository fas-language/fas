#include <stdio.h>

/* The kernel's scoped_guard: the loop step jumps to a label inside `if (0)`.
   f calls a function defined later, so fcc generates its body at the end of
   the file; folding `if (0)` there once dropped the label and the jump hit
   an `unreachable` (ud2 in device_links_check_suppliers). */
static int later(int v);

static int f(int v) {
    int r = 0;
    for (int once = 1; once; ({ goto out; }))
        if (0) {
out:
            break;
        } else {
            r = later(v);
        }
    return r;
}

/* The same shape in the else branch of `if (1)`. */
static int g(int v) {
    int r = 0;
    for (int once = 1; once; ({ goto out; }))
        if (1) {
            r = later(v) * 2;
        } else {
out:
            break;
        }
    return r;
}

static int later(int v) { return v + 1; }

int main(void) {
    printf("%d %d\n", f(4), g(4));
}
