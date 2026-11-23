#include <stdatomic.h>
#include <stdio.h>

static int value(int n) { return n + 1; }
static _Atomic(int (*)(int)) callback = value;

int main(void) {
    int (*f)(int) = (void *)atomic_load_explicit(&callback, memory_order_relaxed);
    void *p = f;
    int (*g)(int) = p;
    printf("%d\n", g(41));
}
