#include <stdio.h>

extern struct state global;

static struct state *address(void) {
    return &global;
}

struct state { int value; };
struct state global = {42};

int main(void) {
    printf("%d\n", address()->value);
    return address() != &global || address()->value != 42;
}
