#include <stdio.h>

int function();

static int via_pointer(int (*fn)(int, double)) {
    return fn(3, 4.0);
}

int function(int a, double b) {
    return a + (int)b;
};

int main(void) {
    int (*old)() = function;
    int first = old((char)5, 6.0f);
    int second = via_pointer((int (*)())old);
    printf("%d %d\n", first, second);
    return first != 11 || second != 7;
}
