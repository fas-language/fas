#include <stdio.h>

int main(void) {
    int sum = 0;
    for (int i = 0, step = i + 2, end = 7; i < end; i += step) {
        sum += i;
        ++step;
    }
    printf("%d\n", sum);
}
