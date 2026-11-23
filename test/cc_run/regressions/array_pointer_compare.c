#include <stdio.h>

static int check(char *p, char storage[8])
{
    return p == storage && storage == p && 0 != storage && storage != 0;
}

int main(void)
{
    char storage[8];
    char other[8];
    char *volatile p = storage;
    char *volatile q = other;
    int same = p == storage && storage == p && check(p, storage);
    int different = q != storage && storage != q;
    printf("%d %d\n", same, different);
    return !same || !different;
}
