#include <stdio.h>

int main(void)
{
    unsigned long carry;
    int wide = __builtin_addcl(3, 4, 0, &carry) == 7 && carry == 0;
    int real = __builtin_powi(2.0, 2) == 4.0;
    printf("%d %d\n", wide, real);
    return !wide || !real;
}
