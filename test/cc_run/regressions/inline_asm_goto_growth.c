static __attribute__((always_inline)) inline int step(int n)
{
    int result;
    asm goto ("mov %1, %0; test %1, %1; jne %l[nonzero]"
              : "=r"(result) : "r"(n) : "cc" : nonzero);
    return result + 1;
nonzero:
    return result + 3;
}

int sum(int n)
{
    return step(n) + step(n + 1) + step(n + 2) + step(n + 3) +
           step(n + 4) + step(n + 5);
}

int main(void)
{
    return sum(0) != 31;
}
