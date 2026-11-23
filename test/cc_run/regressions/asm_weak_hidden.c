#include <stdio.h>

__asm__(".text\n"
        ".globl probe\n"
        ".type probe,@function\n"
        ".weak missing\n"
        ".hidden missing\n"
        "probe:\n"
        "mov missing@GOTPCREL(%rip), %rax\n"
        "ret\n"
        ".size probe, .-probe\n");

extern void *probe(void);

int main(void)
{
    printf("%s\n", probe() ? "present" : "missing");
    return probe() != 0;
}
