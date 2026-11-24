#include <stdint.h>
#include <stdio.h>

static unsigned tied_input(uintptr_t p)
{
    unsigned status;
    // The input is the full pointer in rcx. Only the status is narrowed.
    asm("shrq $32, %%rcx" : "=c"(status) : "0"((void *)p) : "cc");
    return status;
}

static unsigned tied_goto(uintptr_t p)
{
    unsigned status;
    asm goto("shrq $32, %%rcx; jnz %l[done]"
             : [status] "=c"(status) : "[status]"(p) : "cc" : done);
done:
    return status;
}

int main(void)
{
    unsigned a = tied_input(UINT64_C(0x123456789abcdef0));
    unsigned b = tied_goto(UINT64_C(0xabcdef0192345678));
    unsigned c = tied_goto(UINT64_C(0x92345678));
    printf("%x %x %x\n", a, b, c);
    return a != 0x12345678 || b != 0xabcdef01 || c != 0;
}
