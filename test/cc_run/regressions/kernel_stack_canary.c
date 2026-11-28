#include <asm/prctl.h>
#include <sys/syscall.h>
#include <unistd.h>

__attribute__((noinline))
int protected_function(int n)
{
    return n + 1;
}

__attribute__((no_stack_protector))
int main(void)
{
    unsigned long data[6] = {0, 0, 0, 0, 0, 0x12345678};
    unsigned long old_gs;
    if (syscall(SYS_arch_prctl, ARCH_GET_GS, &old_gs) ||
        syscall(SYS_arch_prctl, ARCH_SET_GS, data))
        return 1;
    int result = protected_function(41);
    if (syscall(SYS_arch_prctl, ARCH_SET_GS, old_gs))
        return 1;
    return result != 42;
}
