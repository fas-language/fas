#include <asm/unistd.h>
#include <linux/reboot.h>

static long kernel_call(long number, long a, long b, long c, long d) {
    long result;
    register long fourth __asm__("r10") = d;
    __asm__ volatile("syscall" : "=a"(result)
                     : "0"(number), "D"(a), "S"(b), "d"(c), "r"(fourth)
                     : "rcx", "r11", "memory");
    return result;
}

void _start(void) {
    static char const marker[] = "FCC_KERNEL_INIT_OK\n";
    kernel_call(__NR_write, 1, (long)marker, sizeof(marker) - 1, 0);
    kernel_call(__NR_reboot, LINUX_REBOOT_MAGIC1, LINUX_REBOOT_MAGIC2,
                LINUX_REBOOT_CMD_POWER_OFF, 0);
    kernel_call(__NR_exit, 1, 0, 0, 0);
    __builtin_unreachable();
}
