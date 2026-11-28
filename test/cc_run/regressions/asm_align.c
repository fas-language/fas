int main(void) {
    unsigned long address;
    asm(".p2align 6\n"
        "1: lea 1b(%%rip), %0"
        : "=r"(address));
    return (address & 63) != 0;
}
