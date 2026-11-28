int main(void) {
    unsigned long result;
    asm("xor %%eax, %%eax\n"
        "xor %%edx, %%edx\n"
        "mov %1, %0"
        : "=r"(result)
        : "r"(42UL)
        : "0", "1");
    return result != 42;
}
