int main(void)
{
    const char *text;
    __asm__ volatile ("leaq %c1(%%rip), %0" : "=r"(text) : "i"("marker"));
    return text[0] != 'm' || text[5] != 'r' || text[6] != '\0';
}
