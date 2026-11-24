int main(void)
{
    unsigned long output;
    asm("incq %[result]" : [result] "=r"(output) : "[result]"(41ul));
    return output != 42;
}
