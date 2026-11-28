int kernel_data[4];

__attribute__((noinline))
int *link_address(void)
{
    return &kernel_data[2];
}

__attribute__((noinline))
int *mapped_address(void)
{
    int *result;
    __asm__("leaq %c1(%%rip), %0" : "=r"(result) : "i"(&kernel_data[2]));
    return result;
}

int main(void)
{
    return link_address() != &kernel_data[2] ||
           mapped_address() != &kernel_data[2];
}
