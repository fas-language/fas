int main(void)
{
    asm volatile("mov %rsp, %rsp\n"
                 "# This basic asm has literal { | } and %= markers");
    asm volatile("mov %%rsp, %%rsp" ::: "memory");
    return 0;
}
