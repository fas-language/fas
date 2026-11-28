__attribute__((noinline))
static long add(long a, long b)
{
    return a + b;
}

__attribute__((ms_abi, noinline))
static long firmware_entry(long a, long b, long c, long d, long e)
{
    return add(a + b + c, d + e);
}

int main(void)
{
    volatile long a = 2;
    return firmware_entry(1, a, 3, 4, 5) != 15;
}
