enum { LOW = __builtin_ctz(8u), HIGH = __builtin_ctzl(1ul << 42) };

struct field {
    unsigned width : __builtin_ctzll(1ull << 17);
};

int main(void)
{
    volatile unsigned long long input = 1ull << 50;
    struct field f = { 1u << 16 };
    return LOW != 3 || HIGH != 42 || f.width != 65536 ||
           __builtin_ctzll(input) != 50;
}
