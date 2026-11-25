/* An always_inline static whose asm needs an immediate from its caller is
 * kept for inlining. Kernel code calls such functions before their
 * definitions (DEFINE_IDTENTRY) and takes their address through a cast
 * (BPF_CALL_x), so fcc must still inline the first and emit the second. */
typedef unsigned long (*entry_fn)(unsigned long);

static inline __attribute__((always_inline)) unsigned long scaled(unsigned long x);

unsigned long early(unsigned long x)
{
    return scaled(x);
}

unsigned long through_cast(unsigned long x)
{
    return ((entry_fn)scaled)(x);
}

static inline __attribute__((always_inline)) unsigned long add_imm(unsigned long x, const int k)
{
    asm("add %1, %0" : "+r"(x) : "i"(k));
    return x;
}

static inline __attribute__((always_inline)) unsigned long scaled(unsigned long x)
{
    const int k = 3;
    return add_imm(x, k);
}

int main(void)
{
    return early(4) != 7 || through_cast(5) != 8;
}
