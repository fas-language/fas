typedef long Signed __attribute__((vector_size(16)));
typedef unsigned long Unsigned __attribute__((vector_size(16)));

static_assert(__builtin_types_compatible_p(
    typeof((Signed){0, 0} >> (Unsigned){0, 0}), Signed));
static_assert(__builtin_types_compatible_p(
    typeof((Unsigned){0, 0} >> (Signed){0, 0}), Unsigned));

void do_jump(int abort_jump)
{
    if (abort_jump)
        goto *0;
    void *labels[] = {&&finish};
    goto *labels[0];
finish:
    return;
}

void runtime_bad_bound(void)
{
    int v[1 / 0];
    (void) v;
}

void arrays_in_asm(int n)
{
    char array[n];
    asm volatile ("" : "+m" (array));
}
