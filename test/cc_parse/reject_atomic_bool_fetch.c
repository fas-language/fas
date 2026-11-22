bool b;
int f(void)
{
    return __atomic_fetch_add(&b, 1, __ATOMIC_RELAXED);
}
