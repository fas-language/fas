int value;
int f(void)
{
    return __atomic_load_n(&value, 0.0f);
}
