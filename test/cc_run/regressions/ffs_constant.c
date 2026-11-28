static const int positions[] = {
    __builtin_ffs(0),
    __builtin_ffs(-8),
    __builtin_ffsl(1l << 45),
    __builtin_ffsll(1ll << 53),
};

int main(void)
{
    volatile long long value = 1ll << 53;
    return positions[0] != 0 || positions[1] != 4 ||
           positions[2] != 46 || positions[3] != 54 ||
           __builtin_ffsll(value) != positions[3];
}
