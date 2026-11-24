static_assert(_Generic(__builtin_fmin(1, 2), double: 1));
static_assert(_Generic(__builtin_fmaxf(1, 2), float: 1));
static_assert(_Generic(__builtin_fmaxl(1, 2), long double: 1));

int main(void)
{
    volatile double x = 0.125, y = 0.25;
    long double _Complex a = 6.0L + 8.0Li;
    long double _Complex b = 2.0L - 4.0Li;
    long double _Complex q = a / b;
    return __real__ q != -1.0L || __imag__ q != 2.0L ||
           __builtin_fmin(x, y) != x ||
           __builtin_fmaxf(x, y) != y ||
           __builtin_fmaxl(x, y) != y;
}
