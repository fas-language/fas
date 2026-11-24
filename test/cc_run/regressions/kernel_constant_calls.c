extern void disabled_callback(void);
extern int disabled_config_table;

static volatile int visits;

static int feature_test(const int *value)
{
    visits++;
    return value && (*value & 0);
}

static int compat(int enabled)
{
    if (enabled)
        disabled_callback();
    return 42;
}

__attribute__((noinline)) void feature_noop(const int *table)
{
}

__attribute__((used))
static int retained_setting = 19;

int main(void)
{
    int probe = visits;
    if (feature_test(&probe))
        disabled_callback();
    int enabled = visits;
    enabled &= 0;
    feature_noop(&disabled_config_table);
    return compat(enabled) != 42 || visits != 1;
}
