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

struct group;

/* Disabled-config stubs that build null from an integer. */
static struct group *absent_group(void)
{
    unsigned long none = 0;
    return (struct group *)none;
}

static unsigned long group_bits(void)
{
    return (unsigned long)(struct group *)0;
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
    if (absent_group())
        disabled_callback();
    if (group_bits())
        disabled_callback();
    return compat(enabled) != 42 || visits != 1;
}
