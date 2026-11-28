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

/* bitfield.h: call sites disagree on the mask, so each constant mask
 * needs its own specialization before __bad_mask folds away. */
static unsigned long field_multiplier(unsigned long field)
{
    if ((field | (field - 1)) & ((field | (field - 1)) + 1))
        disabled_callback();
    return field & -field;
}

static unsigned long field_mask(unsigned long field)
{
    return field / field_multiplier(field);
}

static unsigned long encode_bits(unsigned long v, unsigned long field)
{
    return (v & field_mask(field)) * field_multiplier(field);
}

/* A !CONFIG stub ignores its table argument; the table and everything it
 * points to must not be kept alive by the call. */
static void table_entry(void)
{
    disabled_callback();
}

static void (*const table_ops[])(void) = { table_entry };

static void *alloc_stub(void *owner, void (*const *ops)(void))
{
    return owner;
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
    unsigned long raw = visits + 4;
    if (encode_bits(raw, 0x30) != 0x10 || encode_bits(raw, 0xf00) != 0x500)
        return 1;
    if (alloc_stub(&probe, table_ops) != &probe)
        return 1;
    return compat(enabled) != 42 || visits != 1;
}
