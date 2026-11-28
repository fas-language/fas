extern int disabled_subsystem(void);

static volatile int effects;

static inline int feature_disabled(int unused)
{
    return 0;
}

static inline void empty_stub(int unused)
{
}

static inline void *absent_device(void)
{
    return (void *)0;
}

static int check(int jump)
{
    if (jump)
        goto live;
    if (0) {
live:
        return 5;
    }
    if (__builtin_expect(0, 0))
        return disabled_subsystem();
    if (jump && 0)
        return disabled_subsystem();
    if ((effects = effects + 1, jump) && 0)
        return disabled_subsystem();
    if (feature_disabled(effects = effects + 1))
        return disabled_subsystem();
    empty_stub(effects = effects + 1);
    void *device = absent_device();
    if (device)
        return disabled_subsystem();
    switch (jump) {
    case 0:
        if (sizeof(int) == 0)
            return disabled_subsystem();
        break;
    }
    return 9;
}

int main(void)
{
    return check(1) != 5 || check(0) != 9 || effects != 3;
}
