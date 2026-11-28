#include <stdio.h>
#include <string.h>

struct state {
    unsigned kind;
    unsigned data;
};

extern void impossible_case(void);

static inline __attribute__((always_inline)) void next(struct state *s)
{
    switch (s->kind) {
    case 0:
        impossible_case();
        return;
    case 1:
        s->kind = 0;
        return;
    default:
        s->kind = 1;
    }
}

int guarded(unsigned kind)
{
    struct state s = {kind, 0};
    for (; s.kind != 0; next(&s))
        ++s.data;
    return s.data;
}

static __attribute__((noinline)) void reset(unsigned *kind)
{
    *kind = 0;
}

int escapes(unsigned kind)
{
    struct state s = {kind, 0};
    if (s.kind != 0) {
        reset(&s.kind);
        switch (s.kind) {
        case 0: return 9;
        case 1: return 8;
        default: return 7;
        }
    }
    return 6;
}

int overlapping_store(unsigned kind)
{
    struct state s = {kind, 0};
    if (s.kind != 0) {
        memset(&s.kind, 0, 1);
        switch (s.kind) {
        case 0: return 3;
        case 1: return 4;
        default: return 5;
        }
    }
    return 6;
}

int main(void)
{
    printf("%d %d %d %d %d %d\n", guarded(0), guarded(1), guarded(2),
           escapes(1), overlapping_store(1), overlapping_store(0x100));
}
