#include <stdio.h>

// BusyBox relies on these references disappearing even at -O0.
extern void not_linked(void);
static int constant_if(void) {
    if (0) not_linked();
    if (1) return 5;
    else not_linked();
    return 0;
}

static int jump_in(void) {
    goto label;
    if (0) {
label:
        return 21;
    }
    return 0;
}

static int case_inside_if(int n) {
    switch (n) {
        if (0) {
        case 1: return 17;
        }
    }
    return 0;
}

int main(void) {
    printf("%d %d %d\n", constant_if(), jump_in(), case_inside_if(1));
}
