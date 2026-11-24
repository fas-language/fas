#include <stdio.h>

static unsigned int update(unsigned int value)
{
    switch (sizeof(value)) {
    case 1:
        asm("incb %0" : "+r"(value));
        break;
    case 4:
        asm("incl %0" : "+r"(value));
        break;
    case 8:
        asm("incq %0" : "+r"(value));
        break;
    }
    return value;
}

static int jump_into_switch(int jump)
{
    int result = 0;
    if (jump)
        goto entered;
    switch (-3) {
    case -5 ... -2:
        result = 2;
        // fall through
    case 0:
entered:
        result += 5;
        break;
    default:
        result = 90;
    }
    return result;
}

static int switch_after_return(int jump)
{
    if (jump)
        goto entered;
    return 11;
    switch (jump) {
    default:
entered:
        return 22;
    }
}

int main(void)
{
    printf("%u %d %d\n", update(40), jump_into_switch(0), jump_into_switch(1));
    printf("%d %d\n", switch_after_return(0), switch_after_return(1));
    return update(40) != 41 || jump_into_switch(0) != 7 || jump_into_switch(1) != 5
        || switch_after_return(0) != 11 || switch_after_return(1) != 22;
}
