#include <stdio.h>

static int wait_result(int which)
{
    if (which) {
        return ({
            __label__ done;
            long value = 13;
            if (which > 1)
                goto done;
            value += 4;
        done:
            value;
        });
    }
    return ({
        __label__ done;
        int value = 29;
        if (which == 0)
            goto done;
        value += 2;
    done:
        value;
    });
}

int main(void)
{
    printf("%d %d %d\n", wait_result(0), wait_result(1), wait_result(2));
    return wait_result(0) != 29 || wait_result(1) != 17 || wait_result(2) != 13;
}
