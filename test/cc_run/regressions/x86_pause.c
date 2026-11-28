#include <stdio.h>

#if !__has_builtin(__builtin_ia32_pause)
#error expected x86 pause builtin
#endif

int main(void) {
    __builtin_ia32_pause();
    puts("paused");
}
