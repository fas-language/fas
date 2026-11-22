#include <dlfcn.h>
#include <stdio.h>

int exported_answer(void) { return 42; }

int main(void) {
    int (*answer)(void) = (int (*)(void))dlsym(RTLD_DEFAULT, "exported_answer");
    if (!answer)
        return 1;
    printf("%d\n", answer());
    return answer() != 42;
}
