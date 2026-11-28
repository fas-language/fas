#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

static void *worker(void *arg) {
    long *value = arg;
    *value += 5;
    return arg;
}

int main(void) {
    char *end;
    long value = strtol("37", &end, 10);
    pthread_t thread;
    void *result;
    if (*end || pthread_create(&thread, NULL, worker, &value) ||
        pthread_join(thread, &result) || result != &value)
        return 1;
    printf("musl result: %ld\n", value);
    return value != 42;
}
