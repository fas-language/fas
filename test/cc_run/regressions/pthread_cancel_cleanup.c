#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

static int ready[2], blocked[2], trace;

static void record(int *value)
{
    trace = trace * 10 + *value;
}

static int wait_for_cancel(void)
{
    char c;
    if (write(ready[1], ".", 1) != 1)
        abort();
    if (read(blocked[0], &c, 1) != 1)
        abort();
    return 4;
}

static void *worker(void *unused)
{
    {
        int done __attribute__((cleanup(record))) = 1;
    }
    int outer __attribute__((cleanup(record))) = 2;
    {
        int inner __attribute__((cleanup(record))) = 3;
        int initializing __attribute__((cleanup(record))) = wait_for_cancel();
        abort();
    }
}

int main(void)
{
    pthread_t thread;
    if (pipe(ready) || pipe(blocked) || pthread_create(&thread, NULL, worker, NULL))
        abort();
    char signal;
    void *result;
    if (read(ready[0], &signal, 1) != 1 ||
        pthread_cancel(thread) || pthread_join(thread, &result))
        abort();
    printf("canceled=%d cleanups=%d\n", result == PTHREAD_CANCELED, trace);
    return result != PTHREAD_CANCELED || trace != 132;
}
