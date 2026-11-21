#include <dlfcn.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef uint64_t (*u64_fn)(void);
typedef void (*set_u64_fn)(uint64_t);
typedef void (*set_slot_fn)(uint64_t, uint64_t);
typedef void *(*addr_fn)(void);

static u64_fn dso_get_init;
static set_u64_fn dso_set_init;
static u64_fn dso_get_zero;
static set_u64_fn dso_set_zero;
static u64_fn dso_get_slot_sum;
static set_slot_fn dso_set_slot;
static addr_fn dso_addr;

static void *addrs[4];
static int results[4];

static void *worker(void *arg) {
    int id = *(int *)arg;
    if (dso_get_init() != 0xfeedfacecafebeefULL) { results[id] = 1; return NULL; }
    if (dso_get_zero() != 0) { results[id] = 2; return NULL; }
    if (dso_get_slot_sum() != 33) { results[id] = 3; return NULL; }
    dso_set_init(100 + (uint64_t)id);
    dso_set_zero(200 + (uint64_t)id);
    dso_set_slot(300 + (uint64_t)id, 400 + (uint64_t)id);
    addrs[id] = dso_addr();
    for (int i = 0; i < 10000; i++) {
        if (dso_get_init() != 100 + (uint64_t)id) { results[id] = 4; return NULL; }
        if (dso_get_zero() != 200 + (uint64_t)id) { results[id] = 5; return NULL; }
        if (dso_get_slot_sum() != 700 + 2 * (uint64_t)id) { results[id] = 6; return NULL; }
    }
    results[id] = 0;
    return NULL;
}

int main(int argc, char **argv) {
    if (argc != 2) { fprintf(stderr, "usage: host path/to/lib.so\n"); return 2; }
    void *h = dlopen(argv[1], RTLD_NOW);
    if (!h) { fprintf(stderr, "dlopen failed: %s\n", dlerror()); return 3; }
    dso_get_init = (u64_fn)dlsym(h, "dso_get_init");
    dso_set_init = (set_u64_fn)dlsym(h, "dso_set_init");
    dso_get_zero = (u64_fn)dlsym(h, "dso_get_zero");
    dso_set_zero = (set_u64_fn)dlsym(h, "dso_set_zero");
    dso_get_slot_sum = (u64_fn)dlsym(h, "dso_get_slot_sum");
    dso_set_slot = (set_slot_fn)dlsym(h, "dso_set_slot");
    dso_addr = (addr_fn)dlsym(h, "dso_addr");
    if (!dso_get_init || !dso_set_init || !dso_get_zero || !dso_set_zero ||
        !dso_get_slot_sum || !dso_set_slot || !dso_addr) {
        fprintf(stderr, "dlsym failed\n");
        return 4;
    }
    pthread_t ts[4];
    int ids[4] = {0, 1, 2, 3};
    for (int i = 0; i < 4; i++) {
        results[i] = -1;
        if (pthread_create(&ts[i], NULL, worker, &ids[i]) != 0) return 5;
    }
    for (int i = 0; i < 4; i++) pthread_join(ts[i], NULL);
    for (int i = 0; i < 4; i++) {
        if (results[i] != 0) { fprintf(stderr, "worker %d failed: %d\n", i, results[i]); return 10 + i; }
        for (int j = 0; j < i; j++) {
            if (addrs[i] == addrs[j]) { fprintf(stderr, "duplicate tls addr\n"); return 20; }
        }
    }
    dlclose(h);
    return 0;
}
