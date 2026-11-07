/* Checks test/fas_fir/c/tlsgd.fir, built as a shared library and loaded
   with dlopen (argv[1]) while threads already run: each thread sees its own
   copies of the library's thread-locals, including a 1 MiB one no static
   TLS block has room for, and of the executable's c_tls; exit 0 when all
   agree */
#include <dlfcn.h>
#include <pthread.h>
#include <stdio.h>
__thread long c_tls = 100;
static long (*gd_bump)(long), *(*gd_addr)(void), (*gd_big_sum)(long), (*gd_add_c)(long);
static void *handle;
static int bad;
static pthread_barrier_t loaded;
#define CHECK(e, want) do { long g_ = (long)(e); if (g_ != (long)(want)) { \
    printf("thread %ld: %s = %ld, expected %ld\n", k, #e, g_, (long)(want)); bad = 1; } } while (0)
static void run(long k) {
  CHECK(gd_bump(k), 7 + k);
  CHECK(gd_bump(1), 8 + k);
  long *mine = (long *)dlsym(handle, "gd_tls"); /* this thread's copy */
  CHECK(gd_addr(), mine);
  CHECK(*mine, 8 + k);
  long want = 0;
  for (long i = 0; i < 131072; i += 4096) want += k + i;
  CHECK(gd_big_sum(k), want + k + 126976);
  CHECK(gd_add_c(k * 10), 100 + k * 10);
  CHECK(c_tls, 100 + k * 10);
}
static void *thread(void *arg) {
  pthread_barrier_wait(&loaded); /* started before the dlopen */
  long k = (long)arg;
  run(k);
  return 0;
}
int main(int argc, char **argv) {
  long k = -1;
  if (argc < 2) return 2;
  pthread_t t[4];
  pthread_barrier_init(&loaded, 0, 5);
  for (long i = 0; i < 4; i++) pthread_create(&t[i], 0, thread, (void *)(i + 1));
  void *h = handle = dlopen(argv[1], RTLD_NOW);
  if (!h) { printf("dlopen: %s\n", dlerror()); return 1; }
  gd_bump = (long (*)(long))dlsym(h, "gd_bump");
  gd_addr = (long *(*)(void))dlsym(h, "gd_addr");
  gd_big_sum = (long (*)(long))dlsym(h, "gd_big_sum");
  gd_add_c = (long (*)(long))dlsym(h, "gd_add_c");
  if (!gd_bump || !gd_addr || !gd_big_sum || !gd_add_c) { printf("dlsym failed\n"); return 1; }
  pthread_barrier_wait(&loaded);
  for (int i = 0; i < 4; i++) pthread_join(t[i], 0);
  k = 0;
  run(0);
  return bad;
}
