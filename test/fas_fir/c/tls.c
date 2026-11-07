/* Checks test/fas_fir/c/tls.fir from four threads and the main thread:
   each sees its own fir_tls (starting at 7) and c_tls (starting at 100),
   and the same addresses as C does; exit 0 when all agree */
#include <pthread.h>
#include <stdio.h>
__thread long c_tls = 100;
extern __thread long fir_tls, fir_tls_alias;
extern long __start_fir_reg[], __stop_fir_reg[];
long fir_bump(long);
long *fir_addr(void);
long fir_add_c(long);
static int bad;
#define CHECK(e, want) do { long g_ = (long)(e); if (g_ != (long)(want)) { \
    printf("thread %ld: %s = %ld, expected %ld\n", k, #e, g_, (long)(want)); bad = 1; } } while (0)
static void *run(void *arg) {
  long k = (long)arg;
  CHECK(fir_tls, 7);
  CHECK(fir_bump(k), 7 + k);
  CHECK(fir_bump(1), 8 + k);
  CHECK(fir_tls, 8 + k);
  CHECK(fir_tls_alias, 8 + k);
  CHECK(fir_addr(), &fir_tls);
  CHECK(fir_add_c(k * 10), 100 + k * 10);
  CHECK(c_tls, 100 + k * 10);
  return 0;
}
int main(void) {
  long k = -1;
  pthread_t t[4];
  for (long i = 0; i < 4; i++) pthread_create(&t[i], 0, run, (void *)(i + 1));
  for (int i = 0; i < 4; i++) pthread_join(t[i], 0);
  run((void *)0);
  CHECK(__stop_fir_reg - __start_fir_reg, 2);
  CHECK(__start_fir_reg[0] + __start_fir_reg[1], 3);
  return bad;
}
