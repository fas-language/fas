/* Checks test/fas_fir/c/promises.fir: calls to declarations with
   memory(...) and willreturn promises, kept by these definitions; exit 0
   when fir_run returns 38 */
#include <stdio.h>
long fir_run(void);
long c_sq(long x) { return x * x; }
long c_sum(const long *p, long n) {
  long s = 0;
  for (long i = 0; i < n; i++) s += p[i];
  return s;
}
void c_bump(long *p) { *p += 1; }
int main(void) {
  long r = fir_run();
  if (r != 38) { printf("fir_run() = %ld, expected 38\n", r); return 1; }
  return 0;
}
