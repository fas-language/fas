/* Checks test/fas_fir/c/varargs.fir against the same sums in C; exit 0 when they agree */
#include <stdarg.h>
#include <stdio.h>
long fir_vsum(int n, ...);
long fir_vlist(int n, va_list ap);
long c_vlist(int n, va_list ap) {
  long acc = 0;
  for (int i = 0; i < n; i++) { long x = va_arg(ap, long); double f = va_arg(ap, double); acc = acc * 7 + x - (long)f; }
  return acc;
}
long via_fir(int n, ...) { va_list ap; va_start(ap, n); long r = fir_vlist(n, ap); va_end(ap); return r; }
long via_c(int n, ...) { va_list ap; va_start(ap, n); long r = c_vlist(n, ap); va_end(ap); return r; }
#define ARGS 9, 1L, 2.0, 30L, 4.5, -5L, 60.0, 7L, -8.0, 9L, 10.0, 11L, 12.0, 13L, 1.0e3, 15L, 16.0, 170L, 18.0
int main(void) {
  long a = fir_vsum(ARGS), b = via_fir(ARGS), c = via_c(ARGS);
  printf("%ld %ld %ld\n", a, b, c);
  return !(a == c && b == c);
}
