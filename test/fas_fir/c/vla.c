/* Checks test/fas_fir/c/vla.fir: variable-size alloca and stacksave/stackrestore */
#include <stdint.h>
#include <stdio.h>
int64_t fir_vla_sum(int64_t), fir_vla_align(int64_t, int64_t), fir_vla_loop(int64_t, int64_t), fir_vla_zero(void);
static int misaligned, bad;
#define FAIL(...) do { printf(__VA_ARGS__); bad = 1; } while (0)

/* at entry rsp+8 is 16-aligned, so after `push rbp` the frame address is */
int64_t c_fill(int64_t *p, int64_t n, int64_t a, int64_t b, int64_t c, int64_t d, int64_t e, int64_t f, double g) {
  if ((uintptr_t)__builtin_frame_address(0) & 15) misaligned = 1;
  char buf[64];
  snprintf(buf, sizeof buf, "%f", g); /* glibc's movaps would fault on a misaligned stack */
  for (int64_t i = 0; i < n; i++) p[i] = 3 * i + 1;
  return a + b + c + d + e + f + (int64_t)g; /* 21 + 1 */
}
int64_t c_touch(int64_t *p, int64_t n, int64_t want) {
  if ((uintptr_t)__builtin_frame_address(0) & 15) misaligned = 1;
  return p[n - 1] == want;
}

int main(void) {
  for (int64_t n = 0; n <= 70; n += 7) {
    int64_t want = 3 * n * (n - 1) / 2 + n + 22;
    if (fir_vla_sum(n) != want) FAIL("sum %lld: %lld\n", (long long)n, (long long)fir_vla_sum(n));
  }
  for (int64_t pre = 0; pre < 40; pre += 3)
    for (int64_t n = 1; n < 200; n += 37) {
      int64_t r = fir_vla_align(pre, n);
      if (r != 0) FAIL("align pre=%lld n=%lld: %lld\n", (long long)pre, (long long)n, (long long)r);
    }
  /* 200000 iterations of a 4 KiB VLA: 800 MB if never freed */
  int64_t r = fir_vla_loop(200000, 512);
  if (r != 0) FAIL("loop: %lld\n", (long long)r);
  if (fir_vla_zero() != 7) FAIL("zero\n");
  if (misaligned) FAIL("C saw a misaligned stack\n");
  if (!bad) printf("ok\n");
  return bad;
}
