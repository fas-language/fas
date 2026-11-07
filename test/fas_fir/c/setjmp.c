/* Checks test/fas_fir/c/setjmp.fir: setjmp/longjmp across Fir frames;
   exit 0 when the results agree with the same code in C */
#include <setjmp.h>
#include <stdio.h>
long fir_sj1(long, long), fir_sj_loop(long);

/* noinline, and it clobbers the callee-saved registers it may use */
__attribute__((noinline)) long c_touch(long x) {
  __asm__ volatile("" ::: "rbx", "r12", "r13", "r14", "r15", "memory");
  return x * 5 + 1;
}
void c_jump(void *jb, int v) { longjmp(*(jmp_buf *)jb, v); }

static long want_sj1(long a, long b) {
  long x = a * 3, y = b + 7, t = x * 5 + 1;
  long p = y * 5 + 1, q = p * 5 + 1, u = (p + q) * 5 + 1;
  int r = (int)u;
  if (r == 0) r = 1; /* longjmp(.., 0) returns 1 */
  return x + y * 100 + (long)r * 10000 + t;
}
static long want_loop(long n) {
  long v[13];
  for (int i = 1; i <= 12; i++) v[i] = i % 2 ? n + i : n * i;
  long a = 0;
  for (int i = 1; i <= 11; i++) a += v[i];
  a *= v[12];
  double f1 = (double)n, f2 = f1 * 1.5;
  long c = n > 0 ? n : 0, r = 0;
  if (n > 0) {
    long w1 = n * 5 + 1, w2 = w1 * 5 + 1, w4 = (w1 * w2) * 5 + 1;
    r = (int)w4;
    if (r == 0) r = 1;
  }
  return a + (long)(f1 + f2) + c * 1000000 + r * 7;
}

int main(void) {
  int bad = 0;
  long ab[][2] = {{1, 2}, {0, 0}, {-5, 17}, {123456, -98765}, {7, 7}};
  for (unsigned i = 0; i < sizeof ab / sizeof ab[0]; i++) {
    long got = fir_sj1(ab[i][0], ab[i][1]), want = want_sj1(ab[i][0], ab[i][1]);
    if (got != want) { printf("sj1(%ld, %ld): got %ld want %ld\n", ab[i][0], ab[i][1], got, want); bad = 1; }
  }
  for (long n = 0; n < 6; n++) {
    long got = fir_sj_loop(n), want = want_loop(n);
    if (got != want) { printf("sj_loop(%ld): got %ld want %ld\n", n, got, want); bad = 1; }
  }
  return bad;
}
