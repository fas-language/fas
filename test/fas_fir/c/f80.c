#define _GNU_SOURCE
/* Checks test/fas_fir/c/f80.fir against clang's long double (x87
   extended precision); exit 0 when all agree */
#include <math.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
typedef long double LD;
LD f80_add(LD, LD), f80_sub(LD, LD), f80_mul(LD, LD), f80_div(LD, LD), f80_rem(LD, LD);
LD f80_fma(LD, LD, LD);
LD f80_min(LD, LD), f80_max(LD, LD), f80_copysign(LD, LD);
LD f80_neg(LD), f80_abs(LD), f80_sqrt(LD), f80_floor(LD), f80_ceil(LD), f80_trunc(LD), f80_roundeven(LD);
int f80_cmp(LD, LD);
int f80_to_i32(LD); long f80_to_i64(LD); unsigned f80_to_u32(LD); unsigned long f80_to_u64(LD);
signed char f80_sat_i8(LD); long f80_sat_i64(LD); unsigned short f80_sat_u16(LD); unsigned long f80_sat_u64(LD);
LD i32_to_f80(int), i64_to_f80(long), u32_to_f80(unsigned), u64_to_f80(unsigned long);
double f80_to_f64(LD); float f80_to_f32(LD); LD f64_to_f80(double), f32_to_f80(float);
LD f80_select(int, LD, LD), f80_fib(LD, LD, int), f80_scale(LD *, long, LD);
LD f80_many(int, LD, double, LD, long, LD, LD, int, LD, float, LD);
LD f80_calls_c(LD, LD), f80_vsum(int, ...);

LD c_mix(LD a, double d, LD b, int i) { return a * d - b / i; }
LD c_vsum(int n, ...) {
  va_list ap; va_start(ap, n);
  LD a = va_arg(ap, LD); double d = va_arg(ap, double); LD b = va_arg(ap, LD);
  va_end(ap);
  return n * a + d * b;
}

static int bad = 0;
/* bitwise on the 10 significant bytes; any NaN equals any NaN */
static int same(LD x, LD y) { return (isnan(x) && isnan(y)) || memcmp(&x, &y, 10) == 0; }
static void check(const char *what, LD x, LD y, LD got, LD want) {
  if (!same(got, want)) {
    printf("%s(%La, %La): got %La want %La\n", what, x, y, got, want); bad = 1;
  }
}
static void checki(const char *what, LD x, long got, long want) {
  if (got != want) { printf("%s(%La): got %ld want %ld\n", what, x, got, want); bad = 1; }
}
/* IEEE minimumNumber / maximumNumber */
static LD minnum(LD a, LD b) {
  if (isnan(a)) return b; if (isnan(b)) return a;
  if (a == b) return signbit(a) ? a : b; return a < b ? a : b;
}
static LD maxnum(LD a, LD b) {
  if (isnan(a)) return b; if (isnan(b)) return a;
  if (a == b) return signbit(a) ? b : a; return a > b ? a : b;
}
static int cmp(LD a, LD b) {
  int u = isunordered(a, b), r = 0, k = 0;
  r |= (!u && a == b) << k++; r |= (!u && a > b) << k++; r |= (!u && a >= b) << k++;
  r |= (!u && a < b) << k++; r |= (!u && a <= b) << k++; r |= (!u && a != b) << k++;
  r |= (!u) << k++; r |= (u || a == b) << k++; r |= (u || a > b) << k++; r |= (u || a >= b) << k++;
  r |= (u || a < b) << k++; r |= (u || a <= b) << k++; r |= (u || a != b) << k++; r |= u << k++;
  return r | 1 << 14;
}
static long sat(LD x, LD lo, LD hi) { return isnan(x) ? 0 : x <= lo ? (long)lo : x >= hi ? (long)hi : (long)x; }
static unsigned long satu64(LD x) {
  return isnan(x) || x <= 0 ? 0 : x >= 0x1p64L ? UINT64_MAX : (unsigned long)x;
}

int main(void) {
  LD v[] = {0.0L, -0.0L, 1.0L, -1.5L, 3.25L, 0.1L, -7.5L, 2.5L, 3.5L, -2.5L, 123456789.987654321L,
            0x1p63L, 0x1p64L - 1, 1e4000L, -1e4000L, 0x1p-16440L, 1e-300L, INFINITY, -INFINITY, NAN,
            0x1.fffffffffffffffep16383L, 1e20L, -1e20L, 300.0L, -200.0L, 65535.5L, 70000.0L};
  int n = sizeof v / sizeof v[0];
  for (int i = 0; i < n; i++) {
    LD x = v[i];
    for (int j = 0; j < n; j++) {
      LD y = v[j];
      check("add", x, y, f80_add(x, y), x + y);
      check("sub", x, y, f80_sub(x, y), x - y);
      check("mul", x, y, f80_mul(x, y), x * y);
      check("div", x, y, f80_div(x, y), x / y);
      check("rem", x, y, f80_rem(x, y), fmodl(x, y));
      check("min", x, y, f80_min(x, y), minnum(x, y));
      check("max", x, y, f80_max(x, y), maxnum(x, y));
      check("copysign", x, y, f80_copysign(x, y), copysignl(x, y));
      check("select1", x, y, f80_select(1, x, y), x);
      check("select0", x, y, f80_select(0, x, y), y);
      checki("cmp", x, f80_cmp(x, y), cmp(x, y));
    }
    check("neg", x, 0, f80_neg(x), -x);
    check("abs", x, 0, f80_abs(x), fabsl(x));
    check("sqrt", x, 0, f80_sqrt(x), sqrtl(x));
    check("floor", x, 0, f80_floor(x), floorl(x));
    check("ceil", x, 0, f80_ceil(x), ceill(x));
    check("trunc", x, 0, f80_trunc(x), truncl(x));
    check("roundeven", x, 0, f80_roundeven(x), roundevenl(x));
    check("f64", x, 0, f80_to_f64(x), (double)x);
    check("f32", x, 0, f80_to_f32(x), (float)x);
    check("from f64", x, 0, f64_to_f80((double)x), (LD)(double)x);
    check("from f32", x, 0, f32_to_f80((float)x), (LD)(float)x);
    if (x > -0x1p31L && x < 0x1p31L) checki("i32", x, f80_to_i32(x), (int)x);
    if (x > -0x1p63L && x < 0x1p63L) checki("i64", x, f80_to_i64(x), (long)x);
    if (x > -1 && x < 0x1p32L) checki("u32", x, f80_to_u32(x), (unsigned)x);
    if (x > -1 && x < 0x1p64L) checki("u64", x, (long)f80_to_u64(x), (long)(unsigned long)x);
    checki("sat i8", x, f80_sat_i8(x), sat(x, -128, 127));
    checki("sat i64", x, f80_sat_i64(x), isnan(x) ? 0 : x <= -0x1p63L ? INT64_MIN : x >= 0x1p63L ? INT64_MAX : (long)x);
    checki("sat u16", x, f80_sat_u16(x), sat(x, 0, 65535));
    checki("sat u64", x, (long)f80_sat_u64(x), (long)satu64(x));
  }
  long iv[] = {0, 1, -1, 42, -123456789, INT32_MAX, INT32_MIN, INT64_MAX, INT64_MIN, 0x7fffffffffffffffL - 1000,
               (long)0x8000000000000001UL, (long)0xffffffffffffffffUL, (long)0xfffffffffffff801UL};
  for (int i = 0; i < (int)(sizeof iv / sizeof iv[0]); i++) {
    long k = iv[i];
    check("from i32", k, 0, i32_to_f80((int)k), (LD)(int)k);
    check("from i64", k, 0, i64_to_f80(k), (LD)k);
    check("from u32", k, 0, u32_to_f80((unsigned)k), (LD)(unsigned)k);
    check("from u64", k, 0, u64_to_f80((unsigned long)k), (LD)(unsigned long)k);
  }
  /* loops, memory, the ABI */
  {
    LD a = 0.1L, b = 0.2L, want_a = a, want_b = b;
    for (int i = 0; i < 50; i++) { LD s = want_a + want_b; want_a = want_b; want_b = s; }
    check("fib", a, b, f80_fib(a, b, 50), want_a);
  }
  {
    LD arr[7], ref[7], sum = 0;
    for (int i = 0; i < 7; i++) arr[i] = ref[i] = 1.0L / (i + 3);
    LD k = 3.0L / 7;
    for (int i = 0; i < 7; i++) { ref[i] = ref[i] * k + 0.5L; sum += ref[i]; }
    check("scale", k, 0, f80_scale(arr, 7, k), sum);
    for (int i = 0; i < 7; i++) check("scale[i]", i, 0, arr[i], ref[i]);
  }
  {
    LD a = 1.25L, b = 0.1L, c = -3.5L, e = 7.0L / 3, f = 1e-3L, g = 1e30L;
    double d = 2.75; long l = -99; float s = 0.375f;
    LD t = a * 4 + b; t = t - d; t = t * c; t = t + l; t = t / e; t = t + 11; t = t * f; t = t - s; t = t + g;
    check("many", a, b, f80_many(4, a, d, b, l, c, e, 11, f, s, g), t);
  }
  {
    LD a = 2.0L / 3, b = -0.7L;
    LD r = c_mix(a, 1.5, b, 7);
    LD want = c_vsum(3, r, 2.0, a) + b;
    check("calls_c", a, b, f80_calls_c(a, b), want);
  }
  {
    LD x = 0.3L, y = -1e10L, z = 1.0L / 7;
    check("vsum", x, y, f80_vsum(3, x, 2, y, -3, z, 5), x * 2 + y * -3 + z * 5);
    /* past the six integer registers: still from the overflow area */
    check("vsum8", x, y, f80_vsum(8, x, 1, y, 2, z, 3, x, 4, y, 5, z, 6, x, 7, y, 8),
          ((((((x * 1 + y * 2) + z * 3) + x * 4) + y * 5) + z * 6) + x * 7) + y * 8);
  }
  /* fma: every triple of the table, then random operands built to hit
     cancellation, sticky bits, ties and the subnormal range */
  for (int i = 0; i < n; i++)
    for (int j = 0; j < n; j++)
      for (int k = 0; k < n; k++) {
        LD z = v[k], got = f80_fma(v[i], v[j], z), want = fmal(v[i], v[j], z);
        if (!same(got, want)) {
          printf("fma(%La, %La, %La): got %La want %La\n", v[i], v[j], z, got, want); bad = 1;
        }
      }
  {
    unsigned long s = 88172645463325252UL;
    int nbad = 0;
    for (int t = 0; t < 400000 && nbad < 10; t++) {
      LD op[3];
      for (int q = 0; q < 3; q++) {
        s ^= s << 13; s ^= s >> 7; s ^= s << 17;
        unsigned long m = s | 1UL << 63;
        s ^= s << 13; s ^= s >> 7; s ^= s << 17;
        int mode = s & 7, ex;
        if (mode < 4) ex = 16383 + (int)((s >> 8) % 160) - 80;          /* near 1 */
        else if (mode < 6) ex = 1 + (int)((s >> 8) % 300);              /* low end */
        else ex = (int)((s >> 8) % 32767);                              /* anywhere */
        if (((s >> 20) & 15) == 0) m &= ~0UL << ((s >> 24) % 64);    /* few bits: ties */
        unsigned short se = ex | (s >> 40 & 1) << 15;
        if (ex == 0) m &= ~(1UL << 63);
        memcpy(&op[q], &m, 8); memcpy((char *)&op[q] + 8, &se, 2);
      }
      s ^= s << 13; s ^= s >> 7; s ^= s << 17;
      /* z close to -(x * y): massive cancellation */
      if ((s & 3) == 0) { op[2] = -(op[0] * op[1]); if (s & 4) op[2] = nextafterl(op[2], 0); }
      LD got = f80_fma(op[0], op[1], op[2]), want = fmal(op[0], op[1], op[2]);
      if (!same(got, want)) {
        printf("fma(%La, %La, %La): got %La want %La\n", op[0], op[1], op[2], got, want); bad = 1; nbad++;
      }
    }
  }
  return bad;
}
