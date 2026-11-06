/* Checks test/fas_fir/c/ops.fir (frem against libm fmod, bit for bit) (generated) against clang on edge values; exit 0 when all agree */
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
static int bad = 0;
#define FAIL(...) do { printf(__VA_ARGS__); bad = 1; } while (0)

/* ---- integers ---- */
#define DECL(T, t)                                                              \
  T fir_abs_##t(T); T fir_bitrev_##t(T);                                        \
  int fir_sadd_ovf_##t(T, T, T *); int fir_uadd_ovf_##t(T, T, T *);             \
  int fir_ssub_ovf_##t(T, T, T *); int fir_usub_ovf_##t(T, T, T *);             \
  int fir_smul_ovf_##t(T, T, T *); int fir_umul_ovf_##t(T, T, T *);
DECL(int8_t, i8) DECL(int16_t, i16) DECL(int32_t, i32) DECL(int64_t, i64)
int16_t fir_bswap_i16(int16_t); int32_t fir_bswap_i32(int32_t); int64_t fir_bswap_i64(int64_t);

static uint64_t rev(uint64_t x, int w) { uint64_t r = 0; for (int i = 0; i < w; i++) r |= ((x >> i) & 1) << (w - 1 - i); return r; }
static uint64_t bsw(uint64_t x, int w) { uint64_t r = 0; for (int i = 0; i < w / 8; i++) r |= ((x >> (8 * i)) & 0xff) << (w - 8 - 8 * i); return r; }

static const int64_t ivals[] = { 0, 1, -1, 2, -2, 3, 7, 100, -100, 127, -128, 128, 255, 256, 0x7fff, -0x8000, 0xffff,
  0x12345678, 0x7fffffff, -0x7fffffff - 1, 0xffffffff, 0x123456789abcdef0, INT64_MAX, INT64_MIN, INT64_MIN + 1,
  0x0102030405060708, 3037000499, 3037000500, 65535, 46341, 0x5555555555555555 };
#define NV (int)(sizeof ivals / sizeof ivals[0])

#define CHECK_INT(T, U, t, w)                                                                       \
  for (int i = 0; i < NV; i++) {                                                                    \
    T a = (T)ivals[i];                                                                              \
    T ab = a < 0 ? (T)(0 - (U)a) : a;                                                               \
    if (fir_abs_##t(a) != ab) FAIL("abs " #t " %lld\n", (long long)a);                              \
    if ((U)fir_bitrev_##t(a) != (U)rev((U)a, w)) FAIL("bitrev " #t " %lld\n", (long long)a);        \
    for (int j = 0; j < NV; j++) {                                                                  \
      T b = (T)ivals[j], r, want; U ur, uwant; int f, wf;                                           \
      f = fir_sadd_ovf_##t(a, b, &r); wf = __builtin_add_overflow(a, b, &want);                     \
      if (f != wf || r != want) FAIL("sadd " #t " %lld %lld\n", (long long)a, (long long)b);        \
      f = fir_ssub_ovf_##t(a, b, &r); wf = __builtin_sub_overflow(a, b, &want);                     \
      if (f != wf || r != want) FAIL("ssub " #t " %lld %lld\n", (long long)a, (long long)b);        \
      f = fir_smul_ovf_##t(a, b, &r); wf = __builtin_mul_overflow(a, b, &want);                     \
      if (f != wf || r != want) FAIL("smul " #t " %lld %lld\n", (long long)a, (long long)b);        \
      f = fir_uadd_ovf_##t(a, b, (T *)&ur); wf = __builtin_add_overflow((U)a, (U)b, &uwant);        \
      if (f != wf || ur != uwant) FAIL("uadd " #t " %lld %lld\n", (long long)a, (long long)b);      \
      f = fir_usub_ovf_##t(a, b, (T *)&ur); wf = __builtin_sub_overflow((U)a, (U)b, &uwant);        \
      if (f != wf || ur != uwant) FAIL("usub " #t " %lld %lld\n", (long long)a, (long long)b);      \
      f = fir_umul_ovf_##t(a, b, (T *)&ur); wf = __builtin_mul_overflow((U)a, (U)b, &uwant);        \
      if (f != wf || ur != uwant) FAIL("umul " #t " %lld %lld\n", (long long)a, (long long)b);      \
    }                                                                                               \
  }

/* ---- floats ---- */
#define FDECL(T, t)                                                                                \
  int fir_fcmp_false_##t(T, T); int fir_fcmp_oeq_##t(T, T); int fir_fcmp_ogt_##t(T, T);            \
  int fir_fcmp_oge_##t(T, T); int fir_fcmp_olt_##t(T, T); int fir_fcmp_ole_##t(T, T);              \
  int fir_fcmp_one_##t(T, T); int fir_fcmp_ord_##t(T, T); int fir_fcmp_ueq_##t(T, T);              \
  int fir_fcmp_ugt_##t(T, T); int fir_fcmp_uge_##t(T, T); int fir_fcmp_ult_##t(T, T);              \
  int fir_fcmp_ule_##t(T, T); int fir_fcmp_une_##t(T, T); int fir_fcmp_uno_##t(T, T);              \
  int fir_fcmp_true_##t(T, T);                                                                     \
  T fir_copysign_##t(T, T); T fir_frem_##t(T, T); T fir_fmin_##t(T, T); T fir_fmax_##t(T, T); T fir_ftrunc_##t(T);
FDECL(float, f32) FDECL(double, f64)

static const double fvals[] = { 0.0, -0.0, 1.0, -1.0, 0.5, -0.5, 1.5, -2.5, 0.49999999999999994, 3.0, 1e300, -1e300,
  4503599627370495.5, -4503599627370495.5, 4503599627370496.0, 9007199254740993.0, 8388607.5, -8388607.5, 8388608.0,
  16777217.0, 1e-310, -1e-310, 2147483648.5, -2147483649.5, 9.2e18, -9.3e18, INFINITY, -INFINITY, NAN, -NAN };
#define NF (int)(sizeof fvals / sizeof fvals[0])

/* IEEE minimumNumber/maximumNumber: NaN loses, -0 < +0 */
#define REF_MIN(a, b) (isnan(a) ? (b) : isnan(b) ? (a) : (a) < (b) ? (a) : (b) < (a) ? (b) : signbit(a) ? (a) : (b))
#define REF_MAX(a, b) (isnan(a) ? (b) : isnan(b) ? (a) : (a) > (b) ? (a) : (b) > (a) ? (b) : signbit(a) ? (b) : (a))

#define SAME(T, U, x, y) (isnan(x) ? isnan(y) : ({ T x_ = (x), y_ = (y); U ux, uy; memcpy(&ux, &x_, sizeof ux); memcpy(&uy, &y_, sizeof uy); ux == uy; }))

#define CHECK_FLOAT(T, U, t, truncf_, fmod_)                                                                     \
  for (int i = 0; i < NF; i++) {                                                                          \
    T a = (T)fvals[i];                                                                                    \
    if (!SAME(T, U, fir_ftrunc_##t(a), truncf_(a))) FAIL("ftrunc " #t " %g\n", (double)a);                \
    for (int j = 0; j < NF; j++) {                                                                        \
      T b = (T)fvals[j];                                                                                  \
      int un = isnan(a) || isnan(b);                                                                      \
      int want[16] = { 0, a == b, a > b, a >= b, a < b, a <= b, a < b || a > b, !un, !(a < b || a > b),   \
                       !(a <= b), !(a < b), !(a >= b), !(a > b), a != b, un, 1 };                         \
      int got[16] = { fir_fcmp_false_##t(a, b), fir_fcmp_oeq_##t(a, b), fir_fcmp_ogt_##t(a, b),           \
                      fir_fcmp_oge_##t(a, b), fir_fcmp_olt_##t(a, b), fir_fcmp_ole_##t(a, b),             \
                      fir_fcmp_one_##t(a, b), fir_fcmp_ord_##t(a, b), fir_fcmp_ueq_##t(a, b),             \
                      fir_fcmp_ugt_##t(a, b), fir_fcmp_uge_##t(a, b), fir_fcmp_ult_##t(a, b),             \
                      fir_fcmp_ule_##t(a, b), fir_fcmp_une_##t(a, b), fir_fcmp_uno_##t(a, b),             \
                      fir_fcmp_true_##t(a, b) };                                                          \
      for (int k = 0; k < 16; k++)                                                                        \
        if (got[k] != want[k]) FAIL("fcmp #%d " #t " %g %g: %d\n", k, (double)a, (double)b, got[k]);     \
      if (!SAME(T, U, fir_copysign_##t(a, b), copysign(a, b))) FAIL("copysign " #t " %g %g\n", (double)a, (double)b); \
      if (!SAME(T, U, fir_frem_##t(a, b), fmod_(a, b))) FAIL("frem " #t " %g %g\n", (double)a, (double)b); \
      if (!SAME(T, U, fir_fmin_##t(a, b), REF_MIN(a, b))) FAIL("fmin " #t " %g %g\n", (double)a, (double)b);   \
      if (!SAME(T, U, fir_fmax_##t(a, b), REF_MAX(a, b))) FAIL("fmax " #t " %g %g\n", (double)a, (double)b);   \
    }                                                                                                     \
  }

void fir_memcpy(void *, const void *, long); void fir_memmove(void *, const void *, long);
void fir_memset(void *, int, int); void fir_memset7(void *);

int main(void) {
  CHECK_INT(int8_t, uint8_t, i8, 8) CHECK_INT(int16_t, uint16_t, i16, 16)
  CHECK_INT(int32_t, uint32_t, i32, 32) CHECK_INT(int64_t, uint64_t, i64, 64)
  for (int i = 0; i < NV; i++) {
    uint64_t a = (uint64_t)ivals[i];
    if ((uint16_t)fir_bswap_i16((int16_t)a) != (uint16_t)bsw((uint16_t)a, 16)) FAIL("bswap16 %llx\n", (unsigned long long)a);
    if ((uint32_t)fir_bswap_i32((int32_t)a) != (uint32_t)bsw((uint32_t)a, 32)) FAIL("bswap32 %llx\n", (unsigned long long)a);
    if ((uint64_t)fir_bswap_i64((int64_t)a) != bsw(a, 64)) FAIL("bswap64 %llx\n", (unsigned long long)a);
  }
  CHECK_FLOAT(float, uint32_t, f32, truncf, fmodf) CHECK_FLOAT(double, uint64_t, f64, trunc, fmod)
  /* memory: every small size and offset, overlapping both ways for memmove */
  unsigned char buf[128], ref[128];
  for (int n = 0; n <= 40; n++)
    for (int d = 0; d <= 24; d += 3)
      for (int s = 0; s <= 24; s += 5) {
        for (int k = 0; k < 128; k++) buf[k] = ref[k] = (unsigned char)(k * 7 + 1);
        fir_memmove(buf + d, buf + s, n); memmove(ref + d, ref + s, n);
        if (memcmp(buf, ref, 128)) FAIL("memmove n=%d d=%d s=%d\n", n, d, s);
        for (int k = 0; k < 128; k++) buf[k] = ref[k] = (unsigned char)k;
        fir_memcpy(buf + 64 + d, buf + s, n); memcpy(ref + 64 + d, ref + s, n);
        if (memcmp(buf, ref, 128)) FAIL("memcpy n=%d d=%d s=%d\n", n, d, s);
        fir_memset(buf + d, 0x100 + n, n); memset(ref + d, n, n);
        if (memcmp(buf, ref, 128)) FAIL("memset n=%d d=%d\n", n, d);
      }
  fir_memset7(buf + 1); memset(ref + 1, 0xab, 7);
  if (memcmp(buf, ref, 128)) FAIL("memset7\n");
  if (!bad) printf("ok\n");
  return bad;
}
