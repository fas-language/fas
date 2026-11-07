#define _GNU_SOURCE
/* Checks test/fas_fir/c/f16.fir against clang's _Float16; exit 0 when all
   agree. Every reference operation rounds to _Float16 on its own (a cast
   per step), so clang's excess precision for _Float16 cannot leak in. */
#include <math.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
typedef _Float16 H;
typedef long double LD;
H h_fadd(H, H), h_fsub(H, H), h_fmul(H, H), h_fdiv(H, H), h_frem(H, H);
H h_fmin(H, H), h_fmax(H, H), h_copysign(H, H);
H h_fneg(H), h_fabs(H), h_sqrt(H), h_floor(H), h_ceil(H), h_ftrunc(H), h_roundeven(H);
H h_fma(H, H, H);
int h_cmp(H, H);
int h_fptosi_i32(H); long h_fptosi_i64(H); unsigned h_fptoui_i32(H); unsigned long h_fptoui_i64(H);
signed char h_fptosi_sat_i8(H); long h_fptosi_sat_i64(H); unsigned short h_fptoui_sat_i16(H);
unsigned long h_fptoui_sat_i64(H); signed char h_fptosi_i8(H); void h_fptosi_sat_i33(long *, H);
H h_sitofp_i32(int), h_sitofp_i64(long), h_uitofp_i32(unsigned), h_uitofp_i64(unsigned long);
H h_sitofp_i8(signed char), h_uitofp_i16(unsigned short);
H h_sitofp_i128(__int128 *), h_uitofp_i128(unsigned __int128 *);
float h_to_f32(H); double h_to_f64(H); LD h_to_f80(H);
H h_from_f32(float), h_from_f64(double), h_from_f80(LD);
short h_bits(H); H h_of_bits(short);
H h_select(int, H, H), h_scale(H *, long, H);
H h_many(H, H, H, H, H, H, H, H, H, H, int, float, H);
H h_calls_c(H, H), h_vsum(int, ...);
H h_rmw_add(H *, H), h_rmw_sub(H *, H), h_xchg(H *, H); int h_cas(H *, H, H);

H c_hmix(H a, int i, H b, double d) { return (H)((H)(a * (H)i) - (H)(b * (H)d)); }
H c_hsum(int n, ...) {
  va_list ap; va_start(ap, n);
  H s = 0;
  for (int i = 0; i < n; i++) s = (H)(s + va_arg(ap, H));
  va_end(ap);
  return s;
}

static int bad = 0;
static unsigned short bits(H x) { unsigned short b; memcpy(&b, &x, 2); return b; }
static H of_bits(unsigned short b) { H x; memcpy(&x, &b, 2); return x; }
/* bitwise; any NaN equals any NaN */
static int same(H x, H y) { return (isnan((float)x) && isnan((float)y)) || bits(x) == bits(y); }
static void check(const char *what, H x, H y, H got, H want) {
  if (!same(got, want)) {
    printf("%s(%a, %a): got %a (%04x) want %a (%04x)\n", what, (double)x, (double)y,
           (double)got, bits(got), (double)want, bits(want));
    bad = 1;
  }
}
static void checki(const char *what, H x, long got, long want) {
  if (got != want) { printf("%s(%a): got %ld want %ld\n", what, (double)x, got, want); bad = 1; }
}
/* IEEE minimumNumber / maximumNumber */
static H minnum(H a, H b) {
  if (isnan((float)a)) return b; if (isnan((float)b)) return a;
  if (a == b) return signbit((float)a) ? a : b; return a < b ? a : b;
}
static H maxnum(H a, H b) {
  if (isnan((float)a)) return b; if (isnan((float)b)) return a;
  if (a == b) return signbit((float)a) ? b : a; return a > b ? a : b;
}
static int cmp(H a, H b) {
  int u = isunordered((float)a, (float)b), r = 0, k = 0;
  r |= (!u && a == b) << k++; r |= (!u && a > b) << k++; r |= (!u && a >= b) << k++;
  r |= (!u && a < b) << k++; r |= (!u && a <= b) << k++; r |= (!u && a != b) << k++;
  r |= (!u) << k++; r |= (u || a == b) << k++; r |= (u || a > b) << k++; r |= (u || a >= b) << k++;
  r |= (u || a < b) << k++; r |= (u || a <= b) << k++; r |= (u || a != b) << k++; r |= u << k++;
  return r | 1 << 14;
}
static long sat(H x, double lo, double hi) {
  float f = x; return isnan(f) ? 0 : f <= lo ? (long)lo : f >= hi ? (long)hi : (long)f;
}
static unsigned long satu64(H x) { float f = x; return isnan(f) || f <= 0 ? 0 : isinf(f) ? UINT64_MAX : (unsigned long)f; }

/* exact fused multiply-add: every finite f16 is k * 2^-24 with |k| < 2^40,
   so a*b + c = N * 2^-48 exactly in an __int128; then one RNE rounding */
static long k24(H x) { return (long)ldexp((double)x, 24); }
static H fma_ref(H a, H b, H c) {
  if (!isfinite((float)a) || !isfinite((float)b) || !isfinite((float)c))
    return (H)fmaf(a, b, c); /* infinities and NaNs: exact in f32 too */
  __int128 p = (__int128)k24(a) * k24(b), n = p + ((__int128)k24(c) << 24);
  if (n == 0) {
    int sp = signbit((float)a) != signbit((float)b), sc = signbit((float)c) != 0;
    if (p != 0) return 0; /* exact cancellation rounds to +0 */
    return sp && sc ? -(H)0 : (H)0;
  }
  int neg = n < 0; unsigned __int128 m = neg ? -n : n;
  int len = 0; while ((m >> len) != 0) len++;
  int sh = len - 11 > 24 ? len - 11 : 24; /* the quantum: 11 bits, or 2^-24 */
  unsigned __int128 q = m >> sh, rest = m - (q << sh), half = (unsigned __int128)1 << (sh - 1);
  if (rest > half || (rest == half && (q & 1))) q++;
  double v = ldexp((double)q, sh - 48);
  return (H)(neg ? -v : v); /* exact unless it overflows, which rounds to inf */
}

static H vals[40]; static int nv = 0;
static void add(H x) { vals[nv++] = x; }

int main(void) {
  float fv[] = { 0.0f, -0.0f, 1.0f, -1.0f, 0.5f, 2.5f, 3.5f, -2.5f, -3.5f, 0.1f, 1000.7f, 255.5f, 127.5f,
                 -128.5f, 65504.0f, -65504.0f, 0x1p-24f, -0x1p-24f, 0x1p-14f, 0x1.ffcp-15f, 3.0f, 7.0f, 1e-7f,
                 300.25f, -0.75f, 1.0f / 3, 40000.0f };
  for (unsigned i = 0; i < sizeof fv / sizeof fv[0]; i++) add((H)fv[i]);
  add((H)INFINITY); add((H)-INFINITY); add((H)NAN);
  for (int i = 0; i < nv; i++) {
    H x = vals[i];
    check("neg", x, 0, h_fneg(x), of_bits(bits(x) ^ 0x8000));
    check("abs", x, 0, h_fabs(x), of_bits(bits(x) & 0x7fff));
    check("sqrt", x, 0, h_sqrt(x), (H)sqrtf(x));
    check("floor", x, 0, h_floor(x), (H)floorf(x));
    check("ceil", x, 0, h_ceil(x), (H)ceilf(x));
    check("trunc", x, 0, h_ftrunc(x), (H)truncf(x));
    check("roundeven", x, 0, h_roundeven(x), (H)roundevenf(x));
    checki("bits", x, h_bits(x), (short)bits(x));
    check("of_bits", x, 0, h_of_bits((short)bits(x)), x);
    if (bits(h_of_bits((short)bits(x))) != bits(x)) { printf("of_bits %04x\n", bits(x)); bad = 1; }
    float f = x;
    if (isfinite(f)) {
      checki("fptosi_i32", x, h_fptosi_i32(x), (int)f);
      checki("fptosi_i64", x, h_fptosi_i64(x), (long)f);
      if (f > -1) {
        checki("fptoui_i32", x, h_fptoui_i32(x), (unsigned)f);
        checki("fptoui_i64", x, (long)h_fptoui_i64(x), (long)(unsigned long)f);
      }
      if (f > -129 && f < 128) checki("fptosi_i8", x, h_fptosi_i8(x), (signed char)f);
    }
    checki("sat_i8", x, h_fptosi_sat_i8(x), sat(x, -128, 127));
    checki("sat_i64", x, h_fptosi_sat_i64(x), isinf(f) ? (f < 0 ? INT64_MIN : INT64_MAX) : sat(x, -1e30, 1e30));
    checki("sat_u16", x, h_fptoui_sat_i16(x), sat(x, 0, 65535));
    checki("sat_u64", x, (long)h_fptoui_sat_i64(x), (long)satu64(x));
    long w = 0x5555555555555555; h_fptosi_sat_i33(&w, x);
    long w33 = (long)((unsigned long)w << 31) >> 31;
    checki("sat_i33", x, w33, isinf(f) ? (f < 0 ? -(1L << 32) : (1L << 32) - 1) : sat(x, -1e30, 1e30));
    if (h_to_f32(x) != f && !isnan(f)) { printf("to_f32(%a)\n", f); bad = 1; }
    if (h_to_f64(x) != (double)f && !isnan(f)) { printf("to_f64(%a)\n", f); bad = 1; }
    if (h_to_f80(x) != (LD)f && !isnan(f)) { printf("to_f80(%a)\n", f); bad = 1; }
    if (isnan(f) && !(isnan(h_to_f32(x)) && isnan(h_to_f64(x)) && isnan(h_to_f80(x)))) { printf("to nan\n"); bad = 1; }
    for (int j = 0; j < nv; j++) {
      H y = vals[j];
      check("add", x, y, h_fadd(x, y), (H)((float)x + y));
      check("sub", x, y, h_fsub(x, y), (H)((float)x - y));
      check("mul", x, y, h_fmul(x, y), (H)((float)x * y));
      check("div", x, y, h_fdiv(x, y), (H)((float)x / y));
      check("rem", x, y, h_frem(x, y), (H)fmodf(x, y));
      check("min", x, y, h_fmin(x, y), minnum(x, y));
      check("max", x, y, h_fmax(x, y), maxnum(x, y));
      check("copysign", x, y, h_copysign(x, y), of_bits((bits(x) & 0x7fff) | (bits(y) & 0x8000)));
      if (h_cmp(x, y) != cmp(x, y)) { printf("cmp(%a, %a): %x %x\n", (double)x, (double)y, h_cmp(x, y), cmp(x, y)); bad = 1; }
      check("select1", x, y, h_select(1, x, y), x);
      check("select0", x, y, h_select(0, x, y), y);
      for (int k = 0; k < nv; k += 3) check("fma", x, y, h_fma(x, y, vals[k]), fma_ref(x, y, vals[k]));
    }
  }
  /* fma cases where rounding the product first would differ */
  H one = 1, e = (H)0x1p-11f, ulp = (H)0x1p-10f;
  check("fma2", one + ulp, one - e, h_fma(one + ulp, (H)(one - e), (H)-1), fma_ref(one + ulp, (H)(one - e), (H)-1));
  check("fma3", (H)0x1p-12f, (H)0x1p-12f, h_fma((H)0x1p-12f, (H)0x1p-12f, (H)0x1p-14f), fma_ref((H)0x1p-12f, (H)0x1p-12f, (H)0x1p-14f));
  /* narrowing: exact halfway cases, and ones a second rounding would get wrong */
  float nf[] = { 1 + 0x1p-11f, 1 + 0x1p-11f + 0x1p-20f, 1 + 0x3p-11f, 0x1p-25f, 0x3p-25f, 0x1.8p-25f, 65519.99f,
                 65520.0f, 1e9f, -1e-9f, 0x1.ffcp-15f, 0x1.ffep-15f, 0x1.fffp-15f, INFINITY, -INFINITY, NAN };
  for (unsigned i = 0; i < sizeof nf / sizeof nf[0]; i++) check("from_f32", 0, 0, h_from_f32(nf[i]), (H)nf[i]);
  double nd[] = { 1 + 0x1p-11, 1 + 0x1p-11 + 0x1p-40, 1 + 0x1p-11 - 0x1p-40, 0x1p-25 + 0x1p-60, 0x1p-25,
                  65519.999999, 65520, -0x1.ffep-15 - 0x1p-50, 1e300, 1e-300, 0.1, INFINITY, NAN };
  for (unsigned i = 0; i < sizeof nd / sizeof nd[0]; i++) check("from_f64", 0, 0, h_from_f64(nd[i]), (H)nd[i]);
  LD nl[] = { 1 + 0x1p-11L, 1 + 0x1p-11L + 0x1p-60L, 1 + 0x1p-11L - 0x1p-60L, 0x1p-25L + 0x1p-62L, 0x1p-25L,
              65519.999999999L, 65520, 1e4000L, 1e-4000L, 0.1L, -INFINITY, NAN };
  for (unsigned i = 0; i < sizeof nl / sizeof nl[0]; i++) check("from_f80", 0, 0, h_from_f80(nl[i]), (H)(float)0 + (H)nl[i]);
  /* integers: through long double, which is exact for 64 bits, then one rounding */
  long iv[] = { 0, 1, -1, 2049, 2051, -2051, 4097, 65504, 65519, 65520, -65520, 100000, INT32_MAX, INT32_MIN,
                INT64_MAX, INT64_MIN, 127, -128, 255 };
  for (unsigned i = 0; i < sizeof iv / sizeof iv[0]; i++) {
    long v = iv[i];
    check("sitofp_i64", 0, 0, h_sitofp_i64(v), (H)(LD)v);
    check("uitofp_i64", 0, 0, h_uitofp_i64((unsigned long)v), (H)(LD)(unsigned long)v);
    check("sitofp_i32", 0, 0, h_sitofp_i32((int)v), (H)(LD)(int)v);
    check("uitofp_i32", 0, 0, h_uitofp_i32((unsigned)v), (H)(LD)(unsigned)v);
    check("sitofp_i8", 0, 0, h_sitofp_i8((signed char)v), (H)(LD)(signed char)v);
    check("uitofp_i16", 0, 0, h_uitofp_i16((unsigned short)v), (H)(LD)(unsigned short)v);
    __int128 s = v; unsigned __int128 u = (unsigned __int128)v;
    check("sitofp_i128", 0, 0, h_sitofp_i128(&s), (H)(LD)s);
    check("uitofp_i128", 0, 0, h_uitofp_i128(&u), (H)(LD)u);
  }
  /* calls, memory, the SSE argument registers and the stack */
  H a[6] = { 1, (H)2.5f, (H)-3.25f, (H)0.1f, 100, (H)1e-6f }, ref[6], refsum = 0, k = (H)1.5f;
  for (int i = 0; i < 6; i++) { ref[i] = (H)((H)(a[i] * k) + (H)0.5f); refsum = (H)(refsum + ref[i]); }
  check("scale", 0, 0, h_scale(a, 6, k), refsum);
  for (int i = 0; i < 6; i++) check("scale[]", 0, 0, a[i], ref[i]);
  H m[10]; for (int i = 0; i < 10; i++) m[i] = (H)(0.75f * i - 2);
  H t = (H)(m[0] + m[1]);
  for (int i = 2; i < 10; i++) t = i % 2 ? (H)(t * m[i]) : (H)(t + m[i]);
  H many = (H)((H)((H)(t + (H)7) - (H)3.3f) + (H)0.125f);
  check("many", 0, 0, h_many(m[0], m[1], m[2], m[3], m[4], m[5], m[6], m[7], m[8], m[9], 7, 3.3f, (H)0.125f), many);
  for (int i = 0; i < 4; i++) {
    H x = vals[i * 5 + 2], y = vals[i * 3 + 5];
    H mm = c_hmix(x, 3, y, 1.5), s = c_hsum(3, mm, x, y);
    check("calls_c", x, y, h_calls_c(x, y), (H)(s + x));
    check("vsum", x, y, h_vsum(4, x, y, (H)1, x), (H)((H)((H)(x + y) + (H)1) + x));
  }
  {
    H cell = (H)1.5f, w = (H)0.1f;
    check("rmw_add.old", 0, 0, h_rmw_add(&cell, w), (H)1.5f);
    check("rmw_add", 0, 0, cell, (H)((H)1.5f + w));
    H c1 = cell;
    check("rmw_sub.old", 0, 0, h_rmw_sub(&cell, (H)3), c1);
    check("rmw_sub", 0, 0, cell, (H)(c1 - (H)3));
    H c2 = cell;
    check("xchg.old", 0, 0, h_xchg(&cell, (H)-7), c2);
    check("xchg", 0, 0, cell, (H)-7);
    checki("cas.no", 0, h_cas(&cell, (H)7, (H)1), 0);
    check("cas.no.cell", 0, 0, cell, (H)-7);
    checki("cas.yes", 0, h_cas(&cell, (H)-7, (H)2.25f), 1);
    check("cas.yes.cell", 0, 0, cell, (H)2.25f);
  }
  if (!bad) printf("f16 ok\n");
  return bad;
}
