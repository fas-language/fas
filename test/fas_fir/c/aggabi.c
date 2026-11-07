/* Checks test/fas_fir/c/aggabi.fir against clang's struct-by-value ABI; exit 0 when all agree */
#include <stdio.h>
typedef struct { long a, b; } II;
typedef struct { double x, y; } DD;
typedef struct { long a; double d; } ID;
typedef struct { double d; long a; } DI;
typedef struct { float x, y; int z; } FFI;
typedef struct { long a, b, c; } BIG;
typedef struct { int a; } S1;
long fir_ii(II); double fir_dd(DD); double fir_id(ID); double fir_di(DI);
long fir_ffi(FFI); int fir_s1(S1); long fir_big(BIG);
long fir_exhaust(long, long, long, long, long, II, long);
II fir_mk_ii(long, long); DD fir_mk_dd(double, double); DI fir_mk_di(double, long);
FFI fir_mk_ffi(float, float, int); BIG fir_mk_big(long);
long fir_calls_c(void);
typedef struct { char a, b, c; } C3;
typedef struct { int a[5]; } I5;
typedef struct { int i; float f, g; } IFF;
C3 fir_c3(C3); I5 fir_i5(I5); IFF fir_iff(IFF); DI fir_apply(DI (*)(DI), DI);
static DI twice(DI x) { return (DI){x.d * 2, x.a * 2}; }

typedef struct { long double x; int i; } LDI;
typedef struct { long double x; } LD1;
LD1 c_ld_pair(LDI s, long double k) { return (LD1){s.x * k - s.i}; }
_Complex long double c_ld_cplx(long double a, long double b) { return a * b + (a - b) * 1.0iL; }
struct hhf { _Float16 x, y; float f; };
struct hh { _Float16 a, b; };
struct hh c_hpair(struct hhf p) { struct hh r = { (_Float16)(p.x + p.y), (_Float16)((float)p.x * p.f) }; return r; }
_Float16 fir_call_hpair(_Float16, _Float16);
struct hh fir_h_pair(struct hhf);
LD1 fir_ld_pair(LDI, long double);
_Complex long double fir_ld_cplx(long double, long double);
long double fir_call_ld(long double, long double);
long c_ii(II s) { return s.a * 3 + s.b; }
II c_mk_ii(long a, long b) { return (II){a, b}; }
BIG c_mk_big(long n) { return (BIG){n, n + 1, n + 2}; }
long c_big(BIG s) { return s.a + 2 * s.b + 3 * s.c; }
DD c_mk_dd(double x, double y) { return (DD){x, y}; }
long c_ffi(FFI s) { return (long)s.x * 100 + (long)s.y * 10 + s.z; }
long c_exhaust(long a, long b, long c, long d, long e, II s, long g) {
  long t = a;
  t = t * 2 + b; t = t * 2 + c; t = t * 2 + d; t = t * 2 + e;
  t = t * 2 + s.a; t = t * 2 + s.b; return t * 2 + g;
}
static long expect_calls_c(void) {
  II ii = c_mk_ii(5, 7); long r1 = c_ii(ii);
  BIG big = c_mk_big(10); long r2 = c_big(big);
  DD dd = c_mk_dd(1.5, -4.0); long r3 = (long)(dd.x * dd.y);
  long r4 = c_ffi((FFI){3.0f, 4.0f, 5});
  long r5 = c_exhaust(1, 2, 3, 4, 5, ii, 9);
  long s = r1; s = s * 1000003 + r2; s = s * 1000003 + r3; s = s * 1000003 + r4;
  return s * 1000003 + r5;
}
static int bad = 0;
#define CHECK(name, got, want) do { if ((got) != (want)) { printf("%s: got %ld want %ld\n", name, (long)(got), (long)(want)); bad = 1; } } while (0)
int main(void) {
  CHECK("ii", fir_ii((II){11, -4}), c_ii((II){11, -4}));
  CHECK("dd", fir_dd((DD){2.5, 0.25}) * 4, 9);
  CHECK("id", fir_id((ID){7, 1.25}) * 4, 38);
  CHECK("di", fir_di((DI){1.25, 7}) * 4, -18);
  CHECK("ffi", fir_ffi((FFI){6.0f, -2.0f, 9}), 589);
  CHECK("s1", fir_s1((S1){21}), -42);
  CHECK("big", fir_big((BIG){1, 2, 3}), 14);
  CHECK("exhaust", fir_exhaust(1, 2, 3, 4, 5, (II){6, 7}, 8), c_exhaust(1, 2, 3, 4, 5, (II){6, 7}, 8));
  II a = fir_mk_ii(-3, 1L << 40); CHECK("mk_ii.a", a.a, -3); CHECK("mk_ii.b", a.b, 1L << 40);
  DD d = fir_mk_dd(0.5, -8.0); CHECK("mk_dd", d.x * d.y, -4);
  DI di = fir_mk_di(-0.5, 99); CHECK("mk_di.d", di.d * 2, -1); CHECK("mk_di.a", di.a, 99);
  FFI f = fir_mk_ffi(1.5f, 2.5f, -77); CHECK("mk_ffi.xy", (f.x + f.y) * 2, 8); CHECK("mk_ffi.z", f.z, -77);
  BIG b = fir_mk_big(40); CHECK("mk_big", b.a * 10000 + b.b * 100 + b.c, 404142);
  CHECK("calls_c", fir_calls_c(), expect_calls_c());
  C3 c3 = fir_c3((C3){'x', 'y', 'z'}); CHECK("c3", c3.a * 65536 + c3.b * 256 + c3.c, 'z' * 65536 + 'z' * 256 + 'x');
  I5 i5 = fir_i5((I5){{1, 2, 3, 4, -5}});
  for (int k = 0; k < 5; k++) CHECK("i5", i5.a[k], ((int[]){-5, 4, 3, 2, 1})[k]);
  IFF iff = fir_iff((IFF){12, 0.5f, 8.0f}); CHECK("iff.i", iff.i, -12); CHECK("iff.fg", iff.f * 10 + iff.g * 4, 82);
  DI ap = fir_apply(twice, (DI){1.25, -21}); CHECK("apply.d", ap.d * 2, 5); CHECK("apply.a", ap.a, -42);
  {
    long double a = 1.0L / 3, b = -2.5L;
    LD1 p = fir_ld_pair((LDI){a, 5}, b); CHECK("ld_pair", p.x == a * b + 5, 1);
    _Complex long double c = fir_ld_cplx(a, b);
    CHECK("ld_cplx", __real__ c == a + b && __imag__ c == a - b, 1);
    long double re = a * b, im = a - b;
    CHECK("call_ld", fir_call_ld(a, b) == (a * b - 7) + re * im, 1);
  }
  {
    _Float16 x = (_Float16)1.5f, y = (_Float16)-0.375f;
    struct hh q = c_hpair((struct hhf){x, y, 2.0f});
    CHECK("call_hpair", fir_call_hpair(x, y) == (_Float16)(q.a - q.b), 1);
    struct hh r = fir_h_pair((struct hhf){x, y, 2.5f});
    CHECK("h_pair", r.a == y && r.b == (_Float16)(1.5f * 2.5f), 1);
  }
  if (!bad) printf("ok\n");
  return bad;
}
