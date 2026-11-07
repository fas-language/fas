/* Checks test/fas_fir/c/vecabi.fir against clang's SysV ABI for vectors
   and structs with vector, __int128 and over-aligned members; exit 0 when
   all agree */
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
typedef float v4f __attribute__((vector_size(16)));
typedef float v2f __attribute__((vector_size(8)));
typedef signed char v4c __attribute__((vector_size(4)));
typedef double v2d __attribute__((vector_size(16)));
typedef long v1l __attribute__((vector_size(8)));
typedef float v3f __attribute__((ext_vector_type(3)));
typedef float v8f __attribute__((vector_size(32)));
typedef int v4i __attribute__((vector_size(16)));
struct si { __int128 a; };
struct sv { v4f a; };
struct s2v { v2f a; float b; };
struct sa { double d; } __attribute__((aligned(16)));
struct ev { struct {} e; float f; };

v4f fir_v4f(v4f, v4f), fir_call_v4f(v4f, v4f);
v2f fir_v2f(v2f, float), fir_call_v2f(v2f, float);
v4c fir_v4c(v4c, v4c), fir_call_v4c(v4c, v4c);
double fir_v2d(v2d), fir_call_v2d(v2d);
v1l fir_v1l(v1l), fir_call_v1l(v1l);
v3f fir_v3f(v3f), fir_call_v3f(v3f);
float fir_v8f(long, long, long, long, long, long, long, v8f), fir_call_v8f(long, v8f);
int fir_v9(v4i, v4i, v4i, v4i, v4i, v4i, v4i, v4i, v4i, int), fir_call_v9(v4i, v4i, int);
struct si fir_si(long, struct si, struct si, struct si), fir_call_si(long, struct si, struct si, struct si);
struct sv fir_sv(struct sv), fir_call_sv(struct sv);
struct s2v fir_s2v(struct s2v), fir_call_s2v(struct s2v);
struct sa fir_sa(struct sa), fir_call_sa(struct sa);
float fir_ev(struct ev, float), fir_call_ev(struct ev, float);
float fir_vsum(int, ...), fir_call_vsum(v4f, v4f, struct sv, double);

/* the C side: the same functions as the Fir ones (fir_call_X forwards here) */
v4f c_v4f(v4f a, v4f b) { return a * b + a; }
v2f c_v2f(v2f a, float s) { return a * s; }
v4c c_v4c(v4c a, v4c b) { return a + b; }
double c_v2d(v2d a) { return a[0] - a[1]; }
v1l c_v1l(v1l a) { return a * 3; }
v3f c_v3f(v3f a) { return a + a; }
float c_v8f(long x1, long x2, long x3, long x4, long x5, long x6, long x7, v8f v) {
  return v[0] + v[0] + v[7] + (float)x7;
}
int c_v9(v4i a, v4i b, v4i c, v4i d, v4i e, v4i f, v4i g, v4i h, v4i i, int k) { return a[2] + i[1] * 3 + h[0] + k; }
struct si c_si(long x, struct si a, struct si b, struct si c) { struct si r = {x + a.a + 2 * b.a + 3 * c.a}; return r; }
struct sv c_sv(struct sv p) { struct sv r = {{p.a[3], p.a[2], p.a[1], p.a[0]}}; return r; }
struct s2v c_s2v(struct s2v p) { struct s2v r = {p.a * p.b, p.b + 1}; return r; }
struct sa c_sa(struct sa p) { struct sa r = {p.d * 3}; return r; }
float c_ev(struct ev p, float g) { return p.f - g; }
float c_vsum(int n, ...) {
  va_list ap; va_start(ap, n);
  float s = 0;
  for (int i = 0; i < n; i++) { v4f v = va_arg(ap, v4f); s += v[i & 3]; }
  struct sv p = va_arg(ap, struct sv);
  double d = va_arg(ap, double);
  va_end(ap);
  return s * 10 + p.a[3] + (float)d;
}

static int bad = 0;
#define SAME(what, x, y) do { __typeof__(x) x_ = (x), y_ = (y); \
  if (memcmp(&x_, &y_, sizeof x_)) { printf("%s differs\n", what); bad = 1; } } while (0)

int main(void) {
  v4f a = {1.5f, -2, 3.25f, 8}, b = {0.5f, 4, -1, 2};
  SAME("v4f", fir_v4f(a, b), c_v4f(a, b));
  SAME("call_v4f", fir_call_v4f(a, b), c_v4f(a, b));
  v2f p = {3, -0.5f};
  SAME("v2f", fir_v2f(p, 1.5f), c_v2f(p, 1.5f));
  SAME("call_v2f", fir_call_v2f(p, 1.5f), c_v2f(p, 1.5f));
  v4c c = {1, -2, 100, 7}, d = {5, 6, 100, -9};
  SAME("v4c", fir_v4c(c, d), c_v4c(c, d));
  SAME("call_v4c", fir_call_v4c(c, d), c_v4c(c, d));
  v2d e = {10.5, 0.25};
  SAME("v2d", fir_v2d(e), c_v2d(e));
  SAME("call_v2d", fir_call_v2d(e), c_v2d(e));
  v1l l = {-123456789012};
  SAME("v1l", fir_v1l(l), c_v1l(l));
  SAME("call_v1l", fir_call_v1l(l), c_v1l(l));
  v3f t = {1.25f, -3, 7};
  v3f t1 = fir_v3f(t), t2 = fir_call_v3f(t), tw = c_v3f(t);
  for (int i = 0; i < 3; i++) if (t1[i] != tw[i] || t2[i] != tw[i]) { printf("v3f differs\n"); bad = 1; }
  v8f w = {1, 2, 3, 4, 5, 6, 7, 8.5f};
  SAME("v8f", fir_v8f(1, 2, 3, 4, 5, 6, 40, w), c_v8f(1, 2, 3, 4, 5, 6, 40, w));
  SAME("call_v8f", fir_call_v8f(40, w), c_v8f(1, 2, 3, 4, 5, 6, 40, w));
  v4i va = {1, 2, 3, 4}, vb = {10, 20, 30, 40};
  SAME("v9", fir_v9(va, vb, vb, vb, vb, vb, vb, va, vb, 1000), c_v9(va, vb, vb, vb, vb, vb, vb, va, vb, 1000));
  SAME("call_v9", fir_call_v9(va, vb, 1000), c_v9(va, vb, vb, vb, vb, vb, vb, va, vb, 1000));
  struct si i1 = {(__int128)0x123456789abcdefLL << 50}, i2 = {-((__int128)7 << 70)}, i3 = {((__int128)1 << 100) + 5};
  SAME("si", fir_si(-9, i1, i2, i3), c_si(-9, i1, i2, i3));
  SAME("call_si", fir_call_si(-9, i1, i2, i3), c_si(-9, i1, i2, i3));
  struct sv s = {{1, 2, 3, 4.5f}};
  SAME("sv", fir_sv(s), c_sv(s));
  SAME("call_sv", fir_call_sv(s), c_sv(s));
  struct s2v q = {{1.5f, -4}, 2.5f};
  /* fields only: the 4 bytes of padding are unspecified */
  SAME("s2v.a", fir_s2v(q).a, c_s2v(q).a);
  SAME("s2v.b", fir_s2v(q).b, c_s2v(q).b);
  SAME("call_s2v.a", fir_call_s2v(q).a, c_s2v(q).a);
  SAME("call_s2v.b", fir_call_s2v(q).b, c_s2v(q).b);
  struct sa r = {7.25};
  SAME("sa", fir_sa(r).d, c_sa(r).d);
  SAME("call_sa", fir_call_sa(r).d, c_sa(r).d);
  struct ev z = {{}, 9.5f};
  SAME("ev", fir_ev(z, 0.25f), c_ev(z, 0.25f));
  SAME("call_ev", fir_call_ev(z, 0.25f), c_ev(z, 0.25f));
  float want = c_vsum(3, a, b, a, s, 0.5);
  SAME("vsum", fir_vsum(3, a, b, a, s, 0.5), want);
  SAME("call_vsum", fir_call_vsum(a, b, s, 0.5), want);
  /* past the xmm registers: six vectors, then the struct and the double on the stack */
  SAME("vsum6", fir_vsum(6, a, b, a, b, a, b, s, 0.5), c_vsum(6, a, b, a, b, a, b, s, 0.5));
  SAME("vsum9", fir_vsum(9, a, b, a, b, a, b, a, b, a, s, 0.5), c_vsum(9, a, b, a, b, a, b, a, b, a, s, 0.5));
  if (!bad) printf("vecabi ok\n");
  return bad;
}
