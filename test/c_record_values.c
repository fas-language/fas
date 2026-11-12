#include <stdarg.h>
#include "c_record_values.h"

RvSmall rv_small(int32_t k, RvSmall v) {
  RvSmall out = {(int8_t)(v.a + k), (int16_t)(v.b * k), v.c - k};
  return out;
}
RvMixed rv_mixed(RvMixed v, double s) {
  RvMixed out = {v.i + 1, v.f * 2.0f, v.d * s};
  return out;
}
RvF2 rv_f2(RvF2 a, float s, RvF2 b) {
  RvF2 out = {a.x * s + b.x, a.y - b.y * s};
  return out;
}
RvD2 rv_d2(RvD2 a, RvD2 b, RvD2 c, RvD2 d, RvD2 e) {
  RvD2 out = {a.x + 2 * b.x + 4 * c.x + 8 * d.x + 16 * e.x,
              a.y - b.y + c.y - d.y + e.y};
  return out;
}
RvBig rv_big(RvBig v, int64_t k) {
  RvBig out = {v.c * k, v.a + k, v.b - k, v.d / 2};
  return out;
}
RvUnion rv_union(RvUnion v) {
  RvUnion out;
  out.i = v.i ^ 0x5a5a;
  return out;
}
RvPacked rv_packed(RvPacked v) {
  RvPacked out = {(char)(v.c + 1), v.i * 3};
  return out;
}
RvAligned rv_aligned(int32_t k, RvAligned v) {
  RvAligned out = {v.x * k - 1};
  return out;
}
int64_t rv_sum(RvSmall a, RvMixed b, RvF2 c, RvD2 d, RvBig e, RvPacked f) {
  return a.a + a.b + a.c + b.i + (int64_t)b.f + (int64_t)b.d + (int64_t)c.x +
         (int64_t)c.y + (int64_t)d.x + (int64_t)d.y + e.a + e.b + e.c +
         (int64_t)e.d + f.c + f.i;
}
RvF2 rv_make(void) {
  RvF2 out = {0.25f, -8.5f};
  return out;
}
RvNested rv_nested(RvNested v) {
  RvNested out = {{v.inner.y * 2.0f, v.inner.x - 1.0f}, v.n * 7};
  return out;
}
RvArr rv_arr(RvArr v) {
  RvArr out = {{(int16_t)(v.s[2] * 10), (int16_t)(v.s[0] - 4), (int16_t)(v.s[1] + 300)}};
  return out;
}
/* reads RvMixed, RvD2, RvBig, RvSmall, RvPacked in that order */
int64_t rv_va(int32_t n, ...) {
  va_list ap;
  va_start(ap, n);
  RvMixed m = va_arg(ap, RvMixed);
  RvD2 d = va_arg(ap, RvD2);
  RvBig b = va_arg(ap, RvBig);
  RvSmall s = va_arg(ap, RvSmall);
  RvPacked p = va_arg(ap, RvPacked);
  va_end(ap);
  return n * 1000 + m.i + (int64_t)(m.d * 8) + (int64_t)(d.x * 4) + (int64_t)d.y +
         b.a + b.b + b.c + (int64_t)b.d + s.a + s.b + s.c + p.c + p.i;
}
double rv_apply(void *fn) {
  RvD2 (*f)(RvD2, RvD2, RvD2, RvD2, RvD2) = (RvD2 (*)(RvD2, RvD2, RvD2, RvD2, RvD2))fn;
  RvD2 one = {1.0, 1.0};
  RvD2 r = f(one, one, one, one, one);
  return r.x * 100 + r.y;
}
