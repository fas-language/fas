#include <stdint.h>

typedef struct { int8_t a; int16_t b; int32_t c; } RvSmall;
typedef struct { int32_t i; float f; double d; } RvMixed;
typedef struct { float x, y; } RvF2;
typedef struct { double x, y; } RvD2;
typedef struct { int64_t a, b, c; double d; } RvBig;
typedef union { int64_t i; double d; } RvUnion;
typedef struct __attribute__((packed)) { char c; int32_t i; } RvPacked;
typedef struct __attribute__((aligned(16))) { int32_t x; } RvAligned;

RvSmall rv_small(int32_t k, RvSmall v);
RvMixed rv_mixed(RvMixed v, double s);
RvF2 rv_f2(RvF2 a, float s, RvF2 b);
RvD2 rv_d2(RvD2 a, RvD2 b, RvD2 c, RvD2 d, RvD2 e);
RvBig rv_big(RvBig v, int64_t k);
RvUnion rv_union(RvUnion v);
RvPacked rv_packed(RvPacked v);
RvAligned rv_aligned(int32_t k, RvAligned v);
int64_t rv_sum(RvSmall a, RvMixed b, RvF2 c, RvD2 d, RvBig e, RvPacked f);
RvF2 rv_make(void);

/* nested records, arrays in records, variadic record arguments and the
   address of a by-value function (removal B1 lifted these) */
typedef struct { RvF2 inner; int32_t n; } RvNested;
typedef struct { int16_t s[3]; } RvArr;
RvNested rv_nested(RvNested v);
RvArr rv_arr(RvArr v);
int64_t rv_va(int32_t n, ...);
double rv_apply(void *fn);

static inline RvD2 rv_static(RvD2 v, RvF2 w) {
  RvD2 out = {v.y + w.x, v.x - w.y};
  return out;
}
