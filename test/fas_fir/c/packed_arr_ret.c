/* abi_fuzz campaign-9 find 694009, reduced: a packed struct array whose
   later elements misalign a 16-bit member makes the whole agg MEMORY, as
   clang passes it (gcc checks the first element only). Exit 0 when C and
   Fir agree. */
#include <stdint.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
static uint64_t bits16(_Float16 x) { uint16_t u; memcpy(&u, &x, 2); return u; }
static uint64_t bits32(float x) { uint32_t u; memcpy(&u, &x, 4); return u; }
static uint64_t bits64(double x) { uint64_t u; memcpy(&u, &x, 8); return u; }
static uint64_t f80lo(long double x) { uint64_t u; memcpy(&u, &x, 8); return u; }
static uint64_t f80hi(long double x) { uint16_t u; memcpy(&u, (char *)&x + 8, 2); return u; }
/* a void callee's hash */
uint64_t abi_sink;
struct S11 { uint16_t f0; int32_t f1; } __attribute__((packed));
struct S10 { _Float16 f0; struct S11 f1[2]; } __attribute__((packed));
struct S10 c_callee_0(void) {
  uint64_t h = 0xcbf29ce484222325ULL;
  struct S10 r;
  memset(&r, 0, sizeof r);
  r.f0 = (_Float16)(int8_t)(h * 3ULL + 0x0ULL);
  r.f1[0].f0 = (uint16_t)(h * 5ULL + 0x9e3779b97f4a7c15ULL);
  r.f1[0].f1 = (int32_t)(h * 7ULL + 0x3c6ef372fe94f82aULL);
  r.f1[1].f0 = (uint16_t)(h * 9ULL + 0xdaa66d2c7ddf743fULL);
  r.f1[1].f1 = (int32_t)(h * 11ULL + 0x78dde6e5fd29f054ULL);
  return r;
}
struct S10 fir_callee_0(void);
uint64_t ref_0(void) {
  struct S10 r = c_callee_0();
  uint64_t h = 0xcbf29ce484222325ULL;
  h = (h ^ (uint64_t)(bits16(r.f0))) * 0x100000001b3ULL;
  h = (h ^ (uint64_t)(r.f1[0].f0)) * 0x100000001b3ULL;
  h = (h ^ (uint64_t)((int64_t)(r.f1[0].f1))) * 0x100000001b3ULL;
  h = (h ^ (uint64_t)(r.f1[1].f0)) * 0x100000001b3ULL;
  h = (h ^ (uint64_t)((int64_t)(r.f1[1].f1))) * 0x100000001b3ULL;
  return h;
}
uint64_t c2f_0(void) {
  struct S10 r = fir_callee_0();
  uint64_t h = 0xcbf29ce484222325ULL;
  h = (h ^ (uint64_t)(bits16(r.f0))) * 0x100000001b3ULL;
  h = (h ^ (uint64_t)(r.f1[0].f0)) * 0x100000001b3ULL;
  h = (h ^ (uint64_t)((int64_t)(r.f1[0].f1))) * 0x100000001b3ULL;
  h = (h ^ (uint64_t)(r.f1[1].f0)) * 0x100000001b3ULL;
  h = (h ^ (uint64_t)((int64_t)(r.f1[1].f1))) * 0x100000001b3ULL;
  return h;
}
uint64_t f2c_0(void);
uint64_t f2f_0(void);
int main(void) {
#ifndef __clang__
  /* gcc passes this struct in registers, clang and Fir in memory (SPEC 5) */
  printf("ok (not clang, skipped)\n");
  return 0;
#endif
  int bad = 0;
  { uint64_t r = ref_0(), a = c2f_0(), c = f2c_0(), f = f2f_0();
    if (a != r) { printf("case 0: C to Fir %016llx, C to C %016llx\n", (unsigned long long)a, (unsigned long long)r); bad = 1; }
    if (c != r) { printf("case 0: Fir to C %016llx, C to C %016llx\n", (unsigned long long)c, (unsigned long long)r); bad = 1; }
    if (f != r) { printf("case 0: Fir to Fir %016llx, C to C %016llx\n", (unsigned long long)f, (unsigned long long)r); bad = 1; } }
  if (!bad) printf("ok\n");
  return bad;
}
