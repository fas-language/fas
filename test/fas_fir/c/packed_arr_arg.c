/* abi_fuzz campaign-9 find 160591, reduced: a packed struct array whose
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
struct S14 { int8_t f0; uint16_t f1; } __attribute__((packed));
struct S13 { uint8_t f0; struct S14 f1[3]; };
void c_callee_0(struct S13 a0) {
  uint64_t h = 0xcbf29ce484222325ULL;
  h = (h ^ (uint64_t)(a0.f0)) * 0x100000001b3ULL;
  h = (h ^ (uint64_t)((int64_t)(a0.f1[0].f0))) * 0x100000001b3ULL;
  h = (h ^ (uint64_t)(a0.f1[0].f1)) * 0x100000001b3ULL;
  h = (h ^ (uint64_t)((int64_t)(a0.f1[1].f0))) * 0x100000001b3ULL;
  h = (h ^ (uint64_t)(a0.f1[1].f1)) * 0x100000001b3ULL;
  h = (h ^ (uint64_t)((int64_t)(a0.f1[2].f0))) * 0x100000001b3ULL;
  h = (h ^ (uint64_t)(a0.f1[2].f1)) * 0x100000001b3ULL;
  abi_sink = h;
}
void fir_callee_0(struct S13 a0);
uint64_t ref_0(void) {
  abi_sink = 0;
  c_callee_0((struct S13){(uint8_t)(0x8000000000000000ULL), {(struct S14){(int8_t)(0x890faaff418f57a7ULL), (uint16_t)(0x25f597e8f86504c4ULL)}, (struct S14){(int8_t)(0x24ce57f651c28e0dULL), (uint16_t)(0xffffffffffffffffULL)}, (struct S14){(int8_t)(0x80ULL), (uint16_t)(0x8000ULL)}}});
  return abi_sink;
}
uint64_t c2f_0(void) {
  abi_sink = 0;
  fir_callee_0((struct S13){(uint8_t)(0x8000000000000000ULL), {(struct S14){(int8_t)(0x890faaff418f57a7ULL), (uint16_t)(0x25f597e8f86504c4ULL)}, (struct S14){(int8_t)(0x24ce57f651c28e0dULL), (uint16_t)(0xffffffffffffffffULL)}, (struct S14){(int8_t)(0x80ULL), (uint16_t)(0x8000ULL)}}});
  return abi_sink;
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
