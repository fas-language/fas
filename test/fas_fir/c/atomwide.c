/* Checks test/fas_fir/c/atomwide.fir: every op once against C (old value
   and new memory), then threads of Fir code and C code updating the same
   objects at once (C through libatomic, as clang compiles such atomics);
   exit 0 when all agree */
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
typedef unsigned __int128 u128;
typedef unsigned _BitInt(256) u256;
typedef unsigned _BitInt(33) u33;
typedef unsigned _BitInt(7) u7;
typedef long double LD;

#define OPS(X) X(add) X(sub) X(max) X(min) X(umax) X(umin) X(xchg) X(or) X(and) X(xor)
#define DECL33(op) u33 a33_##op(void *, u33);
#define DECL100(op) u128 a100_##op(void *, u128); u128 a128_##op(void *, u128); u256 a256_##op(void *, u256);
OPS(DECL33) OPS(DECL100)
u7 a7_add(void *, u7), a7_sub(void *, u7), a7_max(void *, u7), a7_min(void *, u7), a7_umax(void *, u7), a7_umin(void *, u7);
_Bool a1_xchg(void *, _Bool), a1_load(void *); void a1_store(void *, _Bool);
u33 a33_load(void *); void a33_store(void *, u33); u33 a33_cas(void *, u33, u33, char *);
u7 a7_cas(void *, u7, u7, char *);
u128 a100_load(void *), a128_load(void *); void a100_store(void *, u128), a128_store(void *, u128);
u128 a100_cas(void *, u128, u128, char *), a128_cas(void *, u128, u128, char *);
u256 a256_load(void *); void a256_store(void *, u256); u256 a256_cas(void *, u256, u256, char *);
LD f80_fadd(void *, LD), f80_fsub(void *, LD), f80_xchg(void *, LD), f80_load(void *), f80_cas(void *, LD, LD, char *);
void f80_store(void *, LD);
void a33_add_loop(void *, u33, long), a7_max_loop(void *, u7, long), a128_add_loop(void *, u128, long);
void a256_add_loop(void *, u256, long), f80_fadd_loop(void *, LD, long), a100_sub_loop(void *, u128, long);

static int bad;
#define FAIL(...) do { printf(__VA_ARGS__); printf("\n"); bad = 1; } while (0)

/* the reference: op on w-bit values a, b (as u256, zero above w) */
static u256 mask(int w, u256 x) { return w == 256 ? x : x & (((u256)1 << w) - 1); }
static _BitInt(256) sx(int w, u256 x) { return (_BitInt(256))(x << (256 - w)) >> (256 - w); }
static u256 ref(const char *op, int w, u256 a, u256 b) {
  if (!strcmp(op, "add")) return mask(w, a + b);
  if (!strcmp(op, "sub")) return mask(w, a - b);
  if (!strcmp(op, "max")) return sx(w, a) > sx(w, b) ? a : b;
  if (!strcmp(op, "min")) return sx(w, a) < sx(w, b) ? a : b;
  if (!strcmp(op, "umax")) return a > b ? a : b;
  if (!strcmp(op, "umin")) return a < b ? a : b;
  if (!strcmp(op, "xchg")) return b;
  if (!strcmp(op, "or")) return a | b;
  if (!strcmp(op, "and")) return a & b;
  return a ^ b;
}
static unsigned long rs = 88172645463325252UL;
static unsigned long rnd(void) { rs ^= rs << 13; rs ^= rs >> 7; rs ^= rs << 17; return rs; }
/* edge values and random ones, at width w */
static u256 val(int w, int i) {
  u256 one = 1, top = one << (w - 1);
  switch (i) {
  case 0: return 0;
  case 1: return 1;
  case 2: return mask(w, ~(u256)0);
  case 3: return top;
  case 4: return top - 1;
  case 5: return top + 1;
  case 6: return w > 64 ? (u256)1 << 64 : 5;
  default: {
    u256 x = 0;
    for (int k = 0; k < 4; k++) x = x << 64 | rnd();
    if (i & 1) x >>= rnd() % w;
    return mask(w, x);
  }
  }
}
#define NV 24
#define str(x) #x

/* one width: memory in m (bytes), call a_op through fn(p, v) */
#define CHECK_OPS(W, T, PFX, M)                                                              \
  for (int i = 0; i < NV; i++)                                                               \
    for (int j = 0; j < NV; j++) {                                                           \
      u256 a = val(W, i), b = val(W, j);                                                     \
      _Pragma("GCC diagnostic ignored \"-Wunused-value\"")                                   \
      OPS(PFX)                                                                               \
    }
#define ONE(W, T, M, op, f)                                                                   \
  do {                                                                                        \
    M = (T)a; T old = f(&M, (T)b);                                                            \
    if ((u256)old != a || (u256)(T)M != mask(W, ref(#op, W, a, b)))                           \
      FAIL("i%d %s: old %d new %d", W, #op, (u256)old == a, (u256)(T)M == ref(#op, W, a, b)); \
  } while (0);

static void cas_check(int w, u256 a, u256 b, void *m, u256 (*load)(void *), void (*set)(void *, u256),
                      u256 (*cas)(void *, u256, u256, char *)) {
  char ok = 9;
  set(m, a);
  u256 old = cas(m, a, b, &ok);
  if (old != a || ok != 1 || load(m) != b) FAIL("i%d cas hit", w);
  u256 other = mask(w, a ^ 1);
  set(m, a);
  old = cas(m, other, b, &ok);
  if (old != a || ok != 0 || load(m) != a) FAIL("i%d cas miss", w);
}
/* the same through u256, per width */
static uint64_t m33; static uint8_t m7; static u128 m100 __attribute__((aligned(16))); static u256 m256;
static u256 l33(void *p) { return *(uint64_t *)p; } static void s33(void *p, u256 v) { *(uint64_t *)p = (uint64_t)v; }
static u256 c33(void *p, u256 a, u256 b, char *ok) { return a33_cas(p, (u33)a, (u33)b, ok); }
static u256 l7(void *p) { return *(uint8_t *)p; } static void s7(void *p, u256 v) { *(uint8_t *)p = (uint8_t)v; }
static u256 c7(void *p, u256 a, u256 b, char *ok) {
  /* weak: may fail spuriously; retry a hit that failed with the value unchanged */
  for (;;) {
    u7 r = a7_cas(p, (u7)a, (u7)b, ok);
    if (*ok || r != (u7)a) return r;
  }
}
static u256 l128(void *p) { return *(u128 *)p; } static void s128(void *p, u256 v) { *(u128 *)p = (u128)v; }
static u256 c100(void *p, u256 a, u256 b, char *ok) { return a100_cas(p, (u128)a, (u128)b, ok); }
static u256 c128(void *p, u256 a, u256 b, char *ok) { return a128_cas(p, (u128)a, (u128)b, ok); }
static u256 l256(void *p) { return *(u256 *)p; } static void s256(void *p, u256 v) { *(u256 *)p = v; }
static u256 c256(void *p, u256 a, u256 b, char *ok) { return a256_cas(p, a, b, ok); }

/* threads */
#define N 20000
static u128 t128 __attribute__((aligned(16)));
static u128 t100 __attribute__((aligned(16)));
static struct { uint64_t l[4]; } t256;
static uint64_t t33;
static _Atomic LD tf80;
static void *fir_side(void *arg) {
  (void)arg;
  a128_add_loop(&t128, 3, N);
  a256_add_loop(&t256, (u256)3 << 100, N);
  f80_fadd_loop((void *)&tf80, 1.0L, N);
  a33_add_loop(&t33, 3, N);
  a100_sub_loop(&t100, 3, N);
  return 0;
}
static void *c_side(void *arg) {
  (void)arg;
  for (int i = 0; i < N; i++) __atomic_fetch_add(&t128, 5, __ATOMIC_SEQ_CST);
  for (int i = 0; i < N; i++) {
    typeof(t256) old, nw;
    __atomic_load(&t256, &old, __ATOMIC_RELAXED);
    do {
      u256 v; memcpy(&v, &old, 32); v += (u256)5 << 100; memcpy(&nw, &v, 32);
    } while (!__atomic_compare_exchange(&t256, &old, &nw, 0, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST));
  }
  for (int i = 0; i < N; i++) tf80 += 2.0L;
  for (int i = 0; i < N; i++) __atomic_fetch_add(&t33, 5, __ATOMIC_SEQ_CST);
  for (int i = 0; i < N; i++) __atomic_fetch_sub(&t100, 5, __ATOMIC_SEQ_CST);
  return 0;
}

int main(void) {
#define P33(op) ONE(33, u33, m33, op, a33_##op)
#define P100(op) ONE(100, u128, m100, op, a100_##op)
#define P128(op) ONE(128, u128, m100, op, a128_##op)
#define P256(op) ONE(256, u256, m256, op, a256_##op)
  CHECK_OPS(33, u33, P33, m33)
  CHECK_OPS(100, u128, P100, m100)
  CHECK_OPS(128, u128, P128, m100)
  CHECK_OPS(256, u256, P256, m256)
  for (int i = 0; i < NV; i++)
    for (int j = 0; j < NV; j++) {
      u256 a = val(7, i), b = val(7, j);
      ONE(7, u7, m7, add, a7_add) ONE(7, u7, m7, sub, a7_sub) ONE(7, u7, m7, max, a7_max)
      ONE(7, u7, m7, min, a7_min) ONE(7, u7, m7, umax, a7_umax) ONE(7, u7, m7, umin, a7_umin)
      cas_check(7, a, b, &m7, l7, s7, c7);
      cas_check(33, val(33, i), val(33, j), &m33, l33, s33, c33);
      cas_check(100, val(100, i), val(100, j), &m100, l128, s128, c100);
      cas_check(128, val(128, i), val(128, j), &m100, l128, s128, c128);
      cas_check(256, val(256, i), val(256, j), &m256, l256, s256, c256);
    }
  /* the bits above N stay zero; loads ignore them */
  m33 = 0; a33_store(&m33, (u33)0x1ffffffffUL);
  if (m33 != 0x1ffffffffUL || a33_load(&m33) != (u33)0x1ffffffffUL) FAIL("i33 store/load");
  m33 = 0xfffffffe00000005UL; if (a33_load(&m33) != 5) FAIL("i33 load ignores the bits above");
  u128 big = ((u128)0x0123456789abcdefUL << 64 | 0xfedcba9876543210UL) & (((u128)1 << 100) - 1);
  m100 = 0; a100_store(&m100, big); if (m100 != big || a100_load(&m100) != big) FAIL("i100 store/load");
  m100 = ~(u128)0; a128_store(&m100, 7); if (a128_load(&m100) != 7) FAIL("i128 store/load");
  u256 h = ((u256)0xdeadbeefUL << 200) | 12345; m256 = 0; a256_store(&m256, h);
  if (m256 != h || a256_load(&m256) != h) FAIL("i256 store/load");
  unsigned char bb = 0; a1_store(&bb, 1); if (bb != 1 || a1_load(&bb) != 1 || a1_xchg(&bb, 0) != 1 || bb != 0) FAIL("i1");
  /* f80 */
  {
    LD m __attribute__((aligned(16))) = 0, x = 1.0L / 3, y = -2.5e300L;
    f80_store(&m, x); if (memcmp(&m, &x, 10) || f80_load(&m) != x) FAIL("f80 store/load");
    if (f80_fadd(&m, y) != x || m != x + y) FAIL("f80 fadd");
    LD s = m;
    if (f80_fsub(&m, x) != s || m != s - x) FAIL("f80 fsub");
    s = m;
    if (f80_xchg(&m, x) != s || m != x) FAIL("f80 xchg");
    char ok = 9;
    if (f80_cas(&m, x, y, &ok) != x || ok != 1 || m != y) FAIL("f80 cas hit");
    if (f80_cas(&m, x, 0.0L, &ok) != y || ok != 0 || m != y) FAIL("f80 cas miss");
  }
  /* threads: two of Fir, two of C, on each object */
  t128 = ~(u128)0 >> 64; t128 -= 10;
  u256 s256 = ~(u256)0 >> 100; memcpy(&t256, &s256, 32);
  tf80 = 0.5L; t33 = (1UL << 32) - 100; t100 = (u128)1 << 99;
  pthread_t th[4];
  for (int i = 0; i < 4; i++) pthread_create(&th[i], 0, i & 1 ? c_side : fir_side, 0);
  for (int i = 0; i < 4; i++) pthread_join(th[i], 0);
  u128 w128 = (~(u128)0 >> 64) - 10 + (u128)2 * N * 8;
  if (t128 != w128) FAIL("threads i128");
  u256 g256; memcpy(&g256, &t256, 32);
  if (g256 != s256 + ((u256)2 * N * 8 << 100)) FAIL("threads i256");
  if (tf80 != 0.5L + 2.0L * N * 3) FAIL("threads f80: %Lf", (LD)tf80);
  if (t33 != (1UL << 32) - 100 + 2UL * N * 8) FAIL("threads i33");
  if (t100 != ((u128)1 << 99) - (u128)2 * N * 8) FAIL("threads i100");
  return bad;
}
