/* Checks test/fas_fir/c/atomvec.fir: each vector atomic once (old value
   and new memory), then three threads updating the same vectors at once;
   exit 0 when all agree */
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
void v4i8_add_loop(void *, long), v4i32_add_loop(void *, long), v3f32_fadd_loop(void *, long);
void v12b_xor_loop(void *, long);
void v3i8_umax(void *, void *, void *), v2i16_min(void *, void *, void *), v3f32_xchg(void *, void *, void *);
void v4i32_load(void *, void *), v4i32_store(void *, void *), v12b_load(void *, void *), v12b_store(void *, void *);
char v2f32_cas(void *, void *, void *, void *), v4i32_cas(void *, void *, void *, void *);

static int bad;
#define CHECK(c) do { if (!(c)) { printf("line %d: %s\n", __LINE__, #c); bad = 1; } } while (0)
#define SAME(a, ...) CHECK(!memcmp(a, (__typeof__((a)[0])[]){__VA_ARGS__}, sizeof(a)))

static _Alignas(4) uint8_t v4i8[4];
static _Alignas(16) uint32_t v4i32[4];
static float v3f32[3];
static _Alignas(2) uint8_t v12b[2];
enum { N = 10001 };

static void *run(void *arg) {
  (void)arg;
  v4i8_add_loop(v4i8, N);
  v4i32_add_loop(v4i32, N);
  v3f32_fadd_loop(v3f32, N);
  v12b_xor_loop(v12b, N);
  return 0;
}

int main(void) {
  /* single operations */
  uint8_t a3[4] = {5, 200, 7, 0x5a}, s3[3] = {9, 100, 7}, o3[3];
  v3i8_umax(a3, s3, o3);
  SAME(o3, 5, 200, 7);
  SAME(a3, 9, 200, 7, 0x5a); /* the fourth byte is not touched */

  _Alignas(4) int16_t a2[2] = {-3, 40}, s2[2] = {1, -50}, o2[2];
  v2i16_min(a2, s2, o2);
  SAME(o2, -3, 40);
  SAME(a2, -3, -50);

  float f[4] = {1, 2, 3, 99}, fs[3] = {-1, -2, -3}, fo[3];
  v3f32_xchg(f, fs, fo);
  SAME(fo, 1, 2, 3);
  SAME(f, -1, -2, -3, 99);

  _Alignas(16) uint32_t q[4] = {1, 2, 3, 4};
  uint32_t qs[4] = {9, 8, 7, 6}, qo[4];
  v4i32_load(q, qo);
  SAME(qo, 1, 2, 3, 4);
  v4i32_store(q, qs);
  SAME(q, 9, 8, 7, 6);

  /* a bool vector: the bits past lane 11 stay zero */
  _Alignas(2) uint8_t b[2] = {0, 0};
  uint8_t bs[2] = {0xa5, 0x0c}, bo[2];
  v12b_store(b, bs);
  SAME(b, 0xa5, 0x0c);
  bs[1] = 0xfc; /* lanes 10 and 11 set, the bits past them too */
  v12b_store(b, bs);
  SAME(b, 0xa5, 0x0c);
  v12b_load(b, bo);
  CHECK(bo[0] == 0xa5 && (bo[1] & 0x0f) == 0x0c);

  _Alignas(8) float c2[2] = {1.5f, -0.0f};
  float ce[2] = {1.5f, 0.0f}, cd[2] = {7, 8}, co[2];
  CHECK(!v2f32_cas(c2, ce, cd, co)); /* -0.0 and 0.0 differ bit for bit */
  SAME(co, 1.5f, -0.0f);
  ce[1] = -0.0f;
  CHECK(v2f32_cas(c2, ce, cd, co));
  SAME(c2, 7, 8);

  _Alignas(16) uint32_t x[4] = {1, 2, 3, 4};
  uint32_t xe[4] = {1, 2, 3, 5}, xd[4] = {10, 20, 30, 40}, xo[4];
  CHECK(!v4i32_cas(x, xe, xd, xo));
  SAME(xo, 1, 2, 3, 4);
  SAME(x, 1, 2, 3, 4);
  xe[3] = 4;
  CHECK(v4i32_cas(x, xe, xd, xo));
  SAME(xo, 1, 2, 3, 4);
  SAME(x, 10, 20, 30, 40);

  /* three threads at once: no update is lost */
  v12b[0] = 0x0f;
  v12b[1] = 0x03;
  pthread_t t[3];
  for (int i = 0; i < 3; i++) pthread_create(&t[i], 0, run, 0);
  for (int i = 0; i < 3; i++) pthread_join(t[i], 0);
  SAME(v4i8, 3 * N * 1 % 256, 3 * N * 2 % 256, 3 * N * 3 % 256, 3 * N * 4 % 256);
  SAME(v4i32, 3 * N, 6 * N, 9 * N, 12 * N);
  SAME(v3f32, 3 * N, 6 * N, 1.5f * N);
  /* an odd number of xors with lanes 0 2 3 7 10 11 */
  SAME(v12b, 0x0f ^ 0x8d, 0x03 ^ 0x0c);
  return bad;
}
