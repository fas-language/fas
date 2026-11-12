#include <stdio.h>
#include <stdbit.h>
#include <stdckdint.h>
#include <stdbool.h>
static unsigned long long st = 88172645463325252ULL;
static unsigned long long rnd(void) { st ^= st << 13; st ^= st >> 7; st ^= st << 17; return st; }
/* edge values first, then random ones of random magnitude */
static unsigned long long val(int i) {
  static const unsigned long long e[] = { 0, 1, 2, 3, ~0ULL, ~0ULL - 1, 1ULL << 63, (1ULL << 63) - 1, 0x80, 0x7f, 0xff, 0x8000, 0x7fff, 0xffff, 0x80000000, 0x7fffffff, 0xffffffff, 0x100000000 };
  if (i < (int)(sizeof e / sizeof e[0])) return e[i];
  unsigned long long r = rnd(); return rnd() % 2 ? r >> (rnd() % 64) : r;
}
static int fails;

unsigned char ckd_add_s32_s32_s32(int *, int, int);
static void t_ckd_add_s32_s32_s32(void) {
  for (int i = 0; i < 3000; i++) {
    int a = (int)val(i); int b = (int)val(i < 18 ? 17 - i : i + 7);
    if (i % 5 == 1) b = (int)val(i / 5);
    int x = 0, y = 0;
    bool o1 = ckd_add(&x, a, b);
    bool o2 = ckd_add_s32_s32_s32(&y, a, b);
    if (o1 != o2 || x != y) { if (fails++ < 20) printf("FAIL ckd_add_s32_s32_s32 %lld %lld: c %d %lld fir %d %lld\n", (long long)a, (long long)b, o1, (long long)x, o2, (long long)y); }
  }
}
unsigned char ckd_add_u32_u32_u32(unsigned *, unsigned, unsigned);
static void t_ckd_add_u32_u32_u32(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned a = (unsigned)val(i); unsigned b = (unsigned)val(i < 18 ? 17 - i : i + 7);
    if (i % 5 == 1) b = (unsigned)val(i / 5);
    unsigned x = 0, y = 0;
    bool o1 = ckd_add(&x, a, b);
    bool o2 = ckd_add_u32_u32_u32(&y, a, b);
    if (o1 != o2 || x != y) { if (fails++ < 20) printf("FAIL ckd_add_u32_u32_u32 %lld %lld: c %d %lld fir %d %lld\n", (long long)a, (long long)b, o1, (long long)x, o2, (long long)y); }
  }
}
unsigned char ckd_add_s64_u64_s64(long *, long, unsigned long);
static void t_ckd_add_s64_u64_s64(void) {
  for (int i = 0; i < 3000; i++) {
    long a = (long)val(i); unsigned long b = (unsigned long)val(i < 18 ? 17 - i : i + 7);
    if (i % 5 == 1) b = (unsigned long)val(i / 5);
    long x = 0, y = 0;
    bool o1 = ckd_add(&x, a, b);
    bool o2 = ckd_add_s64_u64_s64(&y, a, b);
    if (o1 != o2 || x != y) { if (fails++ < 20) printf("FAIL ckd_add_s64_u64_s64 %lld %lld: c %d %lld fir %d %lld\n", (long long)a, (long long)b, o1, (long long)x, o2, (long long)y); }
  }
}
unsigned char ckd_add_u64_s64_u64(unsigned long *, unsigned long, long);
static void t_ckd_add_u64_s64_u64(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned long a = (unsigned long)val(i); long b = (long)val(i < 18 ? 17 - i : i + 7);
    if (i % 5 == 1) b = (long)val(i / 5);
    unsigned long x = 0, y = 0;
    bool o1 = ckd_add(&x, a, b);
    bool o2 = ckd_add_u64_s64_u64(&y, a, b);
    if (o1 != o2 || x != y) { if (fails++ < 20) printf("FAIL ckd_add_u64_s64_u64 %lld %lld: c %d %lld fir %d %lld\n", (long long)a, (long long)b, o1, (long long)x, o2, (long long)y); }
  }
}
unsigned char ckd_add_s64_s64_s8(signed char *, long, long);
static void t_ckd_add_s64_s64_s8(void) {
  for (int i = 0; i < 3000; i++) {
    long a = (long)val(i); long b = (long)val(i < 18 ? 17 - i : i + 7);
    if (i % 5 == 1) b = (long)val(i / 5);
    signed char x = 0, y = 0;
    bool o1 = ckd_add(&x, a, b);
    bool o2 = ckd_add_s64_s64_s8(&y, a, b);
    if (o1 != o2 || x != y) { if (fails++ < 20) printf("FAIL ckd_add_s64_s64_s8 %lld %lld: c %d %lld fir %d %lld\n", (long long)a, (long long)b, o1, (long long)x, o2, (long long)y); }
  }
}
unsigned char ckd_add_u8_s16_u16(unsigned short *, unsigned char, short);
static void t_ckd_add_u8_s16_u16(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned char a = (unsigned char)val(i); short b = (short)val(i < 18 ? 17 - i : i + 7);
    if (i % 5 == 1) b = (short)val(i / 5);
    unsigned short x = 0, y = 0;
    bool o1 = ckd_add(&x, a, b);
    bool o2 = ckd_add_u8_s16_u16(&y, a, b);
    if (o1 != o2 || x != y) { if (fails++ < 20) printf("FAIL ckd_add_u8_s16_u16 %lld %lld: c %d %lld fir %d %lld\n", (long long)a, (long long)b, o1, (long long)x, o2, (long long)y); }
  }
}
unsigned char ckd_add_s32_u64_s16(short *, int, unsigned long);
static void t_ckd_add_s32_u64_s16(void) {
  for (int i = 0; i < 3000; i++) {
    int a = (int)val(i); unsigned long b = (unsigned long)val(i < 18 ? 17 - i : i + 7);
    if (i % 5 == 1) b = (unsigned long)val(i / 5);
    short x = 0, y = 0;
    bool o1 = ckd_add(&x, a, b);
    bool o2 = ckd_add_s32_u64_s16(&y, a, b);
    if (o1 != o2 || x != y) { if (fails++ < 20) printf("FAIL ckd_add_s32_u64_s16 %lld %lld: c %d %lld fir %d %lld\n", (long long)a, (long long)b, o1, (long long)x, o2, (long long)y); }
  }
}
unsigned char ckd_add_u64_u64_s64(long *, unsigned long, unsigned long);
static void t_ckd_add_u64_u64_s64(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned long a = (unsigned long)val(i); unsigned long b = (unsigned long)val(i < 18 ? 17 - i : i + 7);
    if (i % 5 == 1) b = (unsigned long)val(i / 5);
    long x = 0, y = 0;
    bool o1 = ckd_add(&x, a, b);
    bool o2 = ckd_add_u64_u64_s64(&y, a, b);
    if (o1 != o2 || x != y) { if (fails++ < 20) printf("FAIL ckd_add_u64_u64_s64 %lld %lld: c %d %lld fir %d %lld\n", (long long)a, (long long)b, o1, (long long)x, o2, (long long)y); }
  }
}
unsigned char ckd_add_s8_s8_u64(unsigned long *, signed char, signed char);
static void t_ckd_add_s8_s8_u64(void) {
  for (int i = 0; i < 3000; i++) {
    signed char a = (signed char)val(i); signed char b = (signed char)val(i < 18 ? 17 - i : i + 7);
    if (i % 5 == 1) b = (signed char)val(i / 5);
    unsigned long x = 0, y = 0;
    bool o1 = ckd_add(&x, a, b);
    bool o2 = ckd_add_s8_s8_u64(&y, a, b);
    if (o1 != o2 || x != y) { if (fails++ < 20) printf("FAIL ckd_add_s8_s8_u64 %lld %lld: c %d %lld fir %d %lld\n", (long long)a, (long long)b, o1, (long long)x, o2, (long long)y); }
  }
}
unsigned char ckd_add_s64_s64_u32(unsigned *, long, long);
static void t_ckd_add_s64_s64_u32(void) {
  for (int i = 0; i < 3000; i++) {
    long a = (long)val(i); long b = (long)val(i < 18 ? 17 - i : i + 7);
    if (i % 5 == 1) b = (long)val(i / 5);
    unsigned x = 0, y = 0;
    bool o1 = ckd_add(&x, a, b);
    bool o2 = ckd_add_s64_s64_u32(&y, a, b);
    if (o1 != o2 || x != y) { if (fails++ < 20) printf("FAIL ckd_add_s64_s64_u32 %lld %lld: c %d %lld fir %d %lld\n", (long long)a, (long long)b, o1, (long long)x, o2, (long long)y); }
  }
}
unsigned char ckd_add_u16_u16_s8(signed char *, unsigned short, unsigned short);
static void t_ckd_add_u16_u16_s8(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned short a = (unsigned short)val(i); unsigned short b = (unsigned short)val(i < 18 ? 17 - i : i + 7);
    if (i % 5 == 1) b = (unsigned short)val(i / 5);
    signed char x = 0, y = 0;
    bool o1 = ckd_add(&x, a, b);
    bool o2 = ckd_add_u16_u16_s8(&y, a, b);
    if (o1 != o2 || x != y) { if (fails++ < 20) printf("FAIL ckd_add_u16_u16_s8 %lld %lld: c %d %lld fir %d %lld\n", (long long)a, (long long)b, o1, (long long)x, o2, (long long)y); }
  }
}
unsigned char ckd_add_s16_u32_u8(unsigned char *, short, unsigned);
static void t_ckd_add_s16_u32_u8(void) {
  for (int i = 0; i < 3000; i++) {
    short a = (short)val(i); unsigned b = (unsigned)val(i < 18 ? 17 - i : i + 7);
    if (i % 5 == 1) b = (unsigned)val(i / 5);
    unsigned char x = 0, y = 0;
    bool o1 = ckd_add(&x, a, b);
    bool o2 = ckd_add_s16_u32_u8(&y, a, b);
    if (o1 != o2 || x != y) { if (fails++ < 20) printf("FAIL ckd_add_s16_u32_u8 %lld %lld: c %d %lld fir %d %lld\n", (long long)a, (long long)b, o1, (long long)x, o2, (long long)y); }
  }
}
unsigned char ckd_sub_s32_s32_s32(int *, int, int);
static void t_ckd_sub_s32_s32_s32(void) {
  for (int i = 0; i < 3000; i++) {
    int a = (int)val(i); int b = (int)val(i < 18 ? 17 - i : i + 7);
    if (i % 5 == 1) b = (int)val(i / 5);
    int x = 0, y = 0;
    bool o1 = ckd_sub(&x, a, b);
    bool o2 = ckd_sub_s32_s32_s32(&y, a, b);
    if (o1 != o2 || x != y) { if (fails++ < 20) printf("FAIL ckd_sub_s32_s32_s32 %lld %lld: c %d %lld fir %d %lld\n", (long long)a, (long long)b, o1, (long long)x, o2, (long long)y); }
  }
}
unsigned char ckd_sub_u32_u32_u32(unsigned *, unsigned, unsigned);
static void t_ckd_sub_u32_u32_u32(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned a = (unsigned)val(i); unsigned b = (unsigned)val(i < 18 ? 17 - i : i + 7);
    if (i % 5 == 1) b = (unsigned)val(i / 5);
    unsigned x = 0, y = 0;
    bool o1 = ckd_sub(&x, a, b);
    bool o2 = ckd_sub_u32_u32_u32(&y, a, b);
    if (o1 != o2 || x != y) { if (fails++ < 20) printf("FAIL ckd_sub_u32_u32_u32 %lld %lld: c %d %lld fir %d %lld\n", (long long)a, (long long)b, o1, (long long)x, o2, (long long)y); }
  }
}
unsigned char ckd_sub_s64_u64_s64(long *, long, unsigned long);
static void t_ckd_sub_s64_u64_s64(void) {
  for (int i = 0; i < 3000; i++) {
    long a = (long)val(i); unsigned long b = (unsigned long)val(i < 18 ? 17 - i : i + 7);
    if (i % 5 == 1) b = (unsigned long)val(i / 5);
    long x = 0, y = 0;
    bool o1 = ckd_sub(&x, a, b);
    bool o2 = ckd_sub_s64_u64_s64(&y, a, b);
    if (o1 != o2 || x != y) { if (fails++ < 20) printf("FAIL ckd_sub_s64_u64_s64 %lld %lld: c %d %lld fir %d %lld\n", (long long)a, (long long)b, o1, (long long)x, o2, (long long)y); }
  }
}
unsigned char ckd_sub_u64_s64_u64(unsigned long *, unsigned long, long);
static void t_ckd_sub_u64_s64_u64(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned long a = (unsigned long)val(i); long b = (long)val(i < 18 ? 17 - i : i + 7);
    if (i % 5 == 1) b = (long)val(i / 5);
    unsigned long x = 0, y = 0;
    bool o1 = ckd_sub(&x, a, b);
    bool o2 = ckd_sub_u64_s64_u64(&y, a, b);
    if (o1 != o2 || x != y) { if (fails++ < 20) printf("FAIL ckd_sub_u64_s64_u64 %lld %lld: c %d %lld fir %d %lld\n", (long long)a, (long long)b, o1, (long long)x, o2, (long long)y); }
  }
}
unsigned char ckd_sub_s64_s64_s8(signed char *, long, long);
static void t_ckd_sub_s64_s64_s8(void) {
  for (int i = 0; i < 3000; i++) {
    long a = (long)val(i); long b = (long)val(i < 18 ? 17 - i : i + 7);
    if (i % 5 == 1) b = (long)val(i / 5);
    signed char x = 0, y = 0;
    bool o1 = ckd_sub(&x, a, b);
    bool o2 = ckd_sub_s64_s64_s8(&y, a, b);
    if (o1 != o2 || x != y) { if (fails++ < 20) printf("FAIL ckd_sub_s64_s64_s8 %lld %lld: c %d %lld fir %d %lld\n", (long long)a, (long long)b, o1, (long long)x, o2, (long long)y); }
  }
}
unsigned char ckd_sub_u8_s16_u16(unsigned short *, unsigned char, short);
static void t_ckd_sub_u8_s16_u16(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned char a = (unsigned char)val(i); short b = (short)val(i < 18 ? 17 - i : i + 7);
    if (i % 5 == 1) b = (short)val(i / 5);
    unsigned short x = 0, y = 0;
    bool o1 = ckd_sub(&x, a, b);
    bool o2 = ckd_sub_u8_s16_u16(&y, a, b);
    if (o1 != o2 || x != y) { if (fails++ < 20) printf("FAIL ckd_sub_u8_s16_u16 %lld %lld: c %d %lld fir %d %lld\n", (long long)a, (long long)b, o1, (long long)x, o2, (long long)y); }
  }
}
unsigned char ckd_sub_s32_u64_s16(short *, int, unsigned long);
static void t_ckd_sub_s32_u64_s16(void) {
  for (int i = 0; i < 3000; i++) {
    int a = (int)val(i); unsigned long b = (unsigned long)val(i < 18 ? 17 - i : i + 7);
    if (i % 5 == 1) b = (unsigned long)val(i / 5);
    short x = 0, y = 0;
    bool o1 = ckd_sub(&x, a, b);
    bool o2 = ckd_sub_s32_u64_s16(&y, a, b);
    if (o1 != o2 || x != y) { if (fails++ < 20) printf("FAIL ckd_sub_s32_u64_s16 %lld %lld: c %d %lld fir %d %lld\n", (long long)a, (long long)b, o1, (long long)x, o2, (long long)y); }
  }
}
unsigned char ckd_sub_u64_u64_s64(long *, unsigned long, unsigned long);
static void t_ckd_sub_u64_u64_s64(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned long a = (unsigned long)val(i); unsigned long b = (unsigned long)val(i < 18 ? 17 - i : i + 7);
    if (i % 5 == 1) b = (unsigned long)val(i / 5);
    long x = 0, y = 0;
    bool o1 = ckd_sub(&x, a, b);
    bool o2 = ckd_sub_u64_u64_s64(&y, a, b);
    if (o1 != o2 || x != y) { if (fails++ < 20) printf("FAIL ckd_sub_u64_u64_s64 %lld %lld: c %d %lld fir %d %lld\n", (long long)a, (long long)b, o1, (long long)x, o2, (long long)y); }
  }
}
unsigned char ckd_sub_s8_s8_u64(unsigned long *, signed char, signed char);
static void t_ckd_sub_s8_s8_u64(void) {
  for (int i = 0; i < 3000; i++) {
    signed char a = (signed char)val(i); signed char b = (signed char)val(i < 18 ? 17 - i : i + 7);
    if (i % 5 == 1) b = (signed char)val(i / 5);
    unsigned long x = 0, y = 0;
    bool o1 = ckd_sub(&x, a, b);
    bool o2 = ckd_sub_s8_s8_u64(&y, a, b);
    if (o1 != o2 || x != y) { if (fails++ < 20) printf("FAIL ckd_sub_s8_s8_u64 %lld %lld: c %d %lld fir %d %lld\n", (long long)a, (long long)b, o1, (long long)x, o2, (long long)y); }
  }
}
unsigned char ckd_sub_s64_s64_u32(unsigned *, long, long);
static void t_ckd_sub_s64_s64_u32(void) {
  for (int i = 0; i < 3000; i++) {
    long a = (long)val(i); long b = (long)val(i < 18 ? 17 - i : i + 7);
    if (i % 5 == 1) b = (long)val(i / 5);
    unsigned x = 0, y = 0;
    bool o1 = ckd_sub(&x, a, b);
    bool o2 = ckd_sub_s64_s64_u32(&y, a, b);
    if (o1 != o2 || x != y) { if (fails++ < 20) printf("FAIL ckd_sub_s64_s64_u32 %lld %lld: c %d %lld fir %d %lld\n", (long long)a, (long long)b, o1, (long long)x, o2, (long long)y); }
  }
}
unsigned char ckd_sub_u16_u16_s8(signed char *, unsigned short, unsigned short);
static void t_ckd_sub_u16_u16_s8(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned short a = (unsigned short)val(i); unsigned short b = (unsigned short)val(i < 18 ? 17 - i : i + 7);
    if (i % 5 == 1) b = (unsigned short)val(i / 5);
    signed char x = 0, y = 0;
    bool o1 = ckd_sub(&x, a, b);
    bool o2 = ckd_sub_u16_u16_s8(&y, a, b);
    if (o1 != o2 || x != y) { if (fails++ < 20) printf("FAIL ckd_sub_u16_u16_s8 %lld %lld: c %d %lld fir %d %lld\n", (long long)a, (long long)b, o1, (long long)x, o2, (long long)y); }
  }
}
unsigned char ckd_sub_s16_u32_u8(unsigned char *, short, unsigned);
static void t_ckd_sub_s16_u32_u8(void) {
  for (int i = 0; i < 3000; i++) {
    short a = (short)val(i); unsigned b = (unsigned)val(i < 18 ? 17 - i : i + 7);
    if (i % 5 == 1) b = (unsigned)val(i / 5);
    unsigned char x = 0, y = 0;
    bool o1 = ckd_sub(&x, a, b);
    bool o2 = ckd_sub_s16_u32_u8(&y, a, b);
    if (o1 != o2 || x != y) { if (fails++ < 20) printf("FAIL ckd_sub_s16_u32_u8 %lld %lld: c %d %lld fir %d %lld\n", (long long)a, (long long)b, o1, (long long)x, o2, (long long)y); }
  }
}
unsigned char ckd_mul_s32_s32_s32(int *, int, int);
static void t_ckd_mul_s32_s32_s32(void) {
  for (int i = 0; i < 3000; i++) {
    int a = (int)val(i); int b = (int)val(i < 18 ? 17 - i : i + 7);
    if (i % 5 == 1) b = (int)val(i / 5);
    int x = 0, y = 0;
    bool o1 = ckd_mul(&x, a, b);
    bool o2 = ckd_mul_s32_s32_s32(&y, a, b);
    if (o1 != o2 || x != y) { if (fails++ < 20) printf("FAIL ckd_mul_s32_s32_s32 %lld %lld: c %d %lld fir %d %lld\n", (long long)a, (long long)b, o1, (long long)x, o2, (long long)y); }
  }
}
unsigned char ckd_mul_u32_u32_u32(unsigned *, unsigned, unsigned);
static void t_ckd_mul_u32_u32_u32(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned a = (unsigned)val(i); unsigned b = (unsigned)val(i < 18 ? 17 - i : i + 7);
    if (i % 5 == 1) b = (unsigned)val(i / 5);
    unsigned x = 0, y = 0;
    bool o1 = ckd_mul(&x, a, b);
    bool o2 = ckd_mul_u32_u32_u32(&y, a, b);
    if (o1 != o2 || x != y) { if (fails++ < 20) printf("FAIL ckd_mul_u32_u32_u32 %lld %lld: c %d %lld fir %d %lld\n", (long long)a, (long long)b, o1, (long long)x, o2, (long long)y); }
  }
}
unsigned char ckd_mul_s64_u64_s64(long *, long, unsigned long);
static void t_ckd_mul_s64_u64_s64(void) {
  for (int i = 0; i < 3000; i++) {
    long a = (long)val(i); unsigned long b = (unsigned long)val(i < 18 ? 17 - i : i + 7);
    if (i % 5 == 1) b = (unsigned long)val(i / 5);
    long x = 0, y = 0;
    bool o1 = ckd_mul(&x, a, b);
    bool o2 = ckd_mul_s64_u64_s64(&y, a, b);
    if (o1 != o2 || x != y) { if (fails++ < 20) printf("FAIL ckd_mul_s64_u64_s64 %lld %lld: c %d %lld fir %d %lld\n", (long long)a, (long long)b, o1, (long long)x, o2, (long long)y); }
  }
}
unsigned char ckd_mul_u64_s64_u64(unsigned long *, unsigned long, long);
static void t_ckd_mul_u64_s64_u64(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned long a = (unsigned long)val(i); long b = (long)val(i < 18 ? 17 - i : i + 7);
    if (i % 5 == 1) b = (long)val(i / 5);
    unsigned long x = 0, y = 0;
    bool o1 = ckd_mul(&x, a, b);
    bool o2 = ckd_mul_u64_s64_u64(&y, a, b);
    if (o1 != o2 || x != y) { if (fails++ < 20) printf("FAIL ckd_mul_u64_s64_u64 %lld %lld: c %d %lld fir %d %lld\n", (long long)a, (long long)b, o1, (long long)x, o2, (long long)y); }
  }
}
unsigned char ckd_mul_s64_s64_s8(signed char *, long, long);
static void t_ckd_mul_s64_s64_s8(void) {
  for (int i = 0; i < 3000; i++) {
    long a = (long)val(i); long b = (long)val(i < 18 ? 17 - i : i + 7);
    if (i % 5 == 1) b = (long)val(i / 5);
    signed char x = 0, y = 0;
    bool o1 = ckd_mul(&x, a, b);
    bool o2 = ckd_mul_s64_s64_s8(&y, a, b);
    if (o1 != o2 || x != y) { if (fails++ < 20) printf("FAIL ckd_mul_s64_s64_s8 %lld %lld: c %d %lld fir %d %lld\n", (long long)a, (long long)b, o1, (long long)x, o2, (long long)y); }
  }
}
unsigned char ckd_mul_u8_s16_u16(unsigned short *, unsigned char, short);
static void t_ckd_mul_u8_s16_u16(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned char a = (unsigned char)val(i); short b = (short)val(i < 18 ? 17 - i : i + 7);
    if (i % 5 == 1) b = (short)val(i / 5);
    unsigned short x = 0, y = 0;
    bool o1 = ckd_mul(&x, a, b);
    bool o2 = ckd_mul_u8_s16_u16(&y, a, b);
    if (o1 != o2 || x != y) { if (fails++ < 20) printf("FAIL ckd_mul_u8_s16_u16 %lld %lld: c %d %lld fir %d %lld\n", (long long)a, (long long)b, o1, (long long)x, o2, (long long)y); }
  }
}
unsigned char ckd_mul_s32_u64_s16(short *, int, unsigned long);
static void t_ckd_mul_s32_u64_s16(void) {
  for (int i = 0; i < 3000; i++) {
    int a = (int)val(i); unsigned long b = (unsigned long)val(i < 18 ? 17 - i : i + 7);
    if (i % 5 == 1) b = (unsigned long)val(i / 5);
    short x = 0, y = 0;
    bool o1 = ckd_mul(&x, a, b);
    bool o2 = ckd_mul_s32_u64_s16(&y, a, b);
    if (o1 != o2 || x != y) { if (fails++ < 20) printf("FAIL ckd_mul_s32_u64_s16 %lld %lld: c %d %lld fir %d %lld\n", (long long)a, (long long)b, o1, (long long)x, o2, (long long)y); }
  }
}
unsigned char ckd_mul_u64_u64_s64(long *, unsigned long, unsigned long);
static void t_ckd_mul_u64_u64_s64(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned long a = (unsigned long)val(i); unsigned long b = (unsigned long)val(i < 18 ? 17 - i : i + 7);
    if (i % 5 == 1) b = (unsigned long)val(i / 5);
    long x = 0, y = 0;
    bool o1 = ckd_mul(&x, a, b);
    bool o2 = ckd_mul_u64_u64_s64(&y, a, b);
    if (o1 != o2 || x != y) { if (fails++ < 20) printf("FAIL ckd_mul_u64_u64_s64 %lld %lld: c %d %lld fir %d %lld\n", (long long)a, (long long)b, o1, (long long)x, o2, (long long)y); }
  }
}
unsigned char ckd_mul_s8_s8_u64(unsigned long *, signed char, signed char);
static void t_ckd_mul_s8_s8_u64(void) {
  for (int i = 0; i < 3000; i++) {
    signed char a = (signed char)val(i); signed char b = (signed char)val(i < 18 ? 17 - i : i + 7);
    if (i % 5 == 1) b = (signed char)val(i / 5);
    unsigned long x = 0, y = 0;
    bool o1 = ckd_mul(&x, a, b);
    bool o2 = ckd_mul_s8_s8_u64(&y, a, b);
    if (o1 != o2 || x != y) { if (fails++ < 20) printf("FAIL ckd_mul_s8_s8_u64 %lld %lld: c %d %lld fir %d %lld\n", (long long)a, (long long)b, o1, (long long)x, o2, (long long)y); }
  }
}
unsigned char ckd_mul_s64_s64_u32(unsigned *, long, long);
static void t_ckd_mul_s64_s64_u32(void) {
  for (int i = 0; i < 3000; i++) {
    long a = (long)val(i); long b = (long)val(i < 18 ? 17 - i : i + 7);
    if (i % 5 == 1) b = (long)val(i / 5);
    unsigned x = 0, y = 0;
    bool o1 = ckd_mul(&x, a, b);
    bool o2 = ckd_mul_s64_s64_u32(&y, a, b);
    if (o1 != o2 || x != y) { if (fails++ < 20) printf("FAIL ckd_mul_s64_s64_u32 %lld %lld: c %d %lld fir %d %lld\n", (long long)a, (long long)b, o1, (long long)x, o2, (long long)y); }
  }
}
unsigned char ckd_mul_u16_u16_s8(signed char *, unsigned short, unsigned short);
static void t_ckd_mul_u16_u16_s8(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned short a = (unsigned short)val(i); unsigned short b = (unsigned short)val(i < 18 ? 17 - i : i + 7);
    if (i % 5 == 1) b = (unsigned short)val(i / 5);
    signed char x = 0, y = 0;
    bool o1 = ckd_mul(&x, a, b);
    bool o2 = ckd_mul_u16_u16_s8(&y, a, b);
    if (o1 != o2 || x != y) { if (fails++ < 20) printf("FAIL ckd_mul_u16_u16_s8 %lld %lld: c %d %lld fir %d %lld\n", (long long)a, (long long)b, o1, (long long)x, o2, (long long)y); }
  }
}
unsigned char ckd_mul_s16_u32_u8(unsigned char *, short, unsigned);
static void t_ckd_mul_s16_u32_u8(void) {
  for (int i = 0; i < 3000; i++) {
    short a = (short)val(i); unsigned b = (unsigned)val(i < 18 ? 17 - i : i + 7);
    if (i % 5 == 1) b = (unsigned)val(i / 5);
    unsigned char x = 0, y = 0;
    bool o1 = ckd_mul(&x, a, b);
    bool o2 = ckd_mul_s16_u32_u8(&y, a, b);
    if (o1 != o2 || x != y) { if (fails++ < 20) printf("FAIL ckd_mul_s16_u32_u8 %lld %lld: c %d %lld fir %d %lld\n", (long long)a, (long long)b, o1, (long long)x, o2, (long long)y); }
  }
}
unsigned sb_leading_zeros_uc(unsigned char);
static void t_sb_leading_zeros_uc(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned char x = (unsigned char)val(i);
    
    unsigned long long a = stdc_leading_zeros_uc(x), b = sb_leading_zeros_uc(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_leading_zeros_uc(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_leading_ones_uc(unsigned char);
static void t_sb_leading_ones_uc(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned char x = (unsigned char)val(i);
    
    unsigned long long a = stdc_leading_ones_uc(x), b = sb_leading_ones_uc(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_leading_ones_uc(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_trailing_zeros_uc(unsigned char);
static void t_sb_trailing_zeros_uc(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned char x = (unsigned char)val(i);
    
    unsigned long long a = stdc_trailing_zeros_uc(x), b = sb_trailing_zeros_uc(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_trailing_zeros_uc(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_trailing_ones_uc(unsigned char);
static void t_sb_trailing_ones_uc(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned char x = (unsigned char)val(i);
    
    unsigned long long a = stdc_trailing_ones_uc(x), b = sb_trailing_ones_uc(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_trailing_ones_uc(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_first_leading_zero_uc(unsigned char);
static void t_sb_first_leading_zero_uc(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned char x = (unsigned char)val(i);
    
    unsigned long long a = stdc_first_leading_zero_uc(x), b = sb_first_leading_zero_uc(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_first_leading_zero_uc(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_first_leading_one_uc(unsigned char);
static void t_sb_first_leading_one_uc(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned char x = (unsigned char)val(i);
    
    unsigned long long a = stdc_first_leading_one_uc(x), b = sb_first_leading_one_uc(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_first_leading_one_uc(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_first_trailing_zero_uc(unsigned char);
static void t_sb_first_trailing_zero_uc(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned char x = (unsigned char)val(i);
    
    unsigned long long a = stdc_first_trailing_zero_uc(x), b = sb_first_trailing_zero_uc(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_first_trailing_zero_uc(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_first_trailing_one_uc(unsigned char);
static void t_sb_first_trailing_one_uc(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned char x = (unsigned char)val(i);
    
    unsigned long long a = stdc_first_trailing_one_uc(x), b = sb_first_trailing_one_uc(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_first_trailing_one_uc(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_count_zeros_uc(unsigned char);
static void t_sb_count_zeros_uc(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned char x = (unsigned char)val(i);
    
    unsigned long long a = stdc_count_zeros_uc(x), b = sb_count_zeros_uc(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_count_zeros_uc(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_count_ones_uc(unsigned char);
static void t_sb_count_ones_uc(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned char x = (unsigned char)val(i);
    
    unsigned long long a = stdc_count_ones_uc(x), b = sb_count_ones_uc(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_count_ones_uc(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_has_single_bit_uc(unsigned char);
static void t_sb_has_single_bit_uc(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned char x = (unsigned char)val(i);
    
    unsigned long long a = stdc_has_single_bit_uc(x), b = sb_has_single_bit_uc(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_has_single_bit_uc(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_bit_width_uc(unsigned char);
static void t_sb_bit_width_uc(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned char x = (unsigned char)val(i);
    
    unsigned long long a = stdc_bit_width_uc(x), b = sb_bit_width_uc(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_bit_width_uc(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned char sb_bit_floor_uc(unsigned char);
static void t_sb_bit_floor_uc(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned char x = (unsigned char)val(i);
    
    unsigned long long a = stdc_bit_floor_uc(x), b = sb_bit_floor_uc(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_bit_floor_uc(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned char sb_bit_ceil_uc(unsigned char);
static void t_sb_bit_ceil_uc(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned char x = (unsigned char)val(i);
    if (x > (unsigned char)1 << 7) continue;
    unsigned long long a = stdc_bit_ceil_uc(x), b = sb_bit_ceil_uc(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_bit_ceil_uc(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_leading_zeros_us(unsigned short);
static void t_sb_leading_zeros_us(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned short x = (unsigned short)val(i);
    
    unsigned long long a = stdc_leading_zeros_us(x), b = sb_leading_zeros_us(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_leading_zeros_us(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_leading_ones_us(unsigned short);
static void t_sb_leading_ones_us(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned short x = (unsigned short)val(i);
    
    unsigned long long a = stdc_leading_ones_us(x), b = sb_leading_ones_us(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_leading_ones_us(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_trailing_zeros_us(unsigned short);
static void t_sb_trailing_zeros_us(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned short x = (unsigned short)val(i);
    
    unsigned long long a = stdc_trailing_zeros_us(x), b = sb_trailing_zeros_us(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_trailing_zeros_us(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_trailing_ones_us(unsigned short);
static void t_sb_trailing_ones_us(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned short x = (unsigned short)val(i);
    
    unsigned long long a = stdc_trailing_ones_us(x), b = sb_trailing_ones_us(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_trailing_ones_us(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_first_leading_zero_us(unsigned short);
static void t_sb_first_leading_zero_us(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned short x = (unsigned short)val(i);
    
    unsigned long long a = stdc_first_leading_zero_us(x), b = sb_first_leading_zero_us(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_first_leading_zero_us(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_first_leading_one_us(unsigned short);
static void t_sb_first_leading_one_us(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned short x = (unsigned short)val(i);
    
    unsigned long long a = stdc_first_leading_one_us(x), b = sb_first_leading_one_us(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_first_leading_one_us(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_first_trailing_zero_us(unsigned short);
static void t_sb_first_trailing_zero_us(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned short x = (unsigned short)val(i);
    
    unsigned long long a = stdc_first_trailing_zero_us(x), b = sb_first_trailing_zero_us(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_first_trailing_zero_us(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_first_trailing_one_us(unsigned short);
static void t_sb_first_trailing_one_us(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned short x = (unsigned short)val(i);
    
    unsigned long long a = stdc_first_trailing_one_us(x), b = sb_first_trailing_one_us(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_first_trailing_one_us(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_count_zeros_us(unsigned short);
static void t_sb_count_zeros_us(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned short x = (unsigned short)val(i);
    
    unsigned long long a = stdc_count_zeros_us(x), b = sb_count_zeros_us(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_count_zeros_us(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_count_ones_us(unsigned short);
static void t_sb_count_ones_us(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned short x = (unsigned short)val(i);
    
    unsigned long long a = stdc_count_ones_us(x), b = sb_count_ones_us(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_count_ones_us(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_has_single_bit_us(unsigned short);
static void t_sb_has_single_bit_us(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned short x = (unsigned short)val(i);
    
    unsigned long long a = stdc_has_single_bit_us(x), b = sb_has_single_bit_us(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_has_single_bit_us(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_bit_width_us(unsigned short);
static void t_sb_bit_width_us(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned short x = (unsigned short)val(i);
    
    unsigned long long a = stdc_bit_width_us(x), b = sb_bit_width_us(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_bit_width_us(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned short sb_bit_floor_us(unsigned short);
static void t_sb_bit_floor_us(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned short x = (unsigned short)val(i);
    
    unsigned long long a = stdc_bit_floor_us(x), b = sb_bit_floor_us(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_bit_floor_us(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned short sb_bit_ceil_us(unsigned short);
static void t_sb_bit_ceil_us(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned short x = (unsigned short)val(i);
    if (x > (unsigned short)1 << 15) continue;
    unsigned long long a = stdc_bit_ceil_us(x), b = sb_bit_ceil_us(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_bit_ceil_us(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_leading_zeros_ui(unsigned);
static void t_sb_leading_zeros_ui(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned x = (unsigned)val(i);
    
    unsigned long long a = stdc_leading_zeros_ui(x), b = sb_leading_zeros_ui(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_leading_zeros_ui(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_leading_ones_ui(unsigned);
static void t_sb_leading_ones_ui(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned x = (unsigned)val(i);
    
    unsigned long long a = stdc_leading_ones_ui(x), b = sb_leading_ones_ui(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_leading_ones_ui(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_trailing_zeros_ui(unsigned);
static void t_sb_trailing_zeros_ui(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned x = (unsigned)val(i);
    
    unsigned long long a = stdc_trailing_zeros_ui(x), b = sb_trailing_zeros_ui(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_trailing_zeros_ui(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_trailing_ones_ui(unsigned);
static void t_sb_trailing_ones_ui(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned x = (unsigned)val(i);
    
    unsigned long long a = stdc_trailing_ones_ui(x), b = sb_trailing_ones_ui(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_trailing_ones_ui(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_first_leading_zero_ui(unsigned);
static void t_sb_first_leading_zero_ui(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned x = (unsigned)val(i);
    
    unsigned long long a = stdc_first_leading_zero_ui(x), b = sb_first_leading_zero_ui(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_first_leading_zero_ui(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_first_leading_one_ui(unsigned);
static void t_sb_first_leading_one_ui(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned x = (unsigned)val(i);
    
    unsigned long long a = stdc_first_leading_one_ui(x), b = sb_first_leading_one_ui(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_first_leading_one_ui(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_first_trailing_zero_ui(unsigned);
static void t_sb_first_trailing_zero_ui(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned x = (unsigned)val(i);
    
    unsigned long long a = stdc_first_trailing_zero_ui(x), b = sb_first_trailing_zero_ui(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_first_trailing_zero_ui(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_first_trailing_one_ui(unsigned);
static void t_sb_first_trailing_one_ui(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned x = (unsigned)val(i);
    
    unsigned long long a = stdc_first_trailing_one_ui(x), b = sb_first_trailing_one_ui(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_first_trailing_one_ui(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_count_zeros_ui(unsigned);
static void t_sb_count_zeros_ui(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned x = (unsigned)val(i);
    
    unsigned long long a = stdc_count_zeros_ui(x), b = sb_count_zeros_ui(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_count_zeros_ui(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_count_ones_ui(unsigned);
static void t_sb_count_ones_ui(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned x = (unsigned)val(i);
    
    unsigned long long a = stdc_count_ones_ui(x), b = sb_count_ones_ui(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_count_ones_ui(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_has_single_bit_ui(unsigned);
static void t_sb_has_single_bit_ui(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned x = (unsigned)val(i);
    
    unsigned long long a = stdc_has_single_bit_ui(x), b = sb_has_single_bit_ui(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_has_single_bit_ui(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_bit_width_ui(unsigned);
static void t_sb_bit_width_ui(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned x = (unsigned)val(i);
    
    unsigned long long a = stdc_bit_width_ui(x), b = sb_bit_width_ui(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_bit_width_ui(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_bit_floor_ui(unsigned);
static void t_sb_bit_floor_ui(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned x = (unsigned)val(i);
    
    unsigned long long a = stdc_bit_floor_ui(x), b = sb_bit_floor_ui(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_bit_floor_ui(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_bit_ceil_ui(unsigned);
static void t_sb_bit_ceil_ui(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned x = (unsigned)val(i);
    if (x > (unsigned)1 << 31) continue;
    unsigned long long a = stdc_bit_ceil_ui(x), b = sb_bit_ceil_ui(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_bit_ceil_ui(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_leading_zeros_ul(unsigned long);
static void t_sb_leading_zeros_ul(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned long x = (unsigned long)val(i);
    
    unsigned long long a = stdc_leading_zeros_ul(x), b = sb_leading_zeros_ul(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_leading_zeros_ul(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_leading_ones_ul(unsigned long);
static void t_sb_leading_ones_ul(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned long x = (unsigned long)val(i);
    
    unsigned long long a = stdc_leading_ones_ul(x), b = sb_leading_ones_ul(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_leading_ones_ul(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_trailing_zeros_ul(unsigned long);
static void t_sb_trailing_zeros_ul(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned long x = (unsigned long)val(i);
    
    unsigned long long a = stdc_trailing_zeros_ul(x), b = sb_trailing_zeros_ul(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_trailing_zeros_ul(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_trailing_ones_ul(unsigned long);
static void t_sb_trailing_ones_ul(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned long x = (unsigned long)val(i);
    
    unsigned long long a = stdc_trailing_ones_ul(x), b = sb_trailing_ones_ul(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_trailing_ones_ul(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_first_leading_zero_ul(unsigned long);
static void t_sb_first_leading_zero_ul(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned long x = (unsigned long)val(i);
    
    unsigned long long a = stdc_first_leading_zero_ul(x), b = sb_first_leading_zero_ul(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_first_leading_zero_ul(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_first_leading_one_ul(unsigned long);
static void t_sb_first_leading_one_ul(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned long x = (unsigned long)val(i);
    
    unsigned long long a = stdc_first_leading_one_ul(x), b = sb_first_leading_one_ul(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_first_leading_one_ul(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_first_trailing_zero_ul(unsigned long);
static void t_sb_first_trailing_zero_ul(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned long x = (unsigned long)val(i);
    
    unsigned long long a = stdc_first_trailing_zero_ul(x), b = sb_first_trailing_zero_ul(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_first_trailing_zero_ul(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_first_trailing_one_ul(unsigned long);
static void t_sb_first_trailing_one_ul(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned long x = (unsigned long)val(i);
    
    unsigned long long a = stdc_first_trailing_one_ul(x), b = sb_first_trailing_one_ul(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_first_trailing_one_ul(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_count_zeros_ul(unsigned long);
static void t_sb_count_zeros_ul(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned long x = (unsigned long)val(i);
    
    unsigned long long a = stdc_count_zeros_ul(x), b = sb_count_zeros_ul(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_count_zeros_ul(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_count_ones_ul(unsigned long);
static void t_sb_count_ones_ul(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned long x = (unsigned long)val(i);
    
    unsigned long long a = stdc_count_ones_ul(x), b = sb_count_ones_ul(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_count_ones_ul(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_has_single_bit_ul(unsigned long);
static void t_sb_has_single_bit_ul(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned long x = (unsigned long)val(i);
    
    unsigned long long a = stdc_has_single_bit_ul(x), b = sb_has_single_bit_ul(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_has_single_bit_ul(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned sb_bit_width_ul(unsigned long);
static void t_sb_bit_width_ul(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned long x = (unsigned long)val(i);
    
    unsigned long long a = stdc_bit_width_ul(x), b = sb_bit_width_ul(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_bit_width_ul(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned long sb_bit_floor_ul(unsigned long);
static void t_sb_bit_floor_ul(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned long x = (unsigned long)val(i);
    
    unsigned long long a = stdc_bit_floor_ul(x), b = sb_bit_floor_ul(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_bit_floor_ul(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
unsigned long sb_bit_ceil_ul(unsigned long);
static void t_sb_bit_ceil_ul(void) {
  for (int i = 0; i < 3000; i++) {
    unsigned long x = (unsigned long)val(i);
    if (x > (unsigned long)1 << 63) continue;
    unsigned long long a = stdc_bit_ceil_ul(x), b = sb_bit_ceil_ul(x);
    if (a != b) { if (fails++ < 20) printf("FAIL stdc_bit_ceil_ul(%#llx): c %llu fir %llu\n", (unsigned long long)x, a, b); }
  }
}
int main(void) {
  t_ckd_add_s32_s32_s32();
  t_ckd_add_u32_u32_u32();
  t_ckd_add_s64_u64_s64();
  t_ckd_add_u64_s64_u64();
  t_ckd_add_s64_s64_s8();
  t_ckd_add_u8_s16_u16();
  t_ckd_add_s32_u64_s16();
  t_ckd_add_u64_u64_s64();
  t_ckd_add_s8_s8_u64();
  t_ckd_add_s64_s64_u32();
  t_ckd_add_u16_u16_s8();
  t_ckd_add_s16_u32_u8();
  t_ckd_sub_s32_s32_s32();
  t_ckd_sub_u32_u32_u32();
  t_ckd_sub_s64_u64_s64();
  t_ckd_sub_u64_s64_u64();
  t_ckd_sub_s64_s64_s8();
  t_ckd_sub_u8_s16_u16();
  t_ckd_sub_s32_u64_s16();
  t_ckd_sub_u64_u64_s64();
  t_ckd_sub_s8_s8_u64();
  t_ckd_sub_s64_s64_u32();
  t_ckd_sub_u16_u16_s8();
  t_ckd_sub_s16_u32_u8();
  t_ckd_mul_s32_s32_s32();
  t_ckd_mul_u32_u32_u32();
  t_ckd_mul_s64_u64_s64();
  t_ckd_mul_u64_s64_u64();
  t_ckd_mul_s64_s64_s8();
  t_ckd_mul_u8_s16_u16();
  t_ckd_mul_s32_u64_s16();
  t_ckd_mul_u64_u64_s64();
  t_ckd_mul_s8_s8_u64();
  t_ckd_mul_s64_s64_u32();
  t_ckd_mul_u16_u16_s8();
  t_ckd_mul_s16_u32_u8();
  t_sb_leading_zeros_uc();
  t_sb_leading_ones_uc();
  t_sb_trailing_zeros_uc();
  t_sb_trailing_ones_uc();
  t_sb_first_leading_zero_uc();
  t_sb_first_leading_one_uc();
  t_sb_first_trailing_zero_uc();
  t_sb_first_trailing_one_uc();
  t_sb_count_zeros_uc();
  t_sb_count_ones_uc();
  t_sb_has_single_bit_uc();
  t_sb_bit_width_uc();
  t_sb_bit_floor_uc();
  t_sb_bit_ceil_uc();
  t_sb_leading_zeros_us();
  t_sb_leading_ones_us();
  t_sb_trailing_zeros_us();
  t_sb_trailing_ones_us();
  t_sb_first_leading_zero_us();
  t_sb_first_leading_one_us();
  t_sb_first_trailing_zero_us();
  t_sb_first_trailing_one_us();
  t_sb_count_zeros_us();
  t_sb_count_ones_us();
  t_sb_has_single_bit_us();
  t_sb_bit_width_us();
  t_sb_bit_floor_us();
  t_sb_bit_ceil_us();
  t_sb_leading_zeros_ui();
  t_sb_leading_ones_ui();
  t_sb_trailing_zeros_ui();
  t_sb_trailing_ones_ui();
  t_sb_first_leading_zero_ui();
  t_sb_first_leading_one_ui();
  t_sb_first_trailing_zero_ui();
  t_sb_first_trailing_one_ui();
  t_sb_count_zeros_ui();
  t_sb_count_ones_ui();
  t_sb_has_single_bit_ui();
  t_sb_bit_width_ui();
  t_sb_bit_floor_ui();
  t_sb_bit_ceil_ui();
  t_sb_leading_zeros_ul();
  t_sb_leading_ones_ul();
  t_sb_trailing_zeros_ul();
  t_sb_trailing_ones_ul();
  t_sb_first_leading_zero_ul();
  t_sb_first_leading_one_ul();
  t_sb_first_trailing_zero_ul();
  t_sb_first_trailing_one_ul();
  t_sb_count_zeros_ul();
  t_sb_count_ones_ul();
  t_sb_has_single_bit_ul();
  t_sb_bit_width_ul();
  t_sb_bit_floor_ul();
  t_sb_bit_ceil_ul();
  if (fails) printf("fails=%d\n", fails);
  return fails != 0;
}
