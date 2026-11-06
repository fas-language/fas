/* Checks test/fas_fir/c/vastruct.fir: structs by value through `...`. c_vlist
   is the reference reader; every combination of caller (C, Fir) and reader
   (C, Fir, Fir given a C va_list) must agree with it. Exit 0 when all agree. */
#include <stdarg.h>
#include <stdio.h>
typedef struct { long a, b; } II;
typedef struct { double x, y; } DD;
typedef struct { long a; double d; } ID;
typedef struct { float x, y; int z; } FFI;
typedef struct { long a, b, c; } BIG;
typedef struct { int a; } S1;
long fir_vr(int nl, int nd, ...);
long fir_vlist(int nl, int nd, va_list ap);
long fir_drive_0_0(int to_c), fir_drive_3_0(int to_c), fir_drive_4_7(int to_c),
    fir_drive_0_7(int to_c), fir_drive_6_8(int to_c);

#define H(v) (h = h * 31u + (unsigned long)(long)(v))
#define HD(v) H((long)((double)(v) * 4))
long c_vlist(int nl, int nd, va_list ap) {
  unsigned long h = 7;
  for (int i = 0; i < nl; i++) H(va_arg(ap, long));
  for (int i = 0; i < nd; i++) HD(va_arg(ap, double));
  II a = va_arg(ap, II); H(a.a); H(a.b);
  DD b = va_arg(ap, DD); HD(b.x); HD(b.y);
  ID c = va_arg(ap, ID); H(c.a); HD(c.d);
  FFI f = va_arg(ap, FFI); HD(f.x); HD(f.y); H(f.z);
  BIG g = va_arg(ap, BIG); H(g.a); H(g.b); H(g.c);
  S1 s = va_arg(ap, S1); H(s.a);
  DD b2 = va_arg(ap, DD); HD(b2.x); HD(b2.y);
  II a2 = va_arg(ap, II); H(a2.a); H(a2.b);
  return (long)h;
}
long c_vr(int nl, int nd, ...) { va_list ap; va_start(ap, nd); long r = c_vlist(nl, nd, ap); va_end(ap); return r; }
long via_fir(int nl, int nd, ...) { va_list ap; va_start(ap, nd); long r = fir_vlist(nl, nd, ap); va_end(ap); return r; }

static const II ii = {1001, 1002}, ii2 = {1009, 1010};
static const DD dd = {2.5, -3.25}, dd2 = {7.25, 8.5};
static const ID id = {1003, 4.75};
static const FFI ffi = {5.5f, -6.5f, 1004};
static const BIG big = {1005, 1006, 1007};
static const S1 s1 = {1008};
#define SEQ ii, dd, id, ffi, big, s1, dd2, ii2
#define L3 11L, 22L, 33L
#define L4 L3, 44L
#define L6 L4, 55L, 66L
#define D7 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5
#define D8 D7, 7.5
static int bad;
/* C caller: the C reader, the Fir reader and Fir reading C's va_list */
#define CHECK(nl, nd, ...) do { \
    long want = c_vr(nl, nd, __VA_ARGS__), f = fir_vr(nl, nd, __VA_ARGS__), \
         v = via_fir(nl, nd, __VA_ARGS__); \
    printf("C(%d,%d): c %ld fir %ld fir-va_list %ld\n", nl, nd, want, f, v); \
    bad |= f != want || v != want; } while (0)
/* Fir caller, to the C reader and to the Fir reader */
#define DRIVE(nl, nd, fn, ...) do { \
    long want = c_vr(nl, nd, __VA_ARGS__), c = fn(1), f = fn(0); \
    printf("Fir(%d,%d): c %ld fir->c %ld fir->fir %ld\n", nl, nd, want, c, f); \
    bad |= c != want || f != want; } while (0)
int main(void) {
  CHECK(0, 0, SEQ);
  CHECK(3, 0, L3, SEQ);  /* one GPR left: II goes to the stack, ID still in registers */
  CHECK(4, 0, L4, SEQ);  /* GPRs full */
  CHECK(6, 0, L6, SEQ);
  CHECK(0, 7, D7, SEQ);  /* one XMM left: DD to the stack, ID/FFI still fit */
  CHECK(0, 8, D8, SEQ);
  CHECK(3, 7, L3, D7, SEQ);
  CHECK(4, 8, L4, D8, SEQ);
  CHECK(6, 8, L6, D8, SEQ);
  DRIVE(0, 0, fir_drive_0_0, SEQ);
  DRIVE(3, 0, fir_drive_3_0, L3, SEQ);
  DRIVE(4, 7, fir_drive_4_7, L4, D7, SEQ);
  DRIVE(0, 7, fir_drive_0_7, D7, SEQ);
  DRIVE(6, 8, fir_drive_6_8, L6, D8, SEQ);
  return bad;
}
