#include "value_structs_abi.h"
#include <stdint.h>
#include <stdbool.h>

/* the C twins of value_structs_abi.fas's value structs */
typedef struct { int32_t x, y; } Pt;
typedef struct { const uint8_t *p; uint64_t n; } Span;
typedef struct { double x, y; } V2;
typedef struct { float a; int32_t b; double c; } Mix;
typedef struct { float a, b; } F2;
typedef struct { uint8_t a; bool b; uint16_t c; } Tiny;
typedef struct { Pt p; uint8_t tag; } Box;
/* fails results: the value, then the error at the next multiple of 8 */
typedef struct { _Alignas(8) Pt v; _Alignas(8) uint32_t e; } FPt;
typedef struct { _Alignas(8) Span v; _Alignas(8) uint32_t e; } FSpan;
typedef struct { _Alignas(8) F2 v; _Alignas(8) uint32_t e; } FF2;


/* C side of Fas -> C */
Pt c_pt(Pt a, int32_t k) { return (Pt){a.x * k, a.y * k}; }
/* the fourth Span no longer fits in registers: it goes on the stack */
Span c_span(Span a, Span b, Span c, Span d) {
  return (Span){d.p, a.n + b.n + c.n + d.n};
}
V2 c_v2(V2 a, double k) { return (V2){a.y * k, a.x * k}; }
Mix c_mix(Mix m) { return (Mix){m.a * 2, m.b + 1, m.c / 2}; }
F2 c_f2(F2 a) { return (F2){a.b, a.a}; }
Tiny c_tiny(Tiny t) { return (Tiny){(uint8_t)(t.a + 1), !t.b, (uint16_t)(t.c * 2)}; }
Box c_box(Box b) { return (Box){{b.p.y, b.p.x}, (uint8_t)(b.tag + 1)}; }
FPt c_fpt(int32_t x) { return x < 0 ? (FPt){{0, 0}, 0x80004321u} : (FPt){{x, -x}, 0}; }
FSpan c_fspan(int32_t x) {
  return x < 0 ? (FSpan){{0, 0}, 9} : (FSpan){{(const uint8_t *)"span", (uint64_t)x}, 0};
}
FF2 c_ff2(int32_t x) { return x < 0 ? (FF2){{0, 0}, 10} : (FF2){{x + 0.5f, x - 0.5f}, 0}; }

void *c_fns[3] = { (void *)c_fpt, (void *)c_fspan, (void *)c_ff2 };

/* C -> Fas: C calls the Fas functions directly */
Pt fas_pt(Pt a, Pt b);
Span fas_span(Span a, Span b, Span c, Span d);
V2 fas_v2(V2 a);
Mix fas_mix(Mix m);
F2 fas_f2(F2 a);
Tiny fas_tiny(Tiny t);
Box fas_box(Box b);

int c_drive(void **fns) {
  /* the fails functions are Fas functions, reached by address */
  FPt (*fas_fpt)(int32_t) = (FPt (*)(int32_t))fns[0];
  FSpan (*fas_fspan)(int32_t) = (FSpan (*)(int32_t))fns[1];
  FF2 (*fas_ff2)(int32_t) = (FF2 (*)(int32_t))fns[2];
  int bad = 0;
  Pt p = fas_pt((Pt){1, 2}, (Pt){30, 40});
  bad |= (p.x != 31 || p.y != 42) << 0;
  const uint8_t *s = (const uint8_t *)"abcd";
  Span sp = fas_span((Span){s, 1}, (Span){s, 2}, (Span){s, 3}, (Span){s + 3, 4});
  bad |= (sp.p != s + 3 || sp.n != 10) << 1;
  V2 v = fas_v2((V2){1.5, -2.5});
  bad |= (v.x != -2.5 || v.y != 1.5) << 2;
  Mix m = fas_mix((Mix){1.5f, 7, 9.0});
  bad |= (m.a != 3.0f || m.b != 8 || m.c != 4.5) << 3;
  F2 f = fas_f2((F2){1.25f, 2.75f});
  bad |= (f.a != 2.75f || f.b != 1.25f) << 4;
  Tiny t = fas_tiny((Tiny){5, true, 300});
  bad |= (t.a != 6 || t.b != false || t.c != 600) << 5;
  Box b = fas_box((Box){{3, 4}, 9});
  bad |= (b.p.x != 4 || b.p.y != 3 || b.tag != 10) << 6;
  FPt fp = fas_fpt(5);
  bad |= (fp.e != 0 || fp.v.x != 5 || fp.v.y != 50) << 7;
  fp = fas_fpt(-5);
  bad |= (fp.e != 2 || fp.v.x != 0 || fp.v.y != 0) << 8;
  FSpan fs = fas_fspan(3);
  bad |= (fs.e != 0 || fs.v.n != 3 || fs.v.p[0] != 'f') << 9;
  fs = fas_fspan(-3);
  bad |= (fs.e != 2) << 10;
  FF2 ff = fas_ff2(2);
  bad |= (ff.e != 0 || ff.v.a != 2.5f || ff.v.b != 1.5f) << 11;
  ff = fas_ff2(-2);
  bad |= (ff.e != 2) << 12;
  return bad;
}
