/* Checks the native runtime's software fma (src/backend/native/runtime/fma.fas)
   against libm's fma/fmaf on edge, subnormal, huge, near-cancellation and
   random operands. Usage: driver [count]. */
#include <stdlib.h>
#include <math.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
double fas_rt_fma64(double, double, double);
float fas_rt_fma32(float, float, float);
static uint64_t st = 0x9e3779b97f4a7c15ull;
static uint64_t rnd(void){ st ^= st<<13; st ^= st>>7; st ^= st<<17; return st; }
static double d(uint64_t b){ double x; memcpy(&x,&b,8); return x; }
static uint64_t b64(double x){ uint64_t b; memcpy(&b,&x,8); return b; }
static float f(uint32_t b){ float x; memcpy(&x,&b,4); return x; }
static uint32_t b32(float x){ uint32_t b; memcpy(&b,&x,4); return b; }
static int same64(double a, double b){ return (isnan(a)&&isnan(b)) || b64(a)==b64(b); }
static int same32(float a, float b){ return (isnan(a)&&isnan(b)) || b32(a)==b32(b); }
static uint64_t pick64(void){
  static const uint64_t edge[] = {0,0x8000000000000000ull,1,0x000fffffffffffffull,0x0010000000000000ull,
    0x7fefffffffffffffull,0x7ff0000000000000ull,0xfff0000000000000ull,0x7ff8000000000000ull,0x3ff0000000000000ull,
    0xbff0000000000000ull,0x3ff0000000000001ull,0x3fefffffffffffffull};
  uint64_t r = rnd();
  switch (r % 8) {
  case 0: return edge[(r>>8) % 13];
  case 1: return (r & 0x800fffffffffffffull) | ((uint64_t)((r>>40)%40 + 0) << 52);   /* tiny */
  case 2: return (r & 0x800fffffffffffffull) | ((uint64_t)(2046 - (r>>40)%40) << 52); /* huge */
  case 3: return (r & 0x800fffffffffffffull) | ((uint64_t)(1023 + (int)((r>>40)%60) - 30) << 52);
  case 4: return (r & 0x8000000000000fffull) | 0x3ff0000000000000ull | ((r>>20)&0xff)<<44;
  default: return rnd();
  }
}
int main(int argc, char **argv){
  long n = argc > 1 ? atol(argv[1]) : 2000000, bad = 0;
  for (long i = 0; i < n; i++) {
    double a = d(pick64()), b = d(pick64()), c = d(pick64());
    if (rnd()%3==0) c = -a*b + d(pick64()) * 1e-30 * (rnd()%2);  /* cancellation */
    double w = fma(a,b,c), g = fas_rt_fma64(a,b,c);
    if (!same64(w,g) && bad++ < 10) printf("f64 %016llx %016llx %016llx want %016llx got %016llx\n",
      (unsigned long long)b64(a),(unsigned long long)b64(b),(unsigned long long)b64(c),(unsigned long long)b64(w),(unsigned long long)b64(g));
    uint32_t x = (uint32_t)rnd(), y = (uint32_t)rnd(), z = (uint32_t)rnd();
    if (rnd()%4==0) { x = (x & 0x807fffff) | ((uint32_t)(rnd()%60)<<23); y = (y & 0x807fffff) | ((uint32_t)(rnd()%60)<<23); z &= 0x80ffffff; }
    if (rnd()%4==0) { x = (x & 0x80ffffff) | 0x3f000000; y = (y & 0x80ffffff) | 0x3f000000; float p = f(x)*f(y); z = b32(-p) ^ (uint32_t)(rnd()%8); }
    float fw = fmaf(f(x),f(y),f(z)), fg = fas_rt_fma32(f(x),f(y),f(z));
    if (!same32(fw,fg) && bad++ < 10) printf("f32 %08x %08x %08x want %08x got %08x\n", x,y,z,b32(fw),b32(fg));
  }
  printf("bad %ld of %ld\n", bad, n);
  return bad != 0;
}
