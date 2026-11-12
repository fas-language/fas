#include <fenv.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#pragma STDC FENV_ACCESS ON
static int fails;
static const int modes[4]={FE_TONEAREST,FE_UPWARD,FE_DOWNWARD,FE_TOWARDZERO};
/* the 200-bit two's complement of v (sign-extended) */
static void to200(unsigned char *p, __int128 v) { memset(p, v < 0 ? 0xff : 0, 32); memcpy(p, &v, 16); }

float fir_div32(float, float);
__attribute__((noinline)) static float c_div32(float a, float b) { (void)b; return a/b; }
static void t_div32(float a, float b) {
  
  for (int m = 0; m < 4; m++) {
    fesetround(modes[m]); feclearexcept(FE_ALL_EXCEPT);
    volatile float x = c_div32(a, b); int fx = fetestexcept(FE_ALL_EXCEPT);
    feclearexcept(FE_ALL_EXCEPT);
    volatile float y = fir_div32(a, b); int fy = fetestexcept(FE_ALL_EXCEPT);
    fesetround(FE_TONEAREST);
    float xx = x, yy = y;
    if (memcmp(&xx, &yy, sizeof xx) != 0 && !(xx != xx && yy != yy)) { fails++; printf("FAIL value div32 mode %d\n", m); }
    if (fx != fy) { fails++; printf("FAIL flags div32 mode %d: c %#x fir %#x\n", m, fx, fy); }
  }
}
double fir_div64(double, double);
__attribute__((noinline)) static double c_div64(double a, double b) { (void)b; return a/b; }
static void t_div64(double a, double b) {
  
  for (int m = 0; m < 4; m++) {
    fesetround(modes[m]); feclearexcept(FE_ALL_EXCEPT);
    volatile double x = c_div64(a, b); int fx = fetestexcept(FE_ALL_EXCEPT);
    feclearexcept(FE_ALL_EXCEPT);
    volatile double y = fir_div64(a, b); int fy = fetestexcept(FE_ALL_EXCEPT);
    fesetround(FE_TONEAREST);
    double xx = x, yy = y;
    if (memcmp(&xx, &yy, sizeof xx) != 0 && !(xx != xx && yy != yy)) { fails++; printf("FAIL value div64 mode %d\n", m); }
    if (fx != fy) { fails++; printf("FAIL flags div64 mode %d: c %#x fir %#x\n", m, fx, fy); }
  }
}
long double fir_div80(long double, long double);
__attribute__((noinline)) static long double c_div80(long double a, long double b) { (void)b; return a/b; }
static void t_div80(long double a, long double b) {
  
  for (int m = 0; m < 4; m++) {
    fesetround(modes[m]); feclearexcept(FE_ALL_EXCEPT);
    volatile long double x = c_div80(a, b); int fx = fetestexcept(FE_ALL_EXCEPT);
    feclearexcept(FE_ALL_EXCEPT);
    volatile long double y = fir_div80(a, b); int fy = fetestexcept(FE_ALL_EXCEPT);
    fesetround(FE_TONEAREST);
    long double xx = x, yy = y;
    if (memcmp(&xx, &yy, 10) != 0 && !(xx != xx && yy != yy)) { fails++; printf("FAIL value div80 mode %d\n", m); }
    if (fx != fy) { fails++; printf("FAIL flags div80 mode %d: c %#x fir %#x\n", m, fx, fy); }
  }
}
double fir_add64(double, double);
__attribute__((noinline)) static double c_add64(double a, double b) { (void)b; return a+b; }
static void t_add64(double a, double b) {
  
  for (int m = 0; m < 4; m++) {
    fesetround(modes[m]); feclearexcept(FE_ALL_EXCEPT);
    volatile double x = c_add64(a, b); int fx = fetestexcept(FE_ALL_EXCEPT);
    feclearexcept(FE_ALL_EXCEPT);
    volatile double y = fir_add64(a, b); int fy = fetestexcept(FE_ALL_EXCEPT);
    fesetround(FE_TONEAREST);
    double xx = x, yy = y;
    if (memcmp(&xx, &yy, sizeof xx) != 0 && !(xx != xx && yy != yy)) { fails++; printf("FAIL value add64 mode %d\n", m); }
    if (fx != fy) { fails++; printf("FAIL flags add64 mode %d: c %#x fir %#x\n", m, fx, fy); }
  }
}
long double fir_mul80(long double, long double);
__attribute__((noinline)) static long double c_mul80(long double a, long double b) { (void)b; return a*b; }
static void t_mul80(long double a, long double b) {
  
  for (int m = 0; m < 4; m++) {
    fesetround(modes[m]); feclearexcept(FE_ALL_EXCEPT);
    volatile long double x = c_mul80(a, b); int fx = fetestexcept(FE_ALL_EXCEPT);
    feclearexcept(FE_ALL_EXCEPT);
    volatile long double y = fir_mul80(a, b); int fy = fetestexcept(FE_ALL_EXCEPT);
    fesetround(FE_TONEAREST);
    long double xx = x, yy = y;
    if (memcmp(&xx, &yy, 10) != 0 && !(xx != xx && yy != yy)) { fails++; printf("FAIL value mul80 mode %d\n", m); }
    if (fx != fy) { fails++; printf("FAIL flags mul80 mode %d: c %#x fir %#x\n", m, fx, fy); }
  }
}
_Float16 fir_div16(_Float16, _Float16);
__attribute__((noinline)) static _Float16 c_div16(_Float16 a, _Float16 b) { (void)b; return a/b; }
static void t_div16(_Float16 a, _Float16 b) {
  
  for (int m = 0; m < 4; m++) {
    fesetround(modes[m]); feclearexcept(FE_ALL_EXCEPT);
    volatile _Float16 x = c_div16(a, b); int fx = fetestexcept(FE_ALL_EXCEPT);
    feclearexcept(FE_ALL_EXCEPT);
    volatile _Float16 y = fir_div16(a, b); int fy = fetestexcept(FE_ALL_EXCEPT);
    fesetround(FE_TONEAREST);
    _Float16 xx = x, yy = y;
    if (memcmp(&xx, &yy, sizeof xx) != 0 && !(xx != xx && yy != yy)) { fails++; printf("FAIL value div16 mode %d\n", m); }
    if (fx != fy) { fails++; printf("FAIL flags div16 mode %d: c %#x fir %#x\n", m, fx, fy); }
  }
}
_Float16 fir_add16(_Float16, _Float16);
__attribute__((noinline)) static _Float16 c_add16(_Float16 a, _Float16 b) { (void)b; return a+b; }
static void t_add16(_Float16 a, _Float16 b) {
  
  for (int m = 0; m < 4; m++) {
    fesetround(modes[m]); feclearexcept(FE_ALL_EXCEPT);
    volatile _Float16 x = c_add16(a, b); int fx = fetestexcept(FE_ALL_EXCEPT);
    feclearexcept(FE_ALL_EXCEPT);
    volatile _Float16 y = fir_add16(a, b); int fy = fetestexcept(FE_ALL_EXCEPT);
    fesetround(FE_TONEAREST);
    _Float16 xx = x, yy = y;
    if (memcmp(&xx, &yy, sizeof xx) != 0 && !(xx != xx && yy != yy)) { fails++; printf("FAIL value add16 mode %d\n", m); }
    if (fx != fy) { fails++; printf("FAIL flags add16 mode %d: c %#x fir %#x\n", m, fx, fy); }
  }
}
double fir_sqrt64(double);
__attribute__((noinline)) static double c_sqrt64(double a, double b) { (void)b; return __builtin_sqrt(a); }
static void t_sqrt64(double a, double b) {
  
  for (int m = 0; m < 4; m++) {
    fesetround(modes[m]); feclearexcept(FE_ALL_EXCEPT);
    volatile double x = c_sqrt64(a, b); int fx = fetestexcept(FE_ALL_EXCEPT);
    feclearexcept(FE_ALL_EXCEPT);
    volatile double y = fir_sqrt64(a); int fy = fetestexcept(FE_ALL_EXCEPT);
    fesetround(FE_TONEAREST);
    double xx = x, yy = y;
    if (memcmp(&xx, &yy, sizeof xx) != 0 && !(xx != xx && yy != yy)) { fails++; printf("FAIL value sqrt64 mode %d\n", m); }
    if (fx != fy) { fails++; printf("FAIL flags sqrt64 mode %d: c %#x fir %#x\n", m, fx, fy); }
  }
}
long double fir_sqrt80(long double);
__attribute__((noinline)) static long double c_sqrt80(long double a, long double b) { (void)b; return __builtin_sqrtl(a); }
static void t_sqrt80(long double a, long double b) {
  
  for (int m = 0; m < 4; m++) {
    fesetround(modes[m]); feclearexcept(FE_ALL_EXCEPT);
    volatile long double x = c_sqrt80(a, b); int fx = fetestexcept(FE_ALL_EXCEPT);
    feclearexcept(FE_ALL_EXCEPT);
    volatile long double y = fir_sqrt80(a); int fy = fetestexcept(FE_ALL_EXCEPT);
    fesetround(FE_TONEAREST);
    long double xx = x, yy = y;
    if (memcmp(&xx, &yy, 10) != 0 && !(xx != xx && yy != yy)) { fails++; printf("FAIL value sqrt80 mode %d\n", m); }
    if (fx != fy) { fails++; printf("FAIL flags sqrt80 mode %d: c %#x fir %#x\n", m, fx, fy); }
  }
}
double fir_fma64(double, double);
__attribute__((noinline)) static double c_fma64(double a, double b) { (void)b; return __builtin_fma(a,b,a); }
static void t_fma64(double a, double b) {
  
  for (int m = 0; m < 4; m++) {
    fesetround(modes[m]); feclearexcept(FE_ALL_EXCEPT);
    volatile double x = c_fma64(a, b); int fx = fetestexcept(FE_ALL_EXCEPT);
    feclearexcept(FE_ALL_EXCEPT);
    volatile double y = fir_fma64(a, b); int fy = fetestexcept(FE_ALL_EXCEPT);
    fesetround(FE_TONEAREST);
    double xx = x, yy = y;
    if (memcmp(&xx, &yy, sizeof xx) != 0 && !(xx != xx && yy != yy)) { fails++; printf("FAIL value fma64 mode %d\n", m); }
    if (fx != fy) { fails++; printf("FAIL flags fma64 mode %d: c %#x fir %#x\n", m, fx, fy); }
  }
}
long double fir_fma80(long double, long double);
__attribute__((noinline)) static long double c_fma80(long double a, long double b) { (void)b; return __builtin_fmal(a,b,a); }
static void t_fma80(long double a, long double b) {
  
  for (int m = 0; m < 4; m++) {
    fesetround(modes[m]); feclearexcept(FE_ALL_EXCEPT);
    volatile long double x = c_fma80(a, b); int fx = fetestexcept(FE_ALL_EXCEPT);
    feclearexcept(FE_ALL_EXCEPT);
    volatile long double y = fir_fma80(a, b); int fy = fetestexcept(FE_ALL_EXCEPT);
    fesetround(FE_TONEAREST);
    long double xx = x, yy = y;
    if (memcmp(&xx, &yy, 10) != 0 && !(xx != xx && yy != yy)) { fails++; printf("FAIL value fma80 mode %d\n", m); }
    if (fx != fy) { fails++; printf("FAIL flags fma80 mode %d: c %#x fir %#x\n", m, fx, fy); }
  }
}
_Float16 fir_fmah(_Float16, _Float16);
__attribute__((noinline)) static _Float16 c_fmah(_Float16 a, _Float16 b) { (void)b; return (_Float16)__builtin_fmaf(a,b,a); }
static void t_fmah(_Float16 a, _Float16 b) {
  
  for (int m = 0; m < 4; m++) {
    fesetround(modes[m]); feclearexcept(FE_ALL_EXCEPT);
    volatile _Float16 x = c_fmah(a, b); int fx = fetestexcept(FE_ALL_EXCEPT);
    feclearexcept(FE_ALL_EXCEPT);
    volatile _Float16 y = fir_fmah(a, b); int fy = fetestexcept(FE_ALL_EXCEPT);
    fesetround(FE_TONEAREST);
    _Float16 xx = x, yy = y;
    if (memcmp(&xx, &yy, sizeof xx) != 0 && !(xx != xx && yy != yy)) { fails++; printf("FAIL value fmah mode %d\n", m); }
    if (fx != fy) { fails++; printf("FAIL flags fmah mode %d: c %#x fir %#x\n", m, fx, fy); }
  }
}
float fir_tr6432(double);
__attribute__((noinline)) static float c_tr6432(double a, double b) { (void)b; return (float)a; }
static void t_tr6432(double a, double b) {
  
  for (int m = 0; m < 4; m++) {
    fesetround(modes[m]); feclearexcept(FE_ALL_EXCEPT);
    volatile float x = c_tr6432(a, b); int fx = fetestexcept(FE_ALL_EXCEPT);
    feclearexcept(FE_ALL_EXCEPT);
    volatile float y = fir_tr6432(a); int fy = fetestexcept(FE_ALL_EXCEPT);
    fesetround(FE_TONEAREST);
    float xx = x, yy = y;
    if (memcmp(&xx, &yy, sizeof xx) != 0 && !(xx != xx && yy != yy)) { fails++; printf("FAIL value tr6432 mode %d\n", m); }
    if (fx != fy) { fails++; printf("FAIL flags tr6432 mode %d: c %#x fir %#x\n", m, fx, fy); }
  }
}
double fir_tr8064(long double);
__attribute__((noinline)) static double c_tr8064(long double a, long double b) { (void)b; return (double)a; }
static void t_tr8064(long double a, long double b) {
  
  for (int m = 0; m < 4; m++) {
    fesetround(modes[m]); feclearexcept(FE_ALL_EXCEPT);
    volatile double x = c_tr8064(a, b); int fx = fetestexcept(FE_ALL_EXCEPT);
    feclearexcept(FE_ALL_EXCEPT);
    volatile double y = fir_tr8064(a); int fy = fetestexcept(FE_ALL_EXCEPT);
    fesetround(FE_TONEAREST);
    double xx = x, yy = y;
    if (memcmp(&xx, &yy, sizeof xx) != 0 && !(xx != xx && yy != yy)) { fails++; printf("FAIL value tr8064 mode %d\n", m); }
    if (fx != fy) { fails++; printf("FAIL flags tr8064 mode %d: c %#x fir %#x\n", m, fx, fy); }
  }
}
_Float16 fir_tr6416(double);
__attribute__((noinline)) static _Float16 c_tr6416(double a, double b) { (void)b; return (_Float16)a; }
static void t_tr6416(double a, double b) {
  
  for (int m = 0; m < 4; m++) {
    fesetround(modes[m]); feclearexcept(FE_ALL_EXCEPT);
    volatile _Float16 x = c_tr6416(a, b); int fx = fetestexcept(FE_ALL_EXCEPT);
    feclearexcept(FE_ALL_EXCEPT);
    volatile _Float16 y = fir_tr6416(a); int fy = fetestexcept(FE_ALL_EXCEPT);
    fesetround(FE_TONEAREST);
    _Float16 xx = x, yy = y;
    if (memcmp(&xx, &yy, sizeof xx) != 0 && !(xx != xx && yy != yy)) { fails++; printf("FAIL value tr6416 mode %d\n", m); }
    if (fx != fy) { fails++; printf("FAIL flags tr6416 mode %d: c %#x fir %#x\n", m, fx, fy); }
  }
}
_Float16 fir_tr3216(float);
__attribute__((noinline)) static _Float16 c_tr3216(float a, float b) { (void)b; return (_Float16)a; }
static void t_tr3216(float a, float b) {
  
  for (int m = 0; m < 4; m++) {
    fesetround(modes[m]); feclearexcept(FE_ALL_EXCEPT);
    volatile _Float16 x = c_tr3216(a, b); int fx = fetestexcept(FE_ALL_EXCEPT);
    feclearexcept(FE_ALL_EXCEPT);
    volatile _Float16 y = fir_tr3216(a); int fy = fetestexcept(FE_ALL_EXCEPT);
    fesetround(FE_TONEAREST);
    _Float16 xx = x, yy = y;
    if (memcmp(&xx, &yy, sizeof xx) != 0 && !(xx != xx && yy != yy)) { fails++; printf("FAIL value tr3216 mode %d\n", m); }
    if (fx != fy) { fails++; printf("FAIL flags tr3216 mode %d: c %#x fir %#x\n", m, fx, fy); }
  }
}
float fir_si64f32(long);
__attribute__((noinline)) static float c_si64f32(long a, long b) { (void)b; return (float)a; }
static void t_si64f32(long a, long b) {
  
  for (int m = 0; m < 4; m++) {
    fesetround(modes[m]); feclearexcept(FE_ALL_EXCEPT);
    volatile float x = c_si64f32(a, b); int fx = fetestexcept(FE_ALL_EXCEPT);
    feclearexcept(FE_ALL_EXCEPT);
    volatile float y = fir_si64f32(a); int fy = fetestexcept(FE_ALL_EXCEPT);
    fesetround(FE_TONEAREST);
    float xx = x, yy = y;
    if (memcmp(&xx, &yy, sizeof xx) != 0 && !(xx != xx && yy != yy)) { fails++; printf("FAIL value si64f32 mode %d\n", m); }
    if (fx != fy) { fails++; printf("FAIL flags si64f32 mode %d: c %#x fir %#x\n", m, fx, fy); }
  }
}
double fir_si64f64(long);
__attribute__((noinline)) static double c_si64f64(long a, long b) { (void)b; return (double)a; }
static void t_si64f64(long a, long b) {
  
  for (int m = 0; m < 4; m++) {
    fesetround(modes[m]); feclearexcept(FE_ALL_EXCEPT);
    volatile double x = c_si64f64(a, b); int fx = fetestexcept(FE_ALL_EXCEPT);
    feclearexcept(FE_ALL_EXCEPT);
    volatile double y = fir_si64f64(a); int fy = fetestexcept(FE_ALL_EXCEPT);
    fesetround(FE_TONEAREST);
    double xx = x, yy = y;
    if (memcmp(&xx, &yy, sizeof xx) != 0 && !(xx != xx && yy != yy)) { fails++; printf("FAIL value si64f64 mode %d\n", m); }
    if (fx != fy) { fails++; printf("FAIL flags si64f64 mode %d: c %#x fir %#x\n", m, fx, fy); }
  }
}
double fir_ui64f64(unsigned long);
__attribute__((noinline)) static double c_ui64f64(unsigned long a, unsigned long b) { (void)b; return (double)a; }
static void t_ui64f64(unsigned long a, unsigned long b) {
  
  for (int m = 0; m < 4; m++) {
    fesetround(modes[m]); feclearexcept(FE_ALL_EXCEPT);
    volatile double x = c_ui64f64(a, b); int fx = fetestexcept(FE_ALL_EXCEPT);
    feclearexcept(FE_ALL_EXCEPT);
    volatile double y = fir_ui64f64(a); int fy = fetestexcept(FE_ALL_EXCEPT);
    fesetround(FE_TONEAREST);
    double xx = x, yy = y;
    if (memcmp(&xx, &yy, sizeof xx) != 0 && !(xx != xx && yy != yy)) { fails++; printf("FAIL value ui64f64 mode %d\n", m); }
    if (fx != fy) { fails++; printf("FAIL flags ui64f64 mode %d: c %#x fir %#x\n", m, fx, fy); }
  }
}
float fir_ui64f32(unsigned long);
__attribute__((noinline)) static float c_ui64f32(unsigned long a, unsigned long b) { (void)b; return (float)a; }
static void t_ui64f32(unsigned long a, unsigned long b) {
  
  for (int m = 0; m < 4; m++) {
    fesetround(modes[m]); feclearexcept(FE_ALL_EXCEPT);
    volatile float x = c_ui64f32(a, b); int fx = fetestexcept(FE_ALL_EXCEPT);
    feclearexcept(FE_ALL_EXCEPT);
    volatile float y = fir_ui64f32(a); int fy = fetestexcept(FE_ALL_EXCEPT);
    fesetround(FE_TONEAREST);
    float xx = x, yy = y;
    if (memcmp(&xx, &yy, sizeof xx) != 0 && !(xx != xx && yy != yy)) { fails++; printf("FAIL value ui64f32 mode %d\n", m); }
    if (fx != fy) { fails++; printf("FAIL flags ui64f32 mode %d: c %#x fir %#x\n", m, fx, fy); }
  }
}
double fir_si128f64(__int128);
__attribute__((noinline)) static double c_si128f64(__int128 a, __int128 b) { (void)b; return (double)a; }
static void t_si128f64(__int128 a, __int128 b) {
  
  for (int m = 0; m < 4; m++) {
    fesetround(modes[m]); feclearexcept(FE_ALL_EXCEPT);
    volatile double x = c_si128f64(a, b); int fx = fetestexcept(FE_ALL_EXCEPT);
    feclearexcept(FE_ALL_EXCEPT);
    volatile double y = fir_si128f64(a); int fy = fetestexcept(FE_ALL_EXCEPT);
    fesetround(FE_TONEAREST);
    double xx = x, yy = y;
    if (memcmp(&xx, &yy, sizeof xx) != 0 && !(xx != xx && yy != yy)) { fails++; printf("FAIL value si128f64 mode %d\n", m); }
    if (fx != fy) { fails++; printf("FAIL flags si128f64 mode %d: c %#x fir %#x\n", m, fx, fy); }
  }
}
float fir_ui128f32(unsigned __int128);
__attribute__((noinline)) static float c_ui128f32(unsigned __int128 a, unsigned __int128 b) { (void)b; return (float)a; }
static void t_ui128f32(unsigned __int128 a, unsigned __int128 b) {
  
  for (int m = 0; m < 4; m++) {
    fesetround(modes[m]); feclearexcept(FE_ALL_EXCEPT);
    volatile float x = c_ui128f32(a, b); int fx = fetestexcept(FE_ALL_EXCEPT);
    feclearexcept(FE_ALL_EXCEPT);
    volatile float y = fir_ui128f32(a); int fy = fetestexcept(FE_ALL_EXCEPT);
    fesetround(FE_TONEAREST);
    float xx = x, yy = y;
    if (memcmp(&xx, &yy, sizeof xx) != 0 && !(xx != xx && yy != yy)) { fails++; printf("FAIL value ui128f32 mode %d\n", m); }
    if (fx != fy) { fails++; printf("FAIL flags ui128f32 mode %d: c %#x fir %#x\n", m, fx, fy); }
  }
}
long double fir_si128f80(__int128);
__attribute__((noinline)) static long double c_si128f80(__int128 a, __int128 b) { (void)b; return (long double)a; }
static void t_si128f80(__int128 a, __int128 b) {
  
  for (int m = 0; m < 4; m++) {
    fesetround(modes[m]); feclearexcept(FE_ALL_EXCEPT);
    volatile long double x = c_si128f80(a, b); int fx = fetestexcept(FE_ALL_EXCEPT);
    feclearexcept(FE_ALL_EXCEPT);
    volatile long double y = fir_si128f80(a); int fy = fetestexcept(FE_ALL_EXCEPT);
    fesetround(FE_TONEAREST);
    long double xx = x, yy = y;
    if (memcmp(&xx, &yy, 10) != 0 && !(xx != xx && yy != yy)) { fails++; printf("FAIL value si128f80 mode %d\n", m); }
    if (fx != fy) { fails++; printf("FAIL flags si128f80 mode %d: c %#x fir %#x\n", m, fx, fy); }
  }
}
_Float16 fir_si64f16(long);
__attribute__((noinline)) static _Float16 c_si64f16(long a, long b) { (void)b; return (_Float16)a; }
static void t_si64f16(long a, long b) {
  
  for (int m = 0; m < 4; m++) {
    fesetround(modes[m]); feclearexcept(FE_ALL_EXCEPT);
    volatile _Float16 x = c_si64f16(a, b); int fx = fetestexcept(FE_ALL_EXCEPT);
    feclearexcept(FE_ALL_EXCEPT);
    volatile _Float16 y = fir_si64f16(a); int fy = fetestexcept(FE_ALL_EXCEPT);
    fesetround(FE_TONEAREST);
    _Float16 xx = x, yy = y;
    if (memcmp(&xx, &yy, sizeof xx) != 0 && !(xx != xx && yy != yy)) { fails++; printf("FAIL value si64f16 mode %d\n", m); }
    if (fx != fy) { fails++; printf("FAIL flags si64f16 mode %d: c %#x fir %#x\n", m, fx, fy); }
  }
}
long fir_fp64si(double);
__attribute__((noinline)) static long c_fp64si(double a, double b) { (void)b; return (long)a; }
static void t_fp64si(double a, double b) {
  
  for (int m = 0; m < 4; m++) {
    fesetround(modes[m]); feclearexcept(FE_ALL_EXCEPT);
    volatile long x = c_fp64si(a, b); int fx = fetestexcept(FE_ALL_EXCEPT);
    feclearexcept(FE_ALL_EXCEPT);
    volatile long y = fir_fp64si(a); int fy = fetestexcept(FE_ALL_EXCEPT);
    fesetround(FE_TONEAREST);
    long xx = x, yy = y;
    if (memcmp(&xx, &yy, sizeof xx) != 0 && !(xx != xx && yy != yy)) { fails++; printf("FAIL value fp64si mode %d\n", m); }
    if (fx != fy) { fails++; printf("FAIL flags fp64si mode %d: c %#x fir %#x\n", m, fx, fy); }
  }
}
unsigned long fir_fp64ui(double);
__attribute__((noinline)) static unsigned long c_fp64ui(double a, double b) { (void)b; return (unsigned long)a; }
static void t_fp64ui(double a, double b) {
  
  for (int m = 0; m < 4; m++) {
    fesetround(modes[m]); feclearexcept(FE_ALL_EXCEPT);
    volatile unsigned long x = c_fp64ui(a, b); int fx = fetestexcept(FE_ALL_EXCEPT);
    feclearexcept(FE_ALL_EXCEPT);
    volatile unsigned long y = fir_fp64ui(a); int fy = fetestexcept(FE_ALL_EXCEPT);
    fesetround(FE_TONEAREST);
    unsigned long xx = x, yy = y;
    if (memcmp(&xx, &yy, sizeof xx) != 0 && !(xx != xx && yy != yy)) { fails++; printf("FAIL value fp64ui mode %d\n", m); }
    if (fx != fy) { fails++; printf("FAIL flags fp64ui mode %d: c %#x fir %#x\n", m, fx, fy); }
  }
}
unsigned long fir_fp32ui(float);
__attribute__((noinline)) static unsigned long c_fp32ui(float a, float b) { (void)b; return (unsigned long)a; }
static void t_fp32ui(float a, float b) {
  
  for (int m = 0; m < 4; m++) {
    fesetround(modes[m]); feclearexcept(FE_ALL_EXCEPT);
    volatile unsigned long x = c_fp32ui(a, b); int fx = fetestexcept(FE_ALL_EXCEPT);
    feclearexcept(FE_ALL_EXCEPT);
    volatile unsigned long y = fir_fp32ui(a); int fy = fetestexcept(FE_ALL_EXCEPT);
    fesetround(FE_TONEAREST);
    unsigned long xx = x, yy = y;
    if (memcmp(&xx, &yy, sizeof xx) != 0 && !(xx != xx && yy != yy)) { fails++; printf("FAIL value fp32ui mode %d\n", m); }
    if (fx != fy) { fails++; printf("FAIL flags fp32ui mode %d: c %#x fir %#x\n", m, fx, fy); }
  }
}
unsigned long fir_fp80ui(long double);
__attribute__((noinline)) static unsigned long c_fp80ui(long double a, long double b) { (void)b; return (unsigned long)a; }
static void t_fp80ui(long double a, long double b) {
  
  for (int m = 0; m < 4; m++) {
    fesetround(modes[m]); feclearexcept(FE_ALL_EXCEPT);
    volatile unsigned long x = c_fp80ui(a, b); int fx = fetestexcept(FE_ALL_EXCEPT);
    feclearexcept(FE_ALL_EXCEPT);
    volatile unsigned long y = fir_fp80ui(a); int fy = fetestexcept(FE_ALL_EXCEPT);
    fesetround(FE_TONEAREST);
    unsigned long xx = x, yy = y;
    if (memcmp(&xx, &yy, sizeof xx) != 0 && !(xx != xx && yy != yy)) { fails++; printf("FAIL value fp80ui mode %d\n", m); }
    if (fx != fy) { fails++; printf("FAIL flags fp80ui mode %d: c %#x fir %#x\n", m, fx, fy); }
  }
}
double fir_si200f64(const void *);
__attribute__((noinline)) static double c_si200f64(__int128 a, __int128 b) { (void)b; return (double)(a>>64|((a&0xffffffffffffffff)!=0))*0x1p64; }
static void t_si200f64(__int128 a, __int128 b) {
  unsigned char buf[32]; to200(buf, (__int128)a);
  for (int m = 0; m < 4; m++) {
    fesetround(modes[m]); feclearexcept(FE_ALL_EXCEPT);
    volatile double x = c_si200f64(a, b); int fx = fetestexcept(FE_ALL_EXCEPT);
    feclearexcept(FE_ALL_EXCEPT);
    volatile double y = fir_si200f64(buf); int fy = fetestexcept(FE_ALL_EXCEPT);
    fesetround(FE_TONEAREST);
    double xx = x, yy = y;
    if (memcmp(&xx, &yy, sizeof xx) != 0 && !(xx != xx && yy != yy)) { fails++; printf("FAIL value si200f64 mode %d\n", m); }
    if (fx != fy) { fails++; printf("FAIL flags si200f64 mode %d: c %#x fir %#x\n", m, fx, fy); }
  }
}
float fir_ui200f32(const void *);
__attribute__((noinline)) static float c_ui200f32(unsigned __int128 a, unsigned __int128 b) { (void)b; return (float)(a>>64|((a&0xffffffffffffffff)!=0))*0x1p64f; }
static void t_ui200f32(unsigned __int128 a, unsigned __int128 b) {
  unsigned char buf[32]; to200(buf, (__int128)a);
  for (int m = 0; m < 4; m++) {
    fesetround(modes[m]); feclearexcept(FE_ALL_EXCEPT);
    volatile float x = c_ui200f32(a, b); int fx = fetestexcept(FE_ALL_EXCEPT);
    feclearexcept(FE_ALL_EXCEPT);
    volatile float y = fir_ui200f32(buf); int fy = fetestexcept(FE_ALL_EXCEPT);
    fesetround(FE_TONEAREST);
    float xx = x, yy = y;
    if (memcmp(&xx, &yy, sizeof xx) != 0 && !(xx != xx && yy != yy)) { fails++; printf("FAIL value ui200f32 mode %d\n", m); }
    if (fx != fy) { fails++; printf("FAIL flags ui200f32 mode %d: c %#x fir %#x\n", m, fx, fy); }
  }
}
unsigned __int128 fir_fp64ui128(double);
static void t_fp64ui128(double a) {
  int in = a > -1.0 && a < 0x1p128;
  for (int m = 0; m < 4; m++) {
    fesetround(modes[m]); feclearexcept(FE_ALL_EXCEPT);
    volatile unsigned __int128 y = fir_fp64ui128(a); int fy = fetestexcept(FE_ALL_EXCEPT);
    fesetround(FE_TONEAREST);
    if (!in) {
      if (fy != FE_INVALID) { fails++; printf("FAIL flags fp64ui128 %a mode %d: %#x, want invalid\n", a, m, fy); }
      continue;
    }
    double t = a < 0 ? 0.0 : a - fmod(a, 1.0);
    unsigned __int128 want;
    if (t >= 0x1p64) want = (unsigned __int128)(unsigned long)(t * 0x1p-64) << 64 | (unsigned long)fmod(t, 0x1p64);
    else want = (unsigned long)t;
    int allowed = t == a ? 0 : FE_INEXACT;
    if (y != want) { fails++; printf("FAIL value fp64ui128 %a mode %d\n", a, m); }
    if (fy & ~allowed) { fails++; printf("FAIL flags fp64ui128 %a mode %d: %#x\n", a, m, fy); }
  }
}

/* three-operand fma on random and edge operands, against the C library */
float fir_fma32(float, float, float);
double fir_fma64x(double, double, double);
long double fir_fma80x(long double, long double, long double);
static unsigned long long st = 88172645463325252ULL;
static unsigned long long rnd(void) { st ^= st << 13; st ^= st >> 7; st ^= st << 17; return st; }
static long double mk80(unsigned long long m, int e, int s) {
  long double x; unsigned char b[16] = {0}; unsigned short h = (e & 0x7fff) | (s << 15);
  memcpy(b, &m, 8); memcpy(b + 8, &h, 2); memcpy(&x, b, 16); return x;
}
/* random significand bits, a run of ones or a power of two plus a little */
static unsigned long long sig(void) {
  switch (rnd() % 4) { case 0: return ~0ULL << (rnd() % 64); case 1: return 1ULL << 63 | rnd() % 4; default: return rnd(); }
}
/* operands near the bottom, near the top, near 1, subnormal or anywhere */
static long double r80(void) {
  int e;
  switch (rnd() % 5) {
  case 0: e = 1 + rnd() % 40; break;
  case 1: e = 0x7ffe - rnd() % 40; break;
  case 2: e = 0x3fff + (int)(rnd() % 100) - 50; break;
  case 3: return mk80(sig() >> (1 + rnd() % 63), 0, rnd() & 1);
  default: e = 1 + rnd() % 0x7ffd;
  }
  return mk80(sig() | 1ULL << 63, e, rnd() & 1);
}
static double r64(void) {
  unsigned long long m = sig() >> 12, e;
  switch (rnd() % 5) {
  case 0: e = rnd() % 40; break;
  case 1: e = 0x7fe - rnd() % 40; break;
  case 2: e = 0x3ff + rnd() % 100 - 50; break;
  case 3: e = 0; break;
  default: e = rnd() % 0x7ff;
  }
  unsigned long long b = (rnd() & 1) << 63 | e << 52 | (m & 0xfffffffffffffULL);
  double x; memcpy(&x, &b, 8); return x;
}
static float r32(void) {
  unsigned int m = sig() >> 41, e;
  switch (rnd() % 5) {
  case 0: e = rnd() % 20; break;
  case 1: e = 0xfe - rnd() % 20; break;
  case 2: e = 0x7f + rnd() % 40 - 20; break;
  case 3: e = 0; break;
  default: e = rnd() % 0xff;
  }
  unsigned int b = (unsigned)(rnd() & 1) << 31 | e << 23 | (m & 0x7fffff);
  float x; memcpy(&x, &b, 4); return x;
}
#define CHECK(T, n, cf, ff, x, y, z) do { \
  for (int m = 0; m < 4; m++) { \
    fesetround(modes[m]); feclearexcept(FE_ALL_EXCEPT); \
    volatile T a = cf(x, y, z); int fa = fetestexcept(FE_ALL_EXCEPT); \
    feclearexcept(FE_ALL_EXCEPT); \
    volatile T b = ff(x, y, z); int fb = fetestexcept(FE_ALL_EXCEPT); \
    fesetround(FE_TONEAREST); \
    T aa = a, bb = b; \
    if ((memcmp(&aa, &bb, n) != 0 && !(aa != aa && bb != bb)) || fa != fb) { \
      if (fails++ < 20) printf("FAIL " #ff " mode %d: %La %La %La: c %La %#x fir %La %#x\n", m, \
        (long double)x, (long double)y, (long double)z, (long double)aa, fa, (long double)bb, fb); } \
  } } while (0)
__attribute__((noinline)) static float c_fmaf(float x, float y, float z) { return fmaf(x, y, z); }
__attribute__((noinline)) static double c_fma(double x, double y, double z) { return fma(x, y, z); }
__attribute__((noinline)) static long double c_fmal(long double x, long double y, long double z) { return fmal(x, y, z); }
static void t_fma3(void) {
  for (int i = 0; i < 20000; i++) {
    long double x = r80(), y = r80(), z;
    switch (rnd() % 4) {
    case 0: { volatile long double p = x * y; z = -p; break; }
    case 1: z = mk80(rnd() | 1ULL << 63, 1 + rnd() % 3, rnd() & 1); break;
    default: z = r80();
    }
    CHECK(long double, 10, c_fmal, fir_fma80x, x, y, z);
    /* just below 2^-16382: tiny or not depending on rounding at 64 bits */
    x = mk80(~0ULL - rnd() % 4, 0x3fff - rnd() % 3, rnd() & 1);
    y = mk80(1ULL << 63 | rnd() % 8, 1 + rnd() % 3, rnd() & 1);
    z = mk80(rnd() % 16, 0, rnd() & 1);
    CHECK(long double, 10, c_fmal, fir_fma80x, x, y, z);
    double dx = r64(), dy = r64(), dz;
    if (rnd() % 4 == 0) { volatile double p = dx * dy; dz = -p; } else dz = r64();
    CHECK(double, 8, c_fma, fir_fma64x, dx, dy, dz);
    float fx = r32(), fy = r32(), fz;
    if (rnd() % 4 == 0) { volatile float p = fx * fy; fz = -p; } else fz = r32();
    CHECK(float, 4, c_fmaf, fir_fma32, fx, fy, fz);
  }
}
int main(void) {
  t_div32(1.0f, 1.0f);
  t_div32(1.0f, 3.0f);
  t_div32(1.0f, 0.1f);
  t_div32(1.0f, -7.0f);
  t_div32(3.0f, 1.0f);
  t_div32(3.0f, 3.0f);
  t_div32(3.0f, 0.1f);
  t_div32(3.0f, -7.0f);
  t_div32(0.1f, 1.0f);
  t_div32(0.1f, 3.0f);
  t_div32(0.1f, 0.1f);
  t_div32(0.1f, -7.0f);
  t_div32(-7.0f, 1.0f);
  t_div32(-7.0f, 3.0f);
  t_div32(-7.0f, 0.1f);
  t_div32(-7.0f, -7.0f);
  t_div32(1e30f, 1.0f);
  t_div32(1e30f, 3.0f);
  t_div32(1e30f, 0.1f);
  t_div32(1e30f, -7.0f);
  t_div32(0.0f, 1.0f);
  t_div32(0.0f, 3.0f);
  t_div32(0.0f, 0.1f);
  t_div32(0.0f, -7.0f);
  t_div64(1.0, 1.0);
  t_div64(1.0, 3.0);
  t_div64(1.0, 0.1);
  t_div64(1.0, -7.0);
  t_div64(3.0, 1.0);
  t_div64(3.0, 3.0);
  t_div64(3.0, 0.1);
  t_div64(3.0, -7.0);
  t_div64(0.1, 1.0);
  t_div64(0.1, 3.0);
  t_div64(0.1, 0.1);
  t_div64(0.1, -7.0);
  t_div64(-7.0, 1.0);
  t_div64(-7.0, 3.0);
  t_div64(-7.0, 0.1);
  t_div64(-7.0, -7.0);
  t_div64(1e300, 1.0);
  t_div64(1e300, 3.0);
  t_div64(1e300, 0.1);
  t_div64(1e300, -7.0);
  t_div64(0.0, 1.0);
  t_div64(0.0, 3.0);
  t_div64(0.0, 0.1);
  t_div64(0.0, -7.0);
  t_div64(0x1.fffffffffffffp+60, 1.0);
  t_div64(0x1.fffffffffffffp+60, 3.0);
  t_div64(0x1.fffffffffffffp+60, 0.1);
  t_div64(0x1.fffffffffffffp+60, -7.0);
  t_div64(-0x1.8p+70, 1.0);
  t_div64(-0x1.8p+70, 3.0);
  t_div64(-0x1.8p+70, 0.1);
  t_div64(-0x1.8p+70, -7.0);
  t_div64(0x1.8p+100, 1.0);
  t_div64(0x1.8p+100, 3.0);
  t_div64(0x1.8p+100, 0.1);
  t_div64(0x1.8p+100, -7.0);
  t_div64(0x1.0000000000001p+65, 1.0);
  t_div64(0x1.0000000000001p+65, 3.0);
  t_div64(0x1.0000000000001p+65, 0.1);
  t_div64(0x1.0000000000001p+65, -7.0);
  t_div64(-0.5, 1.0);
  t_div64(-0.5, 3.0);
  t_div64(-0.5, 0.1);
  t_div64(-0.5, -7.0);
  t_div80(1.0, 1.0);
  t_div80(1.0, 3.0);
  t_div80(1.0, 0.1);
  t_div80(1.0, -7.0);
  t_div80(3.0, 1.0);
  t_div80(3.0, 3.0);
  t_div80(3.0, 0.1);
  t_div80(3.0, -7.0);
  t_div80(0.1, 1.0);
  t_div80(0.1, 3.0);
  t_div80(0.1, 0.1);
  t_div80(0.1, -7.0);
  t_div80(-7.0, 1.0);
  t_div80(-7.0, 3.0);
  t_div80(-7.0, 0.1);
  t_div80(-7.0, -7.0);
  t_div80(1e300, 1.0);
  t_div80(1e300, 3.0);
  t_div80(1e300, 0.1);
  t_div80(1e300, -7.0);
  t_div80(0.0, 1.0);
  t_div80(0.0, 3.0);
  t_div80(0.0, 0.1);
  t_div80(0.0, -7.0);
  t_div80(0x1.fffffffffffffp+60, 1.0);
  t_div80(0x1.fffffffffffffp+60, 3.0);
  t_div80(0x1.fffffffffffffp+60, 0.1);
  t_div80(0x1.fffffffffffffp+60, -7.0);
  t_div80(-0x1.8p+70, 1.0);
  t_div80(-0x1.8p+70, 3.0);
  t_div80(-0x1.8p+70, 0.1);
  t_div80(-0x1.8p+70, -7.0);
  t_div80(0x1.8p+100, 1.0);
  t_div80(0x1.8p+100, 3.0);
  t_div80(0x1.8p+100, 0.1);
  t_div80(0x1.8p+100, -7.0);
  t_div80(0x1.0000000000001p+65, 1.0);
  t_div80(0x1.0000000000001p+65, 3.0);
  t_div80(0x1.0000000000001p+65, 0.1);
  t_div80(0x1.0000000000001p+65, -7.0);
  t_div80(-0.5, 1.0);
  t_div80(-0.5, 3.0);
  t_div80(-0.5, 0.1);
  t_div80(-0.5, -7.0);
  t_add64(1.0, 1.0);
  t_add64(1.0, 3.0);
  t_add64(1.0, 0.1);
  t_add64(1.0, -7.0);
  t_add64(3.0, 1.0);
  t_add64(3.0, 3.0);
  t_add64(3.0, 0.1);
  t_add64(3.0, -7.0);
  t_add64(0.1, 1.0);
  t_add64(0.1, 3.0);
  t_add64(0.1, 0.1);
  t_add64(0.1, -7.0);
  t_add64(-7.0, 1.0);
  t_add64(-7.0, 3.0);
  t_add64(-7.0, 0.1);
  t_add64(-7.0, -7.0);
  t_add64(1e300, 1.0);
  t_add64(1e300, 3.0);
  t_add64(1e300, 0.1);
  t_add64(1e300, -7.0);
  t_add64(0.0, 1.0);
  t_add64(0.0, 3.0);
  t_add64(0.0, 0.1);
  t_add64(0.0, -7.0);
  t_add64(0x1.fffffffffffffp+60, 1.0);
  t_add64(0x1.fffffffffffffp+60, 3.0);
  t_add64(0x1.fffffffffffffp+60, 0.1);
  t_add64(0x1.fffffffffffffp+60, -7.0);
  t_add64(-0x1.8p+70, 1.0);
  t_add64(-0x1.8p+70, 3.0);
  t_add64(-0x1.8p+70, 0.1);
  t_add64(-0x1.8p+70, -7.0);
  t_add64(0x1.8p+100, 1.0);
  t_add64(0x1.8p+100, 3.0);
  t_add64(0x1.8p+100, 0.1);
  t_add64(0x1.8p+100, -7.0);
  t_add64(0x1.0000000000001p+65, 1.0);
  t_add64(0x1.0000000000001p+65, 3.0);
  t_add64(0x1.0000000000001p+65, 0.1);
  t_add64(0x1.0000000000001p+65, -7.0);
  t_add64(-0.5, 1.0);
  t_add64(-0.5, 3.0);
  t_add64(-0.5, 0.1);
  t_add64(-0.5, -7.0);
  t_mul80(1.0, 1.0);
  t_mul80(1.0, 3.0);
  t_mul80(1.0, 0.1);
  t_mul80(1.0, -7.0);
  t_mul80(3.0, 1.0);
  t_mul80(3.0, 3.0);
  t_mul80(3.0, 0.1);
  t_mul80(3.0, -7.0);
  t_mul80(0.1, 1.0);
  t_mul80(0.1, 3.0);
  t_mul80(0.1, 0.1);
  t_mul80(0.1, -7.0);
  t_mul80(-7.0, 1.0);
  t_mul80(-7.0, 3.0);
  t_mul80(-7.0, 0.1);
  t_mul80(-7.0, -7.0);
  t_mul80(1e300, 1.0);
  t_mul80(1e300, 3.0);
  t_mul80(1e300, 0.1);
  t_mul80(1e300, -7.0);
  t_mul80(0.0, 1.0);
  t_mul80(0.0, 3.0);
  t_mul80(0.0, 0.1);
  t_mul80(0.0, -7.0);
  t_mul80(0x1.fffffffffffffp+60, 1.0);
  t_mul80(0x1.fffffffffffffp+60, 3.0);
  t_mul80(0x1.fffffffffffffp+60, 0.1);
  t_mul80(0x1.fffffffffffffp+60, -7.0);
  t_mul80(-0x1.8p+70, 1.0);
  t_mul80(-0x1.8p+70, 3.0);
  t_mul80(-0x1.8p+70, 0.1);
  t_mul80(-0x1.8p+70, -7.0);
  t_mul80(0x1.8p+100, 1.0);
  t_mul80(0x1.8p+100, 3.0);
  t_mul80(0x1.8p+100, 0.1);
  t_mul80(0x1.8p+100, -7.0);
  t_mul80(0x1.0000000000001p+65, 1.0);
  t_mul80(0x1.0000000000001p+65, 3.0);
  t_mul80(0x1.0000000000001p+65, 0.1);
  t_mul80(0x1.0000000000001p+65, -7.0);
  t_mul80(-0.5, 1.0);
  t_mul80(-0.5, 3.0);
  t_mul80(-0.5, 0.1);
  t_mul80(-0.5, -7.0);
  t_div16((_Float16)1.0, (_Float16)1.0);
  t_div16((_Float16)1.0, (_Float16)3.0);
  t_div16((_Float16)1.0, (_Float16)0.1);
  t_div16((_Float16)1.0, (_Float16)-7.0);
  t_div16((_Float16)3.0, (_Float16)1.0);
  t_div16((_Float16)3.0, (_Float16)3.0);
  t_div16((_Float16)3.0, (_Float16)0.1);
  t_div16((_Float16)3.0, (_Float16)-7.0);
  t_div16((_Float16)0.1, (_Float16)1.0);
  t_div16((_Float16)0.1, (_Float16)3.0);
  t_div16((_Float16)0.1, (_Float16)0.1);
  t_div16((_Float16)0.1, (_Float16)-7.0);
  t_div16((_Float16)-7.0, (_Float16)1.0);
  t_div16((_Float16)-7.0, (_Float16)3.0);
  t_div16((_Float16)-7.0, (_Float16)0.1);
  t_div16((_Float16)-7.0, (_Float16)-7.0);
  t_add16((_Float16)1.0, (_Float16)1.0);
  t_add16((_Float16)1.0, (_Float16)3.0);
  t_add16((_Float16)1.0, (_Float16)0.1);
  t_add16((_Float16)1.0, (_Float16)-7.0);
  t_add16((_Float16)3.0, (_Float16)1.0);
  t_add16((_Float16)3.0, (_Float16)3.0);
  t_add16((_Float16)3.0, (_Float16)0.1);
  t_add16((_Float16)3.0, (_Float16)-7.0);
  t_add16((_Float16)0.1, (_Float16)1.0);
  t_add16((_Float16)0.1, (_Float16)3.0);
  t_add16((_Float16)0.1, (_Float16)0.1);
  t_add16((_Float16)0.1, (_Float16)-7.0);
  t_add16((_Float16)-7.0, (_Float16)1.0);
  t_add16((_Float16)-7.0, (_Float16)3.0);
  t_add16((_Float16)-7.0, (_Float16)0.1);
  t_add16((_Float16)-7.0, (_Float16)-7.0);
  t_sqrt64(1.0, 1.0);
  t_sqrt64(1.0, 3.0);
  t_sqrt64(1.0, 0.1);
  t_sqrt64(1.0, -7.0);
  t_sqrt64(3.0, 1.0);
  t_sqrt64(3.0, 3.0);
  t_sqrt64(3.0, 0.1);
  t_sqrt64(3.0, -7.0);
  t_sqrt64(0.1, 1.0);
  t_sqrt64(0.1, 3.0);
  t_sqrt64(0.1, 0.1);
  t_sqrt64(0.1, -7.0);
  t_sqrt64(-7.0, 1.0);
  t_sqrt64(-7.0, 3.0);
  t_sqrt64(-7.0, 0.1);
  t_sqrt64(-7.0, -7.0);
  t_sqrt64(1e300, 1.0);
  t_sqrt64(1e300, 3.0);
  t_sqrt64(1e300, 0.1);
  t_sqrt64(1e300, -7.0);
  t_sqrt64(0.0, 1.0);
  t_sqrt64(0.0, 3.0);
  t_sqrt64(0.0, 0.1);
  t_sqrt64(0.0, -7.0);
  t_sqrt64(0x1.fffffffffffffp+60, 1.0);
  t_sqrt64(0x1.fffffffffffffp+60, 3.0);
  t_sqrt64(0x1.fffffffffffffp+60, 0.1);
  t_sqrt64(0x1.fffffffffffffp+60, -7.0);
  t_sqrt64(-0x1.8p+70, 1.0);
  t_sqrt64(-0x1.8p+70, 3.0);
  t_sqrt64(-0x1.8p+70, 0.1);
  t_sqrt64(-0x1.8p+70, -7.0);
  t_sqrt64(0x1.8p+100, 1.0);
  t_sqrt64(0x1.8p+100, 3.0);
  t_sqrt64(0x1.8p+100, 0.1);
  t_sqrt64(0x1.8p+100, -7.0);
  t_sqrt64(0x1.0000000000001p+65, 1.0);
  t_sqrt64(0x1.0000000000001p+65, 3.0);
  t_sqrt64(0x1.0000000000001p+65, 0.1);
  t_sqrt64(0x1.0000000000001p+65, -7.0);
  t_sqrt64(-0.5, 1.0);
  t_sqrt64(-0.5, 3.0);
  t_sqrt64(-0.5, 0.1);
  t_sqrt64(-0.5, -7.0);
  t_sqrt80(1.0, 1.0);
  t_sqrt80(1.0, 3.0);
  t_sqrt80(1.0, 0.1);
  t_sqrt80(1.0, -7.0);
  t_sqrt80(3.0, 1.0);
  t_sqrt80(3.0, 3.0);
  t_sqrt80(3.0, 0.1);
  t_sqrt80(3.0, -7.0);
  t_sqrt80(0.1, 1.0);
  t_sqrt80(0.1, 3.0);
  t_sqrt80(0.1, 0.1);
  t_sqrt80(0.1, -7.0);
  t_sqrt80(-7.0, 1.0);
  t_sqrt80(-7.0, 3.0);
  t_sqrt80(-7.0, 0.1);
  t_sqrt80(-7.0, -7.0);
  t_sqrt80(1e300, 1.0);
  t_sqrt80(1e300, 3.0);
  t_sqrt80(1e300, 0.1);
  t_sqrt80(1e300, -7.0);
  t_sqrt80(0.0, 1.0);
  t_sqrt80(0.0, 3.0);
  t_sqrt80(0.0, 0.1);
  t_sqrt80(0.0, -7.0);
  t_sqrt80(0x1.fffffffffffffp+60, 1.0);
  t_sqrt80(0x1.fffffffffffffp+60, 3.0);
  t_sqrt80(0x1.fffffffffffffp+60, 0.1);
  t_sqrt80(0x1.fffffffffffffp+60, -7.0);
  t_sqrt80(-0x1.8p+70, 1.0);
  t_sqrt80(-0x1.8p+70, 3.0);
  t_sqrt80(-0x1.8p+70, 0.1);
  t_sqrt80(-0x1.8p+70, -7.0);
  t_sqrt80(0x1.8p+100, 1.0);
  t_sqrt80(0x1.8p+100, 3.0);
  t_sqrt80(0x1.8p+100, 0.1);
  t_sqrt80(0x1.8p+100, -7.0);
  t_sqrt80(0x1.0000000000001p+65, 1.0);
  t_sqrt80(0x1.0000000000001p+65, 3.0);
  t_sqrt80(0x1.0000000000001p+65, 0.1);
  t_sqrt80(0x1.0000000000001p+65, -7.0);
  t_sqrt80(-0.5, 1.0);
  t_sqrt80(-0.5, 3.0);
  t_sqrt80(-0.5, 0.1);
  t_sqrt80(-0.5, -7.0);
  t_fma64(1.0, 1.0);
  t_fma64(1.0, 3.0);
  t_fma64(1.0, 0.1);
  t_fma64(1.0, -7.0);
  t_fma64(3.0, 1.0);
  t_fma64(3.0, 3.0);
  t_fma64(3.0, 0.1);
  t_fma64(3.0, -7.0);
  t_fma64(0.1, 1.0);
  t_fma64(0.1, 3.0);
  t_fma64(0.1, 0.1);
  t_fma64(0.1, -7.0);
  t_fma64(-7.0, 1.0);
  t_fma64(-7.0, 3.0);
  t_fma64(-7.0, 0.1);
  t_fma64(-7.0, -7.0);
  t_fma64(1e300, 1.0);
  t_fma64(1e300, 3.0);
  t_fma64(1e300, 0.1);
  t_fma64(1e300, -7.0);
  t_fma64(0.0, 1.0);
  t_fma64(0.0, 3.0);
  t_fma64(0.0, 0.1);
  t_fma64(0.0, -7.0);
  t_fma64(0x1.fffffffffffffp+60, 1.0);
  t_fma64(0x1.fffffffffffffp+60, 3.0);
  t_fma64(0x1.fffffffffffffp+60, 0.1);
  t_fma64(0x1.fffffffffffffp+60, -7.0);
  t_fma64(-0x1.8p+70, 1.0);
  t_fma64(-0x1.8p+70, 3.0);
  t_fma64(-0x1.8p+70, 0.1);
  t_fma64(-0x1.8p+70, -7.0);
  t_fma64(0x1.8p+100, 1.0);
  t_fma64(0x1.8p+100, 3.0);
  t_fma64(0x1.8p+100, 0.1);
  t_fma64(0x1.8p+100, -7.0);
  t_fma64(0x1.0000000000001p+65, 1.0);
  t_fma64(0x1.0000000000001p+65, 3.0);
  t_fma64(0x1.0000000000001p+65, 0.1);
  t_fma64(0x1.0000000000001p+65, -7.0);
  t_fma64(-0.5, 1.0);
  t_fma64(-0.5, 3.0);
  t_fma64(-0.5, 0.1);
  t_fma64(-0.5, -7.0);
  t_fma80(1.0, 1.0);
  t_fma80(1.0, 3.0);
  t_fma80(1.0, 0.1);
  t_fma80(1.0, -7.0);
  t_fma80(3.0, 1.0);
  t_fma80(3.0, 3.0);
  t_fma80(3.0, 0.1);
  t_fma80(3.0, -7.0);
  t_fma80(0.1, 1.0);
  t_fma80(0.1, 3.0);
  t_fma80(0.1, 0.1);
  t_fma80(0.1, -7.0);
  t_fma80(-7.0, 1.0);
  t_fma80(-7.0, 3.0);
  t_fma80(-7.0, 0.1);
  t_fma80(-7.0, -7.0);
  t_fma80(1e300, 1.0);
  t_fma80(1e300, 3.0);
  t_fma80(1e300, 0.1);
  t_fma80(1e300, -7.0);
  t_fma80(0.0, 1.0);
  t_fma80(0.0, 3.0);
  t_fma80(0.0, 0.1);
  t_fma80(0.0, -7.0);
  t_fma80(0x1.fffffffffffffp+60, 1.0);
  t_fma80(0x1.fffffffffffffp+60, 3.0);
  t_fma80(0x1.fffffffffffffp+60, 0.1);
  t_fma80(0x1.fffffffffffffp+60, -7.0);
  t_fma80(-0x1.8p+70, 1.0);
  t_fma80(-0x1.8p+70, 3.0);
  t_fma80(-0x1.8p+70, 0.1);
  t_fma80(-0x1.8p+70, -7.0);
  t_fma80(0x1.8p+100, 1.0);
  t_fma80(0x1.8p+100, 3.0);
  t_fma80(0x1.8p+100, 0.1);
  t_fma80(0x1.8p+100, -7.0);
  t_fma80(0x1.0000000000001p+65, 1.0);
  t_fma80(0x1.0000000000001p+65, 3.0);
  t_fma80(0x1.0000000000001p+65, 0.1);
  t_fma80(0x1.0000000000001p+65, -7.0);
  t_fma80(-0.5, 1.0);
  t_fma80(-0.5, 3.0);
  t_fma80(-0.5, 0.1);
  t_fma80(-0.5, -7.0);
  t_fmah((_Float16)1.0, (_Float16)1.0);
  t_fmah((_Float16)1.0, (_Float16)3.0);
  t_fmah((_Float16)1.0, (_Float16)0.1);
  t_fmah((_Float16)1.0, (_Float16)-7.0);
  t_fmah((_Float16)3.0, (_Float16)1.0);
  t_fmah((_Float16)3.0, (_Float16)3.0);
  t_fmah((_Float16)3.0, (_Float16)0.1);
  t_fmah((_Float16)3.0, (_Float16)-7.0);
  t_fmah((_Float16)0.1, (_Float16)1.0);
  t_fmah((_Float16)0.1, (_Float16)3.0);
  t_fmah((_Float16)0.1, (_Float16)0.1);
  t_fmah((_Float16)0.1, (_Float16)-7.0);
  t_fmah((_Float16)-7.0, (_Float16)1.0);
  t_fmah((_Float16)-7.0, (_Float16)3.0);
  t_fmah((_Float16)-7.0, (_Float16)0.1);
  t_fmah((_Float16)-7.0, (_Float16)-7.0);
  t_tr6432(1.0, 1.0);
  t_tr6432(1.0, 3.0);
  t_tr6432(1.0, 0.1);
  t_tr6432(1.0, -7.0);
  t_tr6432(3.0, 1.0);
  t_tr6432(3.0, 3.0);
  t_tr6432(3.0, 0.1);
  t_tr6432(3.0, -7.0);
  t_tr6432(0.1, 1.0);
  t_tr6432(0.1, 3.0);
  t_tr6432(0.1, 0.1);
  t_tr6432(0.1, -7.0);
  t_tr6432(-7.0, 1.0);
  t_tr6432(-7.0, 3.0);
  t_tr6432(-7.0, 0.1);
  t_tr6432(-7.0, -7.0);
  t_tr6432(1e300, 1.0);
  t_tr6432(1e300, 3.0);
  t_tr6432(1e300, 0.1);
  t_tr6432(1e300, -7.0);
  t_tr6432(0.0, 1.0);
  t_tr6432(0.0, 3.0);
  t_tr6432(0.0, 0.1);
  t_tr6432(0.0, -7.0);
  t_tr6432(0x1.fffffffffffffp+60, 1.0);
  t_tr6432(0x1.fffffffffffffp+60, 3.0);
  t_tr6432(0x1.fffffffffffffp+60, 0.1);
  t_tr6432(0x1.fffffffffffffp+60, -7.0);
  t_tr6432(-0x1.8p+70, 1.0);
  t_tr6432(-0x1.8p+70, 3.0);
  t_tr6432(-0x1.8p+70, 0.1);
  t_tr6432(-0x1.8p+70, -7.0);
  t_tr6432(0x1.8p+100, 1.0);
  t_tr6432(0x1.8p+100, 3.0);
  t_tr6432(0x1.8p+100, 0.1);
  t_tr6432(0x1.8p+100, -7.0);
  t_tr6432(0x1.0000000000001p+65, 1.0);
  t_tr6432(0x1.0000000000001p+65, 3.0);
  t_tr6432(0x1.0000000000001p+65, 0.1);
  t_tr6432(0x1.0000000000001p+65, -7.0);
  t_tr6432(-0.5, 1.0);
  t_tr6432(-0.5, 3.0);
  t_tr6432(-0.5, 0.1);
  t_tr6432(-0.5, -7.0);
  t_tr8064(1.0, 1.0);
  t_tr8064(1.0, 3.0);
  t_tr8064(1.0, 0.1);
  t_tr8064(1.0, -7.0);
  t_tr8064(3.0, 1.0);
  t_tr8064(3.0, 3.0);
  t_tr8064(3.0, 0.1);
  t_tr8064(3.0, -7.0);
  t_tr8064(0.1, 1.0);
  t_tr8064(0.1, 3.0);
  t_tr8064(0.1, 0.1);
  t_tr8064(0.1, -7.0);
  t_tr8064(-7.0, 1.0);
  t_tr8064(-7.0, 3.0);
  t_tr8064(-7.0, 0.1);
  t_tr8064(-7.0, -7.0);
  t_tr8064(1e300, 1.0);
  t_tr8064(1e300, 3.0);
  t_tr8064(1e300, 0.1);
  t_tr8064(1e300, -7.0);
  t_tr8064(0.0, 1.0);
  t_tr8064(0.0, 3.0);
  t_tr8064(0.0, 0.1);
  t_tr8064(0.0, -7.0);
  t_tr8064(0x1.fffffffffffffp+60, 1.0);
  t_tr8064(0x1.fffffffffffffp+60, 3.0);
  t_tr8064(0x1.fffffffffffffp+60, 0.1);
  t_tr8064(0x1.fffffffffffffp+60, -7.0);
  t_tr8064(-0x1.8p+70, 1.0);
  t_tr8064(-0x1.8p+70, 3.0);
  t_tr8064(-0x1.8p+70, 0.1);
  t_tr8064(-0x1.8p+70, -7.0);
  t_tr8064(0x1.8p+100, 1.0);
  t_tr8064(0x1.8p+100, 3.0);
  t_tr8064(0x1.8p+100, 0.1);
  t_tr8064(0x1.8p+100, -7.0);
  t_tr8064(0x1.0000000000001p+65, 1.0);
  t_tr8064(0x1.0000000000001p+65, 3.0);
  t_tr8064(0x1.0000000000001p+65, 0.1);
  t_tr8064(0x1.0000000000001p+65, -7.0);
  t_tr8064(-0.5, 1.0);
  t_tr8064(-0.5, 3.0);
  t_tr8064(-0.5, 0.1);
  t_tr8064(-0.5, -7.0);
  t_tr6416(1.0, 1.0);
  t_tr6416(1.0, 3.0);
  t_tr6416(1.0, 0.1);
  t_tr6416(1.0, -7.0);
  t_tr6416(3.0, 1.0);
  t_tr6416(3.0, 3.0);
  t_tr6416(3.0, 0.1);
  t_tr6416(3.0, -7.0);
  t_tr6416(0.1, 1.0);
  t_tr6416(0.1, 3.0);
  t_tr6416(0.1, 0.1);
  t_tr6416(0.1, -7.0);
  t_tr6416(-7.0, 1.0);
  t_tr6416(-7.0, 3.0);
  t_tr6416(-7.0, 0.1);
  t_tr6416(-7.0, -7.0);
  t_tr6416(1e300, 1.0);
  t_tr6416(1e300, 3.0);
  t_tr6416(1e300, 0.1);
  t_tr6416(1e300, -7.0);
  t_tr6416(0.0, 1.0);
  t_tr6416(0.0, 3.0);
  t_tr6416(0.0, 0.1);
  t_tr6416(0.0, -7.0);
  t_tr6416(0x1.fffffffffffffp+60, 1.0);
  t_tr6416(0x1.fffffffffffffp+60, 3.0);
  t_tr6416(0x1.fffffffffffffp+60, 0.1);
  t_tr6416(0x1.fffffffffffffp+60, -7.0);
  t_tr6416(-0x1.8p+70, 1.0);
  t_tr6416(-0x1.8p+70, 3.0);
  t_tr6416(-0x1.8p+70, 0.1);
  t_tr6416(-0x1.8p+70, -7.0);
  t_tr6416(0x1.8p+100, 1.0);
  t_tr6416(0x1.8p+100, 3.0);
  t_tr6416(0x1.8p+100, 0.1);
  t_tr6416(0x1.8p+100, -7.0);
  t_tr6416(0x1.0000000000001p+65, 1.0);
  t_tr6416(0x1.0000000000001p+65, 3.0);
  t_tr6416(0x1.0000000000001p+65, 0.1);
  t_tr6416(0x1.0000000000001p+65, -7.0);
  t_tr6416(-0.5, 1.0);
  t_tr6416(-0.5, 3.0);
  t_tr6416(-0.5, 0.1);
  t_tr6416(-0.5, -7.0);
  t_tr3216(1.0f, 1.0f);
  t_tr3216(1.0f, 3.0f);
  t_tr3216(1.0f, 0.1f);
  t_tr3216(1.0f, -7.0f);
  t_tr3216(3.0f, 1.0f);
  t_tr3216(3.0f, 3.0f);
  t_tr3216(3.0f, 0.1f);
  t_tr3216(3.0f, -7.0f);
  t_tr3216(0.1f, 1.0f);
  t_tr3216(0.1f, 3.0f);
  t_tr3216(0.1f, 0.1f);
  t_tr3216(0.1f, -7.0f);
  t_tr3216(-7.0f, 1.0f);
  t_tr3216(-7.0f, 3.0f);
  t_tr3216(-7.0f, 0.1f);
  t_tr3216(-7.0f, -7.0f);
  t_tr3216(1e30f, 1.0f);
  t_tr3216(1e30f, 3.0f);
  t_tr3216(1e30f, 0.1f);
  t_tr3216(1e30f, -7.0f);
  t_tr3216(0.0f, 1.0f);
  t_tr3216(0.0f, 3.0f);
  t_tr3216(0.0f, 0.1f);
  t_tr3216(0.0f, -7.0f);
  t_si64f32(1, 1);
  t_si64f32(1, 3);
  t_si64f32(1, -7);
  t_si64f32(1, 0x7fffffffffffffffL);
  t_si64f32(3, 1);
  t_si64f32(3, 3);
  t_si64f32(3, -7);
  t_si64f32(3, 0x7fffffffffffffffL);
  t_si64f32(-7, 1);
  t_si64f32(-7, 3);
  t_si64f32(-7, -7);
  t_si64f32(-7, 0x7fffffffffffffffL);
  t_si64f32(0x7fffffffffffffffL, 1);
  t_si64f32(0x7fffffffffffffffL, 3);
  t_si64f32(0x7fffffffffffffffL, -7);
  t_si64f32(0x7fffffffffffffffL, 0x7fffffffffffffffL);
  t_si64f32(123456789123456789L, 1);
  t_si64f32(123456789123456789L, 3);
  t_si64f32(123456789123456789L, -7);
  t_si64f32(123456789123456789L, 0x7fffffffffffffffL);
  t_si64f64(1, 1);
  t_si64f64(1, 3);
  t_si64f64(1, -7);
  t_si64f64(1, 0x7fffffffffffffffL);
  t_si64f64(3, 1);
  t_si64f64(3, 3);
  t_si64f64(3, -7);
  t_si64f64(3, 0x7fffffffffffffffL);
  t_si64f64(-7, 1);
  t_si64f64(-7, 3);
  t_si64f64(-7, -7);
  t_si64f64(-7, 0x7fffffffffffffffL);
  t_si64f64(0x7fffffffffffffffL, 1);
  t_si64f64(0x7fffffffffffffffL, 3);
  t_si64f64(0x7fffffffffffffffL, -7);
  t_si64f64(0x7fffffffffffffffL, 0x7fffffffffffffffL);
  t_si64f64(123456789123456789L, 1);
  t_si64f64(123456789123456789L, 3);
  t_si64f64(123456789123456789L, -7);
  t_si64f64(123456789123456789L, 0x7fffffffffffffffL);
  t_ui64f64(1, 1);
  t_ui64f64(1, 3);
  t_ui64f64(1, -7);
  t_ui64f64(1, 0x7fffffffffffffffL);
  t_ui64f64(3, 1);
  t_ui64f64(3, 3);
  t_ui64f64(3, -7);
  t_ui64f64(3, 0x7fffffffffffffffL);
  t_ui64f64(-7, 1);
  t_ui64f64(-7, 3);
  t_ui64f64(-7, -7);
  t_ui64f64(-7, 0x7fffffffffffffffL);
  t_ui64f64(0x7fffffffffffffffL, 1);
  t_ui64f64(0x7fffffffffffffffL, 3);
  t_ui64f64(0x7fffffffffffffffL, -7);
  t_ui64f64(0x7fffffffffffffffL, 0x7fffffffffffffffL);
  t_ui64f64(123456789123456789L, 1);
  t_ui64f64(123456789123456789L, 3);
  t_ui64f64(123456789123456789L, -7);
  t_ui64f64(123456789123456789L, 0x7fffffffffffffffL);
  t_ui64f32(1, 1);
  t_ui64f32(1, 3);
  t_ui64f32(1, -7);
  t_ui64f32(1, 0x7fffffffffffffffL);
  t_ui64f32(3, 1);
  t_ui64f32(3, 3);
  t_ui64f32(3, -7);
  t_ui64f32(3, 0x7fffffffffffffffL);
  t_ui64f32(-7, 1);
  t_ui64f32(-7, 3);
  t_ui64f32(-7, -7);
  t_ui64f32(-7, 0x7fffffffffffffffL);
  t_ui64f32(0x7fffffffffffffffL, 1);
  t_ui64f32(0x7fffffffffffffffL, 3);
  t_ui64f32(0x7fffffffffffffffL, -7);
  t_ui64f32(0x7fffffffffffffffL, 0x7fffffffffffffffL);
  t_ui64f32(123456789123456789L, 1);
  t_ui64f32(123456789123456789L, 3);
  t_ui64f32(123456789123456789L, -7);
  t_ui64f32(123456789123456789L, 0x7fffffffffffffffL);
  t_si128f64(1, 1);
  t_si128f64(1, 3);
  t_si128f64(1, -7);
  t_si128f64(1, 0x7fffffffffffffffL);
  t_si128f64(3, 1);
  t_si128f64(3, 3);
  t_si128f64(3, -7);
  t_si128f64(3, 0x7fffffffffffffffL);
  t_si128f64(-7, 1);
  t_si128f64(-7, 3);
  t_si128f64(-7, -7);
  t_si128f64(-7, 0x7fffffffffffffffL);
  t_si128f64(0x7fffffffffffffffL, 1);
  t_si128f64(0x7fffffffffffffffL, 3);
  t_si128f64(0x7fffffffffffffffL, -7);
  t_si128f64(0x7fffffffffffffffL, 0x7fffffffffffffffL);
  t_si128f64(123456789123456789L, 1);
  t_si128f64(123456789123456789L, 3);
  t_si128f64(123456789123456789L, -7);
  t_si128f64(123456789123456789L, 0x7fffffffffffffffL);
  t_si128f64(((__int128)0x7fffffffffffffffL<<40)+12345, 1);
  t_si128f64(((__int128)0x7fffffffffffffffL<<40)+12345, 3);
  t_si128f64(((__int128)0x7fffffffffffffffL<<40)+12345, -7);
  t_si128f64(((__int128)0x7fffffffffffffffL<<40)+12345, 0x7fffffffffffffffL);
  t_si128f64(-((__int128)0x7fffffffffffffffL<<40)-12345, 1);
  t_si128f64(-((__int128)0x7fffffffffffffffL<<40)-12345, 3);
  t_si128f64(-((__int128)0x7fffffffffffffffL<<40)-12345, -7);
  t_si128f64(-((__int128)0x7fffffffffffffffL<<40)-12345, 0x7fffffffffffffffL);
  t_ui128f32(1, 1);
  t_ui128f32(1, 3);
  t_ui128f32(1, -7);
  t_ui128f32(1, 0x7fffffffffffffffL);
  t_ui128f32(3, 1);
  t_ui128f32(3, 3);
  t_ui128f32(3, -7);
  t_ui128f32(3, 0x7fffffffffffffffL);
  t_ui128f32(-7, 1);
  t_ui128f32(-7, 3);
  t_ui128f32(-7, -7);
  t_ui128f32(-7, 0x7fffffffffffffffL);
  t_ui128f32(0x7fffffffffffffffL, 1);
  t_ui128f32(0x7fffffffffffffffL, 3);
  t_ui128f32(0x7fffffffffffffffL, -7);
  t_ui128f32(0x7fffffffffffffffL, 0x7fffffffffffffffL);
  t_ui128f32(123456789123456789L, 1);
  t_ui128f32(123456789123456789L, 3);
  t_ui128f32(123456789123456789L, -7);
  t_ui128f32(123456789123456789L, 0x7fffffffffffffffL);
  t_ui128f32(((__int128)0x7fffffffffffffffL<<40)+12345, 1);
  t_ui128f32(((__int128)0x7fffffffffffffffL<<40)+12345, 3);
  t_ui128f32(((__int128)0x7fffffffffffffffL<<40)+12345, -7);
  t_ui128f32(((__int128)0x7fffffffffffffffL<<40)+12345, 0x7fffffffffffffffL);
  t_ui128f32(-((__int128)0x7fffffffffffffffL<<40)-12345, 1);
  t_ui128f32(-((__int128)0x7fffffffffffffffL<<40)-12345, 3);
  t_ui128f32(-((__int128)0x7fffffffffffffffL<<40)-12345, -7);
  t_ui128f32(-((__int128)0x7fffffffffffffffL<<40)-12345, 0x7fffffffffffffffL);
  t_si128f80(1, 1);
  t_si128f80(1, 3);
  t_si128f80(1, -7);
  t_si128f80(1, 0x7fffffffffffffffL);
  t_si128f80(3, 1);
  t_si128f80(3, 3);
  t_si128f80(3, -7);
  t_si128f80(3, 0x7fffffffffffffffL);
  t_si128f80(-7, 1);
  t_si128f80(-7, 3);
  t_si128f80(-7, -7);
  t_si128f80(-7, 0x7fffffffffffffffL);
  t_si128f80(0x7fffffffffffffffL, 1);
  t_si128f80(0x7fffffffffffffffL, 3);
  t_si128f80(0x7fffffffffffffffL, -7);
  t_si128f80(0x7fffffffffffffffL, 0x7fffffffffffffffL);
  t_si128f80(123456789123456789L, 1);
  t_si128f80(123456789123456789L, 3);
  t_si128f80(123456789123456789L, -7);
  t_si128f80(123456789123456789L, 0x7fffffffffffffffL);
  t_si128f80(((__int128)0x7fffffffffffffffL<<40)+12345, 1);
  t_si128f80(((__int128)0x7fffffffffffffffL<<40)+12345, 3);
  t_si128f80(((__int128)0x7fffffffffffffffL<<40)+12345, -7);
  t_si128f80(((__int128)0x7fffffffffffffffL<<40)+12345, 0x7fffffffffffffffL);
  t_si128f80(-((__int128)0x7fffffffffffffffL<<40)-12345, 1);
  t_si128f80(-((__int128)0x7fffffffffffffffL<<40)-12345, 3);
  t_si128f80(-((__int128)0x7fffffffffffffffL<<40)-12345, -7);
  t_si128f80(-((__int128)0x7fffffffffffffffL<<40)-12345, 0x7fffffffffffffffL);
  t_si64f16(1, 1);
  t_si64f16(1, 3);
  t_si64f16(1, -7);
  t_si64f16(1, 0x7fffffffffffffffL);
  t_si64f16(3, 1);
  t_si64f16(3, 3);
  t_si64f16(3, -7);
  t_si64f16(3, 0x7fffffffffffffffL);
  t_si64f16(-7, 1);
  t_si64f16(-7, 3);
  t_si64f16(-7, -7);
  t_si64f16(-7, 0x7fffffffffffffffL);
  t_si64f16(0x7fffffffffffffffL, 1);
  t_si64f16(0x7fffffffffffffffL, 3);
  t_si64f16(0x7fffffffffffffffL, -7);
  t_si64f16(0x7fffffffffffffffL, 0x7fffffffffffffffL);
  t_si64f16(123456789123456789L, 1);
  t_si64f16(123456789123456789L, 3);
  t_si64f16(123456789123456789L, -7);
  t_si64f16(123456789123456789L, 0x7fffffffffffffffL);
  t_fp64si(1.0, 1.0);
  t_fp64si(1.0, 3.0);
  t_fp64si(1.0, 0.1);
  t_fp64si(1.0, -7.0);
  t_fp64si(3.0, 1.0);
  t_fp64si(3.0, 3.0);
  t_fp64si(3.0, 0.1);
  t_fp64si(3.0, -7.0);
  t_fp64si(0.1, 1.0);
  t_fp64si(0.1, 3.0);
  t_fp64si(0.1, 0.1);
  t_fp64si(0.1, -7.0);
  t_fp64si(-7.0, 1.0);
  t_fp64si(-7.0, 3.0);
  t_fp64si(-7.0, 0.1);
  t_fp64si(-7.0, -7.0);
  t_fp64si(1e300, 1.0);
  t_fp64si(1e300, 3.0);
  t_fp64si(1e300, 0.1);
  t_fp64si(1e300, -7.0);
  t_fp64si(0.0, 1.0);
  t_fp64si(0.0, 3.0);
  t_fp64si(0.0, 0.1);
  t_fp64si(0.0, -7.0);
  t_fp64si(0x1.fffffffffffffp+60, 1.0);
  t_fp64si(0x1.fffffffffffffp+60, 3.0);
  t_fp64si(0x1.fffffffffffffp+60, 0.1);
  t_fp64si(0x1.fffffffffffffp+60, -7.0);
  t_fp64si(-0x1.8p+70, 1.0);
  t_fp64si(-0x1.8p+70, 3.0);
  t_fp64si(-0x1.8p+70, 0.1);
  t_fp64si(-0x1.8p+70, -7.0);
  t_fp64si(0x1.8p+100, 1.0);
  t_fp64si(0x1.8p+100, 3.0);
  t_fp64si(0x1.8p+100, 0.1);
  t_fp64si(0x1.8p+100, -7.0);
  t_fp64si(0x1.0000000000001p+65, 1.0);
  t_fp64si(0x1.0000000000001p+65, 3.0);
  t_fp64si(0x1.0000000000001p+65, 0.1);
  t_fp64si(0x1.0000000000001p+65, -7.0);
  t_fp64si(-0.5, 1.0);
  t_fp64si(-0.5, 3.0);
  t_fp64si(-0.5, 0.1);
  t_fp64si(-0.5, -7.0);
  t_fp64ui(1.0, 1.0);
  t_fp64ui(1.0, 3.0);
  t_fp64ui(1.0, 0.1);
  t_fp64ui(1.0, -7.0);
  t_fp64ui(3.0, 1.0);
  t_fp64ui(3.0, 3.0);
  t_fp64ui(3.0, 0.1);
  t_fp64ui(3.0, -7.0);
  t_fp64ui(0.1, 1.0);
  t_fp64ui(0.1, 3.0);
  t_fp64ui(0.1, 0.1);
  t_fp64ui(0.1, -7.0);
  t_fp64ui(-7.0, 1.0);
  t_fp64ui(-7.0, 3.0);
  t_fp64ui(-7.0, 0.1);
  t_fp64ui(-7.0, -7.0);
  t_fp64ui(1e300, 1.0);
  t_fp64ui(1e300, 3.0);
  t_fp64ui(1e300, 0.1);
  t_fp64ui(1e300, -7.0);
  t_fp64ui(0.0, 1.0);
  t_fp64ui(0.0, 3.0);
  t_fp64ui(0.0, 0.1);
  t_fp64ui(0.0, -7.0);
  t_fp64ui(0x1.fffffffffffffp+60, 1.0);
  t_fp64ui(0x1.fffffffffffffp+60, 3.0);
  t_fp64ui(0x1.fffffffffffffp+60, 0.1);
  t_fp64ui(0x1.fffffffffffffp+60, -7.0);
  t_fp64ui(-0x1.8p+70, 1.0);
  t_fp64ui(-0x1.8p+70, 3.0);
  t_fp64ui(-0x1.8p+70, 0.1);
  t_fp64ui(-0x1.8p+70, -7.0);
  t_fp64ui(0x1.8p+100, 1.0);
  t_fp64ui(0x1.8p+100, 3.0);
  t_fp64ui(0x1.8p+100, 0.1);
  t_fp64ui(0x1.8p+100, -7.0);
  t_fp64ui(0x1.0000000000001p+65, 1.0);
  t_fp64ui(0x1.0000000000001p+65, 3.0);
  t_fp64ui(0x1.0000000000001p+65, 0.1);
  t_fp64ui(0x1.0000000000001p+65, -7.0);
  t_fp64ui(-0.5, 1.0);
  t_fp64ui(-0.5, 3.0);
  t_fp64ui(-0.5, 0.1);
  t_fp64ui(-0.5, -7.0);
  t_fp32ui(1.0f, 1.0f);
  t_fp32ui(1.0f, 3.0f);
  t_fp32ui(1.0f, 0.1f);
  t_fp32ui(1.0f, -7.0f);
  t_fp32ui(3.0f, 1.0f);
  t_fp32ui(3.0f, 3.0f);
  t_fp32ui(3.0f, 0.1f);
  t_fp32ui(3.0f, -7.0f);
  t_fp32ui(0.1f, 1.0f);
  t_fp32ui(0.1f, 3.0f);
  t_fp32ui(0.1f, 0.1f);
  t_fp32ui(0.1f, -7.0f);
  t_fp32ui(-7.0f, 1.0f);
  t_fp32ui(-7.0f, 3.0f);
  t_fp32ui(-7.0f, 0.1f);
  t_fp32ui(-7.0f, -7.0f);
  t_fp32ui(1e30f, 1.0f);
  t_fp32ui(1e30f, 3.0f);
  t_fp32ui(1e30f, 0.1f);
  t_fp32ui(1e30f, -7.0f);
  t_fp32ui(0.0f, 1.0f);
  t_fp32ui(0.0f, 3.0f);
  t_fp32ui(0.0f, 0.1f);
  t_fp32ui(0.0f, -7.0f);
  t_fp80ui(1.0, 1.0);
  t_fp80ui(1.0, 3.0);
  t_fp80ui(1.0, 0.1);
  t_fp80ui(1.0, -7.0);
  t_fp80ui(3.0, 1.0);
  t_fp80ui(3.0, 3.0);
  t_fp80ui(3.0, 0.1);
  t_fp80ui(3.0, -7.0);
  t_fp80ui(0.1, 1.0);
  t_fp80ui(0.1, 3.0);
  t_fp80ui(0.1, 0.1);
  t_fp80ui(0.1, -7.0);
  t_fp80ui(-7.0, 1.0);
  t_fp80ui(-7.0, 3.0);
  t_fp80ui(-7.0, 0.1);
  t_fp80ui(-7.0, -7.0);
  t_fp80ui(1e300, 1.0);
  t_fp80ui(1e300, 3.0);
  t_fp80ui(1e300, 0.1);
  t_fp80ui(1e300, -7.0);
  t_fp80ui(0.0, 1.0);
  t_fp80ui(0.0, 3.0);
  t_fp80ui(0.0, 0.1);
  t_fp80ui(0.0, -7.0);
  t_fp80ui(0x1.fffffffffffffp+60, 1.0);
  t_fp80ui(0x1.fffffffffffffp+60, 3.0);
  t_fp80ui(0x1.fffffffffffffp+60, 0.1);
  t_fp80ui(0x1.fffffffffffffp+60, -7.0);
  t_fp80ui(-0x1.8p+70, 1.0);
  t_fp80ui(-0x1.8p+70, 3.0);
  t_fp80ui(-0x1.8p+70, 0.1);
  t_fp80ui(-0x1.8p+70, -7.0);
  t_fp80ui(0x1.8p+100, 1.0);
  t_fp80ui(0x1.8p+100, 3.0);
  t_fp80ui(0x1.8p+100, 0.1);
  t_fp80ui(0x1.8p+100, -7.0);
  t_fp80ui(0x1.0000000000001p+65, 1.0);
  t_fp80ui(0x1.0000000000001p+65, 3.0);
  t_fp80ui(0x1.0000000000001p+65, 0.1);
  t_fp80ui(0x1.0000000000001p+65, -7.0);
  t_fp80ui(-0.5, 1.0);
  t_fp80ui(-0.5, 3.0);
  t_fp80ui(-0.5, 0.1);
  t_fp80ui(-0.5, -7.0);
  t_si200f64((__int128)1 << 126, (__int128)1 << 126);
  t_si200f64((__int128)1 << 126, ((__int128)0x7fffffffffffffffL<<62)+1);
  t_si200f64((__int128)1 << 126, ((__int128)0x1fffffffffffffL<<70)+((__int128)1<<16));
  t_si200f64((__int128)1 << 126, -((__int128)0x7fffffffffffffffL<<60)-12345);
  t_si200f64(((__int128)0x7fffffffffffffffL<<62)+1, (__int128)1 << 126);
  t_si200f64(((__int128)0x7fffffffffffffffL<<62)+1, ((__int128)0x7fffffffffffffffL<<62)+1);
  t_si200f64(((__int128)0x7fffffffffffffffL<<62)+1, ((__int128)0x1fffffffffffffL<<70)+((__int128)1<<16));
  t_si200f64(((__int128)0x7fffffffffffffffL<<62)+1, -((__int128)0x7fffffffffffffffL<<60)-12345);
  t_si200f64(((__int128)0x1fffffffffffffL<<70)+((__int128)1<<16), (__int128)1 << 126);
  t_si200f64(((__int128)0x1fffffffffffffL<<70)+((__int128)1<<16), ((__int128)0x7fffffffffffffffL<<62)+1);
  t_si200f64(((__int128)0x1fffffffffffffL<<70)+((__int128)1<<16), ((__int128)0x1fffffffffffffL<<70)+((__int128)1<<16));
  t_si200f64(((__int128)0x1fffffffffffffL<<70)+((__int128)1<<16), -((__int128)0x7fffffffffffffffL<<60)-12345);
  t_si200f64(-((__int128)0x7fffffffffffffffL<<60)-12345, (__int128)1 << 126);
  t_si200f64(-((__int128)0x7fffffffffffffffL<<60)-12345, ((__int128)0x7fffffffffffffffL<<62)+1);
  t_si200f64(-((__int128)0x7fffffffffffffffL<<60)-12345, ((__int128)0x1fffffffffffffL<<70)+((__int128)1<<16));
  t_si200f64(-((__int128)0x7fffffffffffffffL<<60)-12345, -((__int128)0x7fffffffffffffffL<<60)-12345);
  t_si200f64(-((__int128)1 << 126), (__int128)1 << 126);
  t_si200f64(-((__int128)1 << 126), ((__int128)0x7fffffffffffffffL<<62)+1);
  t_si200f64(-((__int128)1 << 126), ((__int128)0x1fffffffffffffL<<70)+((__int128)1<<16));
  t_si200f64(-((__int128)1 << 126), -((__int128)0x7fffffffffffffffL<<60)-12345);
  t_ui200f32((__int128)1 << 126, (__int128)1 << 126);
  t_ui200f32((__int128)1 << 126, ((__int128)0x7fffffffffffffffL<<62)+1);
  t_ui200f32((__int128)1 << 126, ((__int128)0x1fffffffffffffL<<70)+((__int128)1<<16));
  t_ui200f32(((__int128)0x7fffffffffffffffL<<62)+1, (__int128)1 << 126);
  t_ui200f32(((__int128)0x7fffffffffffffffL<<62)+1, ((__int128)0x7fffffffffffffffL<<62)+1);
  t_ui200f32(((__int128)0x7fffffffffffffffL<<62)+1, ((__int128)0x1fffffffffffffL<<70)+((__int128)1<<16));
  t_ui200f32(((__int128)0x1fffffffffffffL<<70)+((__int128)1<<16), (__int128)1 << 126);
  t_ui200f32(((__int128)0x1fffffffffffffL<<70)+((__int128)1<<16), ((__int128)0x7fffffffffffffffL<<62)+1);
  t_ui200f32(((__int128)0x1fffffffffffffL<<70)+((__int128)1<<16), ((__int128)0x1fffffffffffffL<<70)+((__int128)1<<16));
  t_fp64ui128(1.0);
  t_fp64ui128(3.0);
  t_fp64ui128(0.1);
  t_fp64ui128(-7.0);
  t_fp64ui128(1e300);
  t_fp64ui128(0.0);
  t_fp64ui128(0x1.fffffffffffffp+60);
  t_fp64ui128(-0x1.8p+70);
  t_fp64ui128(0x1.8p+100);
  t_fp64ui128(0x1.0000000000001p+65);
  t_fp64ui128(-0.5);
  t_fma3();
  if (fails) printf("fails=%d\n", fails);
  return fails != 0;
}
