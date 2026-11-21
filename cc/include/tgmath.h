#ifndef __FCC_TGMATH_H
#define __FCC_TGMATH_H
#include <math.h>
/* glibc's generic dispatch requires both of these formats or neither.
   GCC 4.2 compatibility exposes _Float64x but no _Float128 on x86-64.
   _Float64x is the same long-double format here. */
#if defined(__GLIBC__) && __HAVE_FLOAT64X && !__HAVE_FLOAT128 && __HAVE_FLOAT64X_LONG_DOUBLE
#pragma push_macro("__HAVE_FLOAT64X")
#undef __HAVE_FLOAT64X
#define __HAVE_FLOAT64X 0
#include_next <tgmath.h>
#pragma pop_macro("__HAVE_FLOAT64X")
#else
#include_next <tgmath.h>
#endif
#endif
