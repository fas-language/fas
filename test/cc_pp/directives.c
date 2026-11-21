#define A 1
#if defined A && 0
bad
#elifdef A
selected
#elifndef B
bad
#endif
#if 0
bad
#elifndef B
selected
#endif
#if 0 && (1/0)
bad
#elif ((-1 < 1U) ? 0 : (u'X' == 88 && 'AB' == 0x4142))
selected
#endif
#if __has_include("first/chain.h") && __has_builtin(__builtin_expect)
#include <chain.h>
#include <chain.h>
#endif
#line 100 "logical.c"
__FILE__ __FILE_NAME__ __LINE__ __INCLUDE_LEVEL__
#define STR(x) #x
#define PRAGMA(x) _Pragma(STR(x))
PRAGMA(GCC diagnostic push)
#pragma GCC diagnostic pop
__COUNTER__ __COUNTER__
