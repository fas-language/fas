#include "calls.h"

typedef int lanes32 __attribute__((vector_size(32)));
typedef int lanes64 __attribute__((vector_size(64)));

#ifdef CLANG_RECORD_ABI
/* Clang splits bare wide vectors under Win64. A single-vector record
   gives it the same indirect argument and result ABI as GCC's vector. */
typedef struct { alignas(32) lanes32 value; } wide32;
typedef struct { alignas(64) lanes64 value; } wide64;
#define LANES(x) ((x).value)
#else
typedef lanes32 wide32;
typedef lanes64 wide64;
#define LANES(x) (x)
#endif

typedef wide64 MS wide_fn(wide32, double, int, int, wide64);
wide_fn wide_stack;
MS wide32 wide_varargs(int, ...);
