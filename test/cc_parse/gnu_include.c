#define END h>
#include <stddef.END
#define EMPTY
#include <stddef.h> EMPTY

#pragma mark John's work
_Pragma("mark John's work")

static_assert(offsetof(struct { int a; int b; }, b) == sizeof(int));
