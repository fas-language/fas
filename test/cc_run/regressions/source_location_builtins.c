// clang's source-location builtins: the call's expansion site, constant
// LINE/COLUMN of type unsigned, char * strings (const in gcc), FUNCTION empty
// outside a function, and __has_builtin knows them.
#include <stdio.h>
#include <string.h>

#define T(e) _Generic((e), const char *: "const char *", char *: "char *", unsigned: "unsigned", int: "int", default: "other")
#define WHERE() printf("%s:%u:%u %s\n", __builtin_FILE_NAME(), __builtin_LINE(), __builtin_COLUMN(), __builtin_FUNCTION())

#if !__has_builtin(__builtin_FILE) || !__has_builtin(__builtin_LINE) || !__has_builtin(__builtin_FUNCTION) || \
    !__has_builtin(__builtin_COLUMN) || !__has_builtin(__builtin_FILE_NAME)
#error missing source-location builtin
#endif

static const char *top = __builtin_FUNCTION();
static const char *file = __builtin_FILE();
_Static_assert(__builtin_LINE() == 17, "line");
static int sized[__builtin_LINE()];

static void report(const char *f, unsigned line) { printf("%s %u\n", strrchr(f, '/') ? strrchr(f, '/') + 1 : f, line); }

int main(void) {
    WHERE();
    report(__builtin_FILE(), __builtin_LINE());
    printf("[%s] %d %zu %s\n", top, strcmp(file, __FILE__), sizeof sized, __builtin_FUNCTION());
    printf("%s %s %s %s\n", T(__builtin_FILE()), T(__builtin_FUNCTION()), T(__builtin_LINE()), T(__builtin_COLUMN()));
    printf("%u %u\n", __builtin_COLUMN(),
           __builtin_LINE());
    return 0;
}
