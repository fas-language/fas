#include <stddef.h>

#if __SIZEOF_WCHAR_T__ != 2 || __WCHAR_WIDTH__ != 16 || __WCHAR_MAX__ != 65535
#error incorrect macros for -fshort-wchar
#endif
#if (defined(__clang__) || defined(__fcc__)) && !__WCHAR_UNSIGNED__
#error missing unsigned wchar_t macro
#endif
#if L'\xffff' != 65535
#error incorrect wide character in a preprocessor expression
#endif

static_assert(_Generic(L'\xffff', unsigned short: 1, default: 0));
static_assert(sizeof(L"\U0001f600") == 6);
static const wchar_t wide[] = L"A" "\U0001f600";

int main(void) {
    const wchar_t *s = L"EFI";
    return s[0] != 'E' || s[3] != 0 ||
           wide[0] != 'A' || wide[1] != 0xd83d || wide[2] != 0xde00 || wide[3] != 0;
}
