#if U'\u{10fffd}' != U'\o{4177775}' || U'\x{000001234}' != U'\u1234'
#error delimited preprocessing escape has the wrong value
#endif

static_assert(sizeof "\x{ff}" == 2);
static_assert(sizeof "\u{1234}" == 4);
static_assert(sizeof u"\u{10000}" == 6);
static_assert(U'\u{10fffd}' == U'\o{4177775}');
static_assert('\E' == 27);

// An incomplete UCN is a backslash followed by ordinary tokens. Here
// replacement consumes them before they would need to form a C expression.
#define DISCARD_UCN discard_ucn(
#define discard_ucn(unused) incomplete_ucn_ok
int DISCARD_UCN\U0000000z);
