#if U'\u{10fffd}' != U'\o{4177775}' || U'\x{000001234}' != U'\u1234'
#error delimited preprocessing escape has the wrong value
#endif

static_assert(sizeof "\x{ff}" == 2);
static_assert(sizeof "\u{1234}" == 4);
static_assert(sizeof u"\u{10000}" == 6);
static_assert(U'\u{10fffd}' == U'\o{4177775}');
static_assert('\E' == 27);
