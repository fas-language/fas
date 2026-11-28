static_assert(__builtin_bswap16(0x10102UL) == 0x0201);
static_assert(__builtin_bswap32(0x01020304U) == 0x04030201U);
static_assert(__builtin_bswap64(0x0102030405060708UL) == 0x0807060504030201UL);

static const unsigned short protocol = __builtin_bswap16(0x0800);
int main(void) {
    switch (protocol) {
    case __builtin_bswap16(0x0800):
        return 0;
    default:
        return 1;
    }
}
