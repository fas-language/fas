#if __has_embed("embed.dat") != __STDC_EMBED_FOUND__
#error expected an available embed file
#endif
#if __has_embed("embed.dat" limit(0)) != __STDC_EMBED_EMPTY__
#error expected empty result
#endif
#if __has_embed("embed.dat" vendor::unknown(1)) != __STDC_EMBED_NOT_FOUND__
#error expected unknown parameter result
#endif
#define N 1
static const unsigned char a[] = {
#embed "embed.dat" limit(N + 1) prefix(2,) suffix(, 3)
};
static const unsigned char b[] = {
#embed "embed.dat" limit(0) if_empty(7)
};
