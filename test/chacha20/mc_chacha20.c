#include "mc_chacha20.h"

#ifndef MC_CHACHA20_NO_NULL_CHECKS
#define TCH_REQUIRE(x)                                                                             \
    do {                                                                                           \
        if (!(x)) {                                                                                \
            return MC_CHACHA20_ERR_NULL;                                                           \
        }                                                                                          \
    } while (0)
#else
#define TCH_REQUIRE(x)                                                                             \
    do {                                                                                           \
        (void)sizeof(x);                                                                           \
    } while (0)
#endif

static uint32_t tch_load32_le(const uint8_t src[4])
{
    return ((uint32_t)src[0]) | ((uint32_t)src[1] << 8) | ((uint32_t)src[2] << 16) |
           ((uint32_t)src[3] << 24);
}

static void tch_store32_le(uint8_t dst[4], uint32_t x)
{
    dst[0] = (uint8_t)(x);
    dst[1] = (uint8_t)(x >> 8);
    dst[2] = (uint8_t)(x >> 16);
    dst[3] = (uint8_t)(x >> 24);
}

static uint32_t tch_rotl32(uint32_t x, unsigned n)
{
    return (x << n) | (x >> (32u - n));
}

static void tch_quarter_round(uint32_t *a, uint32_t *b, uint32_t *c, uint32_t *d)
{
    *a += *b;
    *d ^= *a;
    *d = tch_rotl32(*d, 16u);
    *c += *d;
    *b ^= *c;
    *b = tch_rotl32(*b, 12u);
    *a += *b;
    *d ^= *a;
    *d = tch_rotl32(*d, 8u);
    *c += *d;
    *b ^= *c;
    *b = tch_rotl32(*b, 7u);
}

void mc_chacha20_wipe(void *ptr, size_t len)
{
#ifndef MC_CHACHA20_NO_INTERNAL_WIPE
    volatile uint8_t *p = (volatile uint8_t *)ptr;
    while (len != 0u) {
        *p++ = 0u;
        len--;
    }
#else
    (void)ptr;
    (void)len;
#endif
}

static void tch_block_from_state(uint8_t out[64], const uint32_t state[16])
{
    uint32_t x[16];
    unsigned i;

    for (i = 0u; i < 16u; i++) {
        x[i] = state[i];
    }

    for (i = 0u; i < 10u; i++) {
        tch_quarter_round(&x[0], &x[4], &x[8], &x[12]);
        tch_quarter_round(&x[1], &x[5], &x[9], &x[13]);
        tch_quarter_round(&x[2], &x[6], &x[10], &x[14]);
        tch_quarter_round(&x[3], &x[7], &x[11], &x[15]);
        tch_quarter_round(&x[0], &x[5], &x[10], &x[15]);
        tch_quarter_round(&x[1], &x[6], &x[11], &x[12]);
        tch_quarter_round(&x[2], &x[7], &x[8], &x[13]);
        tch_quarter_round(&x[3], &x[4], &x[9], &x[14]);
    }

    for (i = 0u; i < 16u; i++) {
        tch_store32_le(out + (i * 4u), x[i] + state[i]);
    }

    mc_chacha20_wipe(x, sizeof(x));
}

static void tch_setup_state(uint32_t state[16],
                            const uint8_t key[32],
                            const uint8_t nonce[12],
                            uint32_t counter)
{
    state[0] = 0x61707865u;
    state[1] = 0x3320646eu;
    state[2] = 0x79622d32u;
    state[3] = 0x6b206574u;
    state[4] = tch_load32_le(key + 0u);
    state[5] = tch_load32_le(key + 4u);
    state[6] = tch_load32_le(key + 8u);
    state[7] = tch_load32_le(key + 12u);
    state[8] = tch_load32_le(key + 16u);
    state[9] = tch_load32_le(key + 20u);
    state[10] = tch_load32_le(key + 24u);
    state[11] = tch_load32_le(key + 28u);
    state[12] = counter;
    state[13] = tch_load32_le(nonce + 0u);
    state[14] = tch_load32_le(nonce + 4u);
    state[15] = tch_load32_le(nonce + 8u);
}

int mc_chacha20_block(uint8_t out[64],
                      const uint8_t key[32],
                      const uint8_t nonce[12],
                      uint32_t counter)
{
    uint32_t state[16];

    TCH_REQUIRE(out != 0);
    TCH_REQUIRE(key != 0);
    TCH_REQUIRE(nonce != 0);

    tch_setup_state(state, key, nonce, counter);
    tch_block_from_state(out, state);
    mc_chacha20_wipe(state, sizeof(state));
    return MC_CHACHA20_OK;
}

int mc_chacha20_init(mc_chacha20_ctx *ctx,
                     const uint8_t key[32],
                     const uint8_t nonce[12],
                     uint32_t counter)
{
    TCH_REQUIRE(ctx != 0);
    TCH_REQUIRE(key != 0);
    TCH_REQUIRE(nonce != 0);

    tch_setup_state(ctx->state, key, nonce, counter);
    ctx->block_used = MC_CHACHA20_BLOCK_SIZE;
    ctx->overflow = 0u;

    return MC_CHACHA20_OK;
}

static int tch_refill(mc_chacha20_ctx *ctx)
{
    if (ctx->overflow != 0u) {
        return MC_CHACHA20_ERR_COUNTER_OVERFLOW;
    }

    tch_block_from_state(ctx->block, ctx->state);
    ctx->block_used = 0u;

    if (ctx->state[12] == 0xffffffffu) {
        ctx->state[12] = 0u;
        ctx->overflow = 1u;
    } else {
        ctx->state[12]++;
    }

    return MC_CHACHA20_OK;
}

int mc_chacha20_update(mc_chacha20_ctx *ctx, uint8_t *out, const uint8_t *in, size_t len)
{
    size_t i;

    TCH_REQUIRE(ctx != 0);
    if (len != 0u) {
        TCH_REQUIRE(out != 0);
        TCH_REQUIRE(in != 0);
    }

    for (i = 0u; i < len; i++) {
        if (ctx->block_used >= MC_CHACHA20_BLOCK_SIZE) {
            int rc = tch_refill(ctx);
            if (rc != MC_CHACHA20_OK) {
                return rc;
            }
        }
        out[i] = (uint8_t)(in[i] ^ ctx->block[ctx->block_used]);
        ctx->block[ctx->block_used] = 0u;
        ctx->block_used++;
    }

    return MC_CHACHA20_OK;
}

int mc_chacha20_keystream_update(mc_chacha20_ctx *ctx, uint8_t *out, size_t len)
{
    size_t i;

    TCH_REQUIRE(ctx != 0);
    if (len != 0u) {
        TCH_REQUIRE(out != 0);
    }

    for (i = 0u; i < len; i++) {
        if (ctx->block_used >= MC_CHACHA20_BLOCK_SIZE) {
            int rc = tch_refill(ctx);
            if (rc != MC_CHACHA20_OK) {
                return rc;
            }
        }
        out[i] = ctx->block[ctx->block_used];
        ctx->block[ctx->block_used] = 0u;
        ctx->block_used++;
    }

    return MC_CHACHA20_OK;
}

void mc_chacha20_wipe_ctx(mc_chacha20_ctx *ctx)
{
    if (ctx != 0) {
        mc_chacha20_wipe(ctx, sizeof(*ctx));
    }
}

int mc_chacha20_xor(uint8_t *out,
                    const uint8_t *in,
                    size_t len,
                    const uint8_t key[32],
                    const uint8_t nonce[12],
                    uint32_t counter)
{
    mc_chacha20_ctx ctx;
    int rc;

    rc = mc_chacha20_init(&ctx, key, nonce, counter);
    if (rc != MC_CHACHA20_OK) {
        return rc;
    }

    rc = mc_chacha20_update(&ctx, out, in, len);
    mc_chacha20_wipe_ctx(&ctx);
    return rc;
}

int mc_chacha20_keystream(
    uint8_t *out, size_t len, const uint8_t key[32], const uint8_t nonce[12], uint32_t counter)
{
    mc_chacha20_ctx ctx;
    int rc;

    rc = mc_chacha20_init(&ctx, key, nonce, counter);
    if (rc != MC_CHACHA20_OK) {
        return rc;
    }

    rc = mc_chacha20_keystream_update(&ctx, out, len);
    mc_chacha20_wipe_ctx(&ctx);
    return rc;
}
