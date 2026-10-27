#ifndef MC_CHACHA20_H
#define MC_CHACHA20_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define MC_CHACHA20_KEY_SIZE 32u
#define MC_CHACHA20_NONCE_SIZE 12u
#define MC_CHACHA20_BLOCK_SIZE 64u

#define MC_CHACHA20_OK 0
#define MC_CHACHA20_ERR_NULL -1
#define MC_CHACHA20_ERR_COUNTER_OVERFLOW -2

typedef struct mc_chacha20_ctx {
    uint32_t state[16];
    uint8_t block[64];
    uint8_t block_used;
    uint8_t overflow;
} mc_chacha20_ctx;

void mc_chacha20_wipe(void *ptr, size_t len);

int mc_chacha20_block(uint8_t out[64],
                      const uint8_t key[32],
                      const uint8_t nonce[12],
                      uint32_t counter);

int mc_chacha20_init(mc_chacha20_ctx *ctx,
                     const uint8_t key[32],
                     const uint8_t nonce[12],
                     uint32_t counter);

int mc_chacha20_update(mc_chacha20_ctx *ctx, uint8_t *out, const uint8_t *in, size_t len);

int mc_chacha20_keystream_update(mc_chacha20_ctx *ctx, uint8_t *out, size_t len);

void mc_chacha20_wipe_ctx(mc_chacha20_ctx *ctx);

int mc_chacha20_xor(uint8_t *out,
                    const uint8_t *in,
                    size_t len,
                    const uint8_t key[32],
                    const uint8_t nonce[12],
                    uint32_t counter);

int mc_chacha20_keystream(
    uint8_t *out, size_t len, const uint8_t key[32], const uint8_t nonce[12], uint32_t counter);

#ifdef __cplusplus
}
#endif

#endif /* MC_CHACHA20_H */
