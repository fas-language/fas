#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <x86intrin.h>

#include "chacha20/mc_chacha20.h"

int c_mc_chacha20_block(uint8_t out[64], const uint8_t key[32], const uint8_t nonce[12], uint32_t counter);
int c_mc_chacha20_init(mc_chacha20_ctx *ctx, const uint8_t key[32], const uint8_t nonce[12], uint32_t counter);
int c_mc_chacha20_update(mc_chacha20_ctx *ctx, uint8_t *out, const uint8_t *in, size_t len);
int c_mc_chacha20_keystream_update(mc_chacha20_ctx *ctx, uint8_t *out, size_t len);
void c_mc_chacha20_wipe_ctx(mc_chacha20_ctx *ctx);
int c_mc_chacha20_xor(uint8_t *out, const uint8_t *in, size_t len, const uint8_t key[32], const uint8_t nonce[12], uint32_t counter);
int c_mc_chacha20_keystream(uint8_t *out, size_t len, const uint8_t key[32], const uint8_t nonce[12], uint32_t counter);
void c_mc_chacha20_wipe(void *ptr, size_t len);

static uint64_t rng_state = UINT64_C(0x8439a20f31b75d6c);
static volatile uint8_t bench_sink;

static int fail(const char *message)
{
    fprintf(stderr, "chacha20: %s\n", message);
    return 1;
}

static uint64_t next_random(void)
{
    uint64_t x = rng_state;
    x ^= x << 13;
    x ^= x >> 7;
    x ^= x << 17;
    rng_state = x;
    return x;
}

static uint32_t next_u32(void)
{
    return (uint32_t)next_random();
}

static int check_rfc_vectors(void)
{
    uint8_t key[32];
    uint8_t block_nonce[12] = {0, 0, 0, 9, 0, 0, 0, 0x4a, 0, 0, 0, 0};
    uint8_t cipher_nonce[12] = {0, 0, 0, 0, 0, 0, 0, 0x4a, 0, 0, 0, 0};
    uint8_t block[64];
    uint8_t c_block[64];
    uint8_t cipher[114];
    uint8_t c_cipher[114];
    static const uint8_t expected_block[64] = {
        0x10, 0xf1, 0xe7, 0xe4, 0xd1, 0x3b, 0x59, 0x15, 0x50, 0x0f, 0xdd, 0x1f, 0xa3, 0x20, 0x71, 0xc4,
        0xc7, 0xd1, 0xf4, 0xc7, 0x33, 0xc0, 0x68, 0x03, 0x04, 0x22, 0xaa, 0x9a, 0xc3, 0xd4, 0x6c, 0x4e,
        0xd2, 0x82, 0x64, 0x46, 0x07, 0x9f, 0xaa, 0x09, 0x14, 0xc2, 0xd7, 0x05, 0xd9, 0x8b, 0x02, 0xa2,
        0xb5, 0x12, 0x9c, 0xd1, 0xde, 0x16, 0x4e, 0xb9, 0xcb, 0xd0, 0x83, 0xe8, 0xa2, 0x50, 0x3c, 0x4e};
    static const uint8_t expected_cipher[114] = {
        0x6e, 0x2e, 0x35, 0x9a, 0x25, 0x68, 0xf9, 0x80, 0x41, 0xba, 0x07, 0x28, 0xdd, 0x0d, 0x69, 0x81,
        0xe9, 0x7e, 0x7a, 0xec, 0x1d, 0x43, 0x60, 0xc2, 0x0a, 0x27, 0xaf, 0xcc, 0xfd, 0x9f, 0xae, 0x0b,
        0xf9, 0x1b, 0x65, 0xc5, 0x52, 0x47, 0x33, 0xab, 0x8f, 0x59, 0x3d, 0xab, 0xcd, 0x62, 0xb3, 0x57,
        0x16, 0x39, 0xd6, 0x24, 0xe6, 0x51, 0x52, 0xab, 0x8f, 0x53, 0x0c, 0x35, 0x9f, 0x08, 0x61, 0xd8,
        0x07, 0xca, 0x0d, 0xbf, 0x50, 0x0d, 0x6a, 0x61, 0x56, 0xa3, 0x8e, 0x08, 0x8a, 0x22, 0xb6, 0x5e,
        0x52, 0xbc, 0x51, 0x4d, 0x16, 0xcc, 0xf8, 0x06, 0x81, 0x8c, 0xe9, 0x1a, 0xb7, 0x79, 0x37, 0x36,
        0x5a, 0xf9, 0x0b, 0xbf, 0x74, 0xa3, 0x5b, 0xe6, 0xb4, 0x0b, 0x8e, 0xed, 0xf2, 0x78, 0x5e, 0x42,
        0x87, 0x4d};
    static const uint8_t plaintext[] =
        "Ladies and Gentlemen of the class of '99: If I could offer you only one tip for the future, sunscreen would be it.";
    size_t i;

    for (i = 0; i < sizeof(key); i++) {
        key[i] = (uint8_t)i;
    }
    if (sizeof(plaintext) - 1u != sizeof(cipher)) {
        return fail("RFC plaintext length mismatch");
    }
    if (mc_chacha20_block(block, key, block_nonce, 1u) != MC_CHACHA20_OK ||
        c_mc_chacha20_block(c_block, key, block_nonce, 1u) != MC_CHACHA20_OK ||
        memcmp(block, expected_block, sizeof(block)) != 0 ||
        memcmp(c_block, expected_block, sizeof(c_block)) != 0) {
        return fail("RFC 8439 block vector mismatch");
    }
    if (mc_chacha20_xor(cipher, plaintext, sizeof(plaintext) - 1u, key, cipher_nonce, 1u) != MC_CHACHA20_OK ||
        c_mc_chacha20_xor(c_cipher, plaintext, sizeof(plaintext) - 1u, key, cipher_nonce, 1u) != MC_CHACHA20_OK ||
        memcmp(cipher, expected_cipher, sizeof(cipher)) != 0 ||
        memcmp(c_cipher, expected_cipher, sizeof(c_cipher)) != 0) {
        return fail("RFC 8439 encryption vector mismatch");
    }
    return 0;
}

static int check_random_cases(void)
{
    uint8_t key[32];
    uint8_t nonce[12];
    uint8_t input[4096];
    uint8_t fas_out[4096];
    uint8_t c_out[4096];
    uint8_t fas_stream[4096];
    uint8_t c_stream[4096];
    uint8_t fas_block[64];
    uint8_t c_block[64];
    size_t case_index;

    for (case_index = 0; case_index < 1000; case_index++) {
        size_t i;
        size_t length;
        uint32_t counter;
        for (i = 0; i < sizeof(key); i++) {
            key[i] = (uint8_t)next_random();
        }
        for (i = 0; i < sizeof(nonce); i++) {
            nonce[i] = (uint8_t)next_random();
        }
        for (i = 0; i < sizeof(input); i++) {
            input[i] = (uint8_t)next_random();
        }
        length = (size_t)(next_random() % (sizeof(input) + 1u));
        counter = next_u32();
        memset(fas_out, 0xa5, sizeof(fas_out));
        memset(c_out, 0xa5, sizeof(c_out));
        memset(fas_stream, 0x5a, sizeof(fas_stream));
        memset(c_stream, 0x5a, sizeof(c_stream));
        if (mc_chacha20_block(fas_block, key, nonce, counter) != MC_CHACHA20_OK ||
            c_mc_chacha20_block(c_block, key, nonce, counter) != MC_CHACHA20_OK ||
            memcmp(fas_block, c_block, sizeof(fas_block)) != 0) {
            return fail("random block cross-check mismatch");
        }
        if (mc_chacha20_xor(fas_out, input, length, key, nonce, counter) != MC_CHACHA20_OK ||
            c_mc_chacha20_xor(c_out, input, length, key, nonce, counter) != MC_CHACHA20_OK ||
            memcmp(fas_out, c_out, sizeof(fas_out)) != 0) {
            return fail("random XOR cross-check mismatch");
        }
        if (mc_chacha20_keystream(fas_stream, length, key, nonce, counter) != MC_CHACHA20_OK ||
            c_mc_chacha20_keystream(c_stream, length, key, nonce, counter) != MC_CHACHA20_OK ||
            memcmp(fas_stream, c_stream, sizeof(fas_stream)) != 0) {
            return fail("random keystream cross-check mismatch");
        }
    }
    return 0;
}

static int check_errors_and_overflow(void)
{
    uint8_t key[32] = {0};
    uint8_t nonce[12] = {0};
    uint8_t out[65] = {0};
    uint8_t c_out[65] = {0};
    uint8_t input[65] = {0};
    uint8_t update_out[65] = {0};
    uint8_t c_update_out[65] = {0};
    mc_chacha20_ctx fas_ctx;
    mc_chacha20_ctx c_ctx;

    if (mc_chacha20_block(NULL, key, nonce, 0u) != MC_CHACHA20_ERR_NULL ||
        mc_chacha20_block(out, NULL, nonce, 0u) != MC_CHACHA20_ERR_NULL ||
        mc_chacha20_block(out, key, NULL, 0u) != MC_CHACHA20_ERR_NULL ||
        mc_chacha20_init(NULL, key, nonce, 0u) != MC_CHACHA20_ERR_NULL ||
        mc_chacha20_init(&fas_ctx, NULL, nonce, 0u) != MC_CHACHA20_ERR_NULL ||
        mc_chacha20_init(&fas_ctx, key, NULL, 0u) != MC_CHACHA20_ERR_NULL ||
        mc_chacha20_update(NULL, out, input, 1u) != MC_CHACHA20_ERR_NULL ||
        mc_chacha20_update(&fas_ctx, NULL, input, 1u) != MC_CHACHA20_ERR_NULL ||
        mc_chacha20_update(&fas_ctx, out, NULL, 1u) != MC_CHACHA20_ERR_NULL ||
        mc_chacha20_keystream_update(NULL, out, 1u) != MC_CHACHA20_ERR_NULL ||
        mc_chacha20_keystream_update(&fas_ctx, NULL, 1u) != MC_CHACHA20_ERR_NULL ||
        mc_chacha20_xor(NULL, input, 1u, key, nonce, 0u) != MC_CHACHA20_ERR_NULL ||
        mc_chacha20_xor(out, NULL, 1u, key, nonce, 0u) != MC_CHACHA20_ERR_NULL ||
        mc_chacha20_xor(out, input, 1u, NULL, nonce, 0u) != MC_CHACHA20_ERR_NULL ||
        mc_chacha20_xor(out, input, 1u, key, NULL, 0u) != MC_CHACHA20_ERR_NULL ||
        mc_chacha20_keystream(NULL, 1u, key, nonce, 0u) != MC_CHACHA20_ERR_NULL ||
        mc_chacha20_keystream(out, 1u, NULL, nonce, 0u) != MC_CHACHA20_ERR_NULL ||
        mc_chacha20_keystream(out, 1u, key, NULL, 0u) != MC_CHACHA20_ERR_NULL) {
        return fail("null argument did not return ERR_NULL");
    }
    if (c_mc_chacha20_block(NULL, key, nonce, 0u) != MC_CHACHA20_ERR_NULL ||
        c_mc_chacha20_block(out, NULL, nonce, 0u) != MC_CHACHA20_ERR_NULL ||
        c_mc_chacha20_block(out, key, NULL, 0u) != MC_CHACHA20_ERR_NULL ||
        c_mc_chacha20_init(NULL, key, nonce, 0u) != MC_CHACHA20_ERR_NULL ||
        c_mc_chacha20_init(&c_ctx, NULL, nonce, 0u) != MC_CHACHA20_ERR_NULL ||
        c_mc_chacha20_init(&c_ctx, key, NULL, 0u) != MC_CHACHA20_ERR_NULL ||
        c_mc_chacha20_update(NULL, out, input, 1u) != MC_CHACHA20_ERR_NULL ||
        c_mc_chacha20_update(&c_ctx, NULL, input, 1u) != MC_CHACHA20_ERR_NULL ||
        c_mc_chacha20_update(&c_ctx, out, NULL, 1u) != MC_CHACHA20_ERR_NULL ||
        c_mc_chacha20_keystream_update(NULL, out, 1u) != MC_CHACHA20_ERR_NULL ||
        c_mc_chacha20_keystream_update(&c_ctx, NULL, 1u) != MC_CHACHA20_ERR_NULL ||
        c_mc_chacha20_xor(NULL, input, 1u, key, nonce, 0u) != MC_CHACHA20_ERR_NULL ||
        c_mc_chacha20_xor(out, NULL, 1u, key, nonce, 0u) != MC_CHACHA20_ERR_NULL ||
        c_mc_chacha20_xor(out, input, 1u, NULL, nonce, 0u) != MC_CHACHA20_ERR_NULL ||
        c_mc_chacha20_xor(out, input, 1u, key, NULL, 0u) != MC_CHACHA20_ERR_NULL ||
        c_mc_chacha20_keystream(NULL, 1u, key, nonce, 0u) != MC_CHACHA20_ERR_NULL ||
        c_mc_chacha20_keystream(out, 1u, NULL, nonce, 0u) != MC_CHACHA20_ERR_NULL ||
        c_mc_chacha20_keystream(out, 1u, key, NULL, 0u) != MC_CHACHA20_ERR_NULL) {
        return fail("C reference null argument did not return ERR_NULL");
    }

    mc_chacha20_wipe_ctx(NULL);
    c_mc_chacha20_wipe_ctx(NULL);
    if (mc_chacha20_init(&fas_ctx, key, nonce, 0u) != MC_CHACHA20_OK ||
        c_mc_chacha20_init(&c_ctx, key, nonce, 0u) != MC_CHACHA20_OK ||
        mc_chacha20_update(&fas_ctx, NULL, NULL, 0u) != MC_CHACHA20_OK ||
        c_mc_chacha20_update(&c_ctx, NULL, NULL, 0u) != MC_CHACHA20_OK ||
        mc_chacha20_keystream_update(&fas_ctx, NULL, 0u) != MC_CHACHA20_OK ||
        c_mc_chacha20_keystream_update(&c_ctx, NULL, 0u) != MC_CHACHA20_OK ||
        mc_chacha20_xor(NULL, NULL, 0u, key, nonce, 0u) != MC_CHACHA20_OK ||
        c_mc_chacha20_xor(NULL, NULL, 0u, key, nonce, 0u) != MC_CHACHA20_OK ||
        mc_chacha20_keystream(NULL, 0u, key, nonce, 0u) != MC_CHACHA20_OK ||
        c_mc_chacha20_keystream(NULL, 0u, key, nonce, 0u) != MC_CHACHA20_OK) {
        return fail("length-zero null-buffer behavior mismatch");
    }

    if (mc_chacha20_init(&fas_ctx, key, nonce, UINT32_MAX) != MC_CHACHA20_OK ||
        c_mc_chacha20_init(&c_ctx, key, nonce, UINT32_MAX) != MC_CHACHA20_OK ||
        mc_chacha20_update(&fas_ctx, update_out, input, sizeof(update_out)) != MC_CHACHA20_ERR_COUNTER_OVERFLOW ||
        c_mc_chacha20_update(&c_ctx, c_update_out, input, sizeof(c_update_out)) != MC_CHACHA20_ERR_COUNTER_OVERFLOW ||
        memcmp(update_out, c_update_out, 64u) != 0) {
        return fail("streaming counter overflow mismatch");
    }
    if (mc_chacha20_xor(out, input, sizeof(out), key, nonce, UINT32_MAX) != MC_CHACHA20_ERR_COUNTER_OVERFLOW ||
        c_mc_chacha20_xor(c_out, input, sizeof(c_out), key, nonce, UINT32_MAX) != MC_CHACHA20_ERR_COUNTER_OVERFLOW ||
        memcmp(out, c_out, sizeof(out)) != 0) {
        return fail("one-shot counter overflow mismatch");
    }
    return 0;
}

static uint64_t read_cycles(void)
{
    unsigned int aux;
    _mm_lfence();
    return __rdtscp(&aux);
}

static int compare_u64(const void *left, const void *right)
{
    uint64_t a = *(const uint64_t *)left;
    uint64_t b = *(const uint64_t *)right;
    return (a > b) - (a < b);
}

static double measure_one(int use_fas, uint8_t *out, const uint8_t *input, const uint8_t key[32], const uint8_t nonce[12])
{
    uint64_t samples[7];
    size_t sample;
    const size_t length = 65536u;
    const size_t repetitions = 128u;

    for (sample = 0; sample < 7u; sample++) {
        uint64_t start = read_cycles();
        uint64_t end;
        size_t repetition;
        for (repetition = 0; repetition < repetitions; repetition++) {
            int rc = use_fas ? mc_chacha20_xor(out, input, length, key, nonce, 7u)
                             : c_mc_chacha20_xor(out, input, length, key, nonce, 7u);
            if (rc != MC_CHACHA20_OK) {
                return -1.0;
            }
            bench_sink ^= out[(repetition * 131u) & (length - 1u)];
        }
        end = read_cycles();
        samples[sample] = end - start;
    }
    qsort(samples, 7u, sizeof(samples[0]), compare_u64);
    return (double)samples[3] / (double)(length * repetitions);
}

static int run_benchmark(void)
{
    uint8_t key[32];
    uint8_t nonce[12];
    uint8_t *input = (uint8_t *)malloc(65536u);
    uint8_t *out = (uint8_t *)malloc(65536u);
    size_t i;
    double c_cycles;
    double fas_cycles;

    if (input == NULL || out == NULL) {
        free(input);
        free(out);
        return fail("benchmark allocation failed");
    }
    for (i = 0; i < sizeof(key); i++) {
        key[i] = (uint8_t)(i * 7u + 3u);
    }
    for (i = 0; i < sizeof(nonce); i++) {
        nonce[i] = (uint8_t)(i * 11u + 5u);
    }
    for (i = 0; i < 65536u; i++) {
        input[i] = (uint8_t)(i * 13u + 17u);
    }
    c_cycles = measure_one(0, out, input, key, nonce);
    fas_cycles = measure_one(1, out, input, key, nonce);
    free(input);
    free(out);
    if (c_cycles < 0.0 || fas_cycles < 0.0) {
        return fail("benchmark operation failed");
    }
    printf("chacha20 O2 64 KiB median: C %.3f cycles/byte, Fas %.3f cycles/byte, ratio %.3fx\n",
           c_cycles,
           fas_cycles,
           fas_cycles / c_cycles);
    return 0;
}

int main(int argc, char **argv)
{
    if (check_rfc_vectors() != 0 || check_random_cases() != 0 || check_errors_and_overflow() != 0) {
        return 1;
    }
    if (argc == 2 && strcmp(argv[1], "--measure") == 0 && run_benchmark() != 0) {
        return 1;
    }
    if (argc > 2 || (argc == 2 && strcmp(argv[1], "--measure") != 0)) {
        return fail("unknown argument");
    }
    printf("chacha20: RFC vectors, 1000 randomized cases, errors, overflow: ok\n");
    return 0;
}
