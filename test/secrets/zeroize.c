/* the probe for zeroize.fas: calls a Fas function from inline asm with a
   key, then, before any C code runs, saves every caller-saved register
   and scans the 4 KiB below rsp for a word derived from the key (k + i,
   i < 16). The key: 0x5ec7e75ec7e70000 (no other code makes it). */
#include <stdint.h>
#include <stdio.h>
#include "zeroize.h"

uint64_t work(uint64_t);
uint64_t work_public(uint64_t);

#define KEY 0x5ec7e75ec7e70000ull

static int derived(uint64_t w) { return w >= KEY && w < KEY + 16; }

/* regs: rax rcx rdx rsi rdi r8 r9 r10 r11, then xmm0..15 (two words each) */
static struct {
  uint64_t regs[9 + 32];
  uint64_t stack[512];
} seen;
#define regs seen.regs
#define stack_copy seen.stack

#define PROBE(fn)                                                             \
  __asm__ volatile(                                                           \
      "movabs $0x5ec7e75ec7e70000, %%rdi\n\t"                                \
      "call " #fn "\n\t"                                                      \
      "mov %%rax, 0(%%rbx)\n\t"                                               \
      "mov %%rcx, 8(%%rbx)\n\t"                                               \
      "mov %%rdx, 16(%%rbx)\n\t"                                              \
      "mov %%rsi, 24(%%rbx)\n\t"                                              \
      "mov %%rdi, 32(%%rbx)\n\t"                                              \
      "mov %%r8, 40(%%rbx)\n\t"                                               \
      "mov %%r9, 48(%%rbx)\n\t"                                               \
      "mov %%r10, 56(%%rbx)\n\t"                                              \
      "mov %%r11, 64(%%rbx)\n\t"                                              \
      "movdqu %%xmm0, 72(%%rbx)\n\tmovdqu %%xmm1, 88(%%rbx)\n\t"            \
      "movdqu %%xmm2, 104(%%rbx)\n\tmovdqu %%xmm3, 120(%%rbx)\n\t"          \
      "movdqu %%xmm4, 136(%%rbx)\n\tmovdqu %%xmm5, 152(%%rbx)\n\t"          \
      "movdqu %%xmm6, 168(%%rbx)\n\tmovdqu %%xmm7, 184(%%rbx)\n\t"          \
      "movdqu %%xmm8, 200(%%rbx)\n\tmovdqu %%xmm9, 216(%%rbx)\n\t"          \
      "movdqu %%xmm10, 232(%%rbx)\n\tmovdqu %%xmm11, 248(%%rbx)\n\t"        \
      "movdqu %%xmm12, 264(%%rbx)\n\tmovdqu %%xmm13, 280(%%rbx)\n\t"        \
      "movdqu %%xmm14, 296(%%rbx)\n\tmovdqu %%xmm15, 312(%%rbx)\n\t"        \
      /* copy the 4 KiB below rsp before anything else touches it */         \
      "lea -4096(%%rsp), %%rsi\n\t"                                           \
      "lea 328(%%rbx), %%rdi\n\t"                                             \
      "mov $4096, %%rcx\n\t"                                                  \
      "cld\n\trep movsb\n\t"                                                  \
      :                                                                       \
      : "b"(&seen)                                                            \
      : "rax", "rcx", "rdx", "rsi", "rdi", "r8", "r9", "r10", "r11", "xmm0",  \
        "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7", "xmm8",      \
        "xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15", "memory", "cc")

static int count(const char *what, int report) {
  int n = 0;
  for (int i = 0; i < 9 + 32; i++)
    if (derived(regs[i])) {
      n++;
      if (report) printf("%s: register word %d holds k+%llu\n", what, i, (unsigned long long)(regs[i] - KEY));
    }
  for (int i = 0; i < 512; i++)
    if (derived(stack_copy[i])) {
      n++;
      if (report) printf("%s: stack word rsp-%d holds k+%llu\n", what, 4096 - 8 * i, (unsigned long long)(stack_copy[i] - KEY));
    }
  return n;
}

int probe(void) {
  PROBE(work_public);
  int pub = count("work_public", 0);
  PROBE(work);
  int sec = count("work", 1);
  printf("public leftovers %d, secret leftovers %d\n", pub, sec);
  if (pub == 0) return 2; /* the probe sees nothing: it proves nothing */
  return sec == 0 ? 0 : 1;
}
