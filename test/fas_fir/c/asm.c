/* Checks test/fas_fir/c/asm.fir: exit 0 when every function agrees */
#include <stdio.h>
#include <stdint.h>
long asm_add(long, long);
long asm_mul(long, long);
long asm_mulhi(long, long);
void asm_store(long *, long);
long asm_load(long *);
int asm_iszero(long);
unsigned char asm_below(long, long);
int asm_lowbyte(long);
long asm_lea(long, long);
long asm_sub(long, long);
long asm_odd(long);
double asm_fadd(double, double);
long asm_clobber(long);
long asm_explicit(long, long);
long asm_cell(long);
uint32_t asm_rol(uint32_t);
static int bad;
#define CHECK(e, want) do { long g_ = (long)(e); if (g_ != (long)(want)) { \
    printf("%s = %ld, expected %ld\n", #e, g_, (long)(want)); bad = 1; } } while (0)
int main(void) {
  CHECK(asm_add(40, 2), 42);
  CHECK(asm_mul(6, 7), 42);
  CHECK(asm_mulhi(1L << 40, 1L << 40), 1L << 16);
  long cell = 0;
  asm_store(&cell, 37);
  CHECK(cell, 42);
  CHECK(asm_load(&cell), 42);
  CHECK(asm_iszero(0), 1);
  CHECK(asm_iszero(5), 0);
  CHECK(asm_below(1, 2), 1);
  CHECK(asm_below(2, 1), 0);
  CHECK(asm_below(-1, 1), 0);
  CHECK(asm_lowbyte(0x1234), 0x34);
  CHECK(asm_lea(2, 10), 42);
  CHECK(asm_sub(50, 8), 42);
  CHECK(asm_odd(3), 1);
  CHECK(asm_odd(4), 0);
  CHECK(asm_fadd(40.5, 1.5) == 42.0, 1);
  long r12_before, rbx_before, r12_after, rbx_after;
  __asm__ volatile("movq %%r12, %0; movq %%rbx, %1" : "=r"(r12_before), "=r"(rbx_before));
  CHECK(asm_clobber(42), 42);
  __asm__ volatile("movq %%r12, %0; movq %%rbx, %1" : "=r"(r12_after), "=r"(rbx_after));
  CHECK(r12_after, r12_before);
  CHECK(rbx_after, rbx_before);
  CHECK(asm_explicit(40, 2), 42);
  CHECK(asm_cell(37), 42);
  CHECK(asm_rol(0x11223344u), 0x22334411u);
  return bad;
}
