/* Checks test/fas_fir/c/trapdiv.fir: each case runs in a child process; a
   defined input must return C's value, a trapping input must die by a
   signal (any: the signal of a trap is unspecified); exit 0 when all
   agree */
#include <limits.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/prctl.h>
#include <sys/wait.h>
#include <unistd.h>
typedef __int128 i128;
typedef unsigned __int128 u128;
int8_t sd8(int8_t, int8_t);
int16_t sd16(int16_t, int16_t);
int sd32(int, int), sr32(int, int), sd24(int, int), dead(int, int), kzero(int, int), kseven(int);
long sd64(long, long), sr64(long, long);
unsigned ud32(unsigned, unsigned);
unsigned long ur64(unsigned long, unsigned long);
i128 sd128(i128, i128);
u128 ud128(u128, u128);
void sdv(const int *, const int *, int *);

static int bad;
/* run call in a child (not dumpable: no core dump per trap): it must
   return want or, when want_trap, die by a signal */
#define CASE(desc, call, want, want_trap) do { \
    pid_t p_ = fork(); \
    if (p_ == 0) { prctl(PR_SET_DUMPABLE, 0); _exit((call) == (want) ? 0 : 1); } \
    int st_; waitpid(p_, &st_, 0); \
    int trapped_ = WIFSIGNALED(st_); \
    int ok_ = want_trap ? trapped_ : (WIFEXITED(st_) && WEXITSTATUS(st_) == 0); \
    if (!ok_) { printf("%s: %s\n", desc, trapped_ ? "trapped" : want_trap ? "returned" : "wrong value"); bad = 1; } \
  } while (0)
#define OK(call, want) CASE(#call, call, want, 0)
#define TRAP(call) CASE(#call, call, 0, 1)

static int vec(int a3, int b3) {
  int a[4] = {10, -9, 8, a3}, b[4] = {3, 2, -4, b3}, r[4];
  sdv(a, b, r);
  return r[0] == 3 && r[1] == -4 && r[2] == -2 && r[3] == a3 / b3;
}

int main(void) {
  OK(sd8(-7, 2), -3); OK(sd8(INT8_MIN, 1), INT8_MIN); OK(sd8(INT8_MAX, -1), -INT8_MAX);
  TRAP(sd8(1, 0)); TRAP(sd8(INT8_MIN, -1));
  OK(sd16(-7, 2), -3); TRAP(sd16(1, 0)); TRAP(sd16(INT16_MIN, -1));
  OK(sd32(7, -2), -3); OK(sd32(INT_MIN, 2), INT_MIN / 2); TRAP(sd32(1, 0)); TRAP(sd32(INT_MIN, -1));
  OK(sr32(-7, 2), -1); OK(sr32(INT_MIN, 3), INT_MIN % 3); TRAP(sr32(1, 0)); TRAP(sr32(INT_MIN, -1));
  OK(sd64(LONG_MIN, 2), LONG_MIN / 2); TRAP(sd64(1, 0)); TRAP(sd64(LONG_MIN, -1));
  OK(sr64(-7, 2), -1); TRAP(sr64(1, 0)); TRAP(sr64(LONG_MIN, -1));
  OK(ud32(UINT_MAX, 2), UINT_MAX / 2); TRAP(ud32(1, 0));
  OK(ur64(ULONG_MAX, 10), ULONG_MAX % 10); TRAP(ur64(1, 0));
  OK(sd24(-8388608, 2), -4194304); OK(sd24(8388607, -1), -8388607);
  TRAP(sd24(1, 0)); TRAP(sd24(-8388608, -1));
  /* only the low 24 bits divide: 1 << 24 is 0 there */
  TRAP(sd24(5, 1 << 24));
  i128 min128 = (i128)((u128)1 << 127);
  OK(sd128(min128, 2), min128 / 2); OK(sd128(-7, 2), -3);
  TRAP(sd128(1, 0)); TRAP(sd128(min128, -1));
  OK(ud128(~(u128)0, 3), ~(u128)0 / 3); TRAP(ud128(1, 0)); TRAP(ud128(1, (u128)0));
  OK(vec(7, 2), 1); OK(vec(INT_MIN, 1), 1); TRAP(vec(1, 0)); TRAP(vec(INT_MIN, -1));
  OK(dead(6, 3), 7); TRAP(dead(1, 0)); TRAP(dead(INT_MIN, -1));
  OK(kzero(9, 0), 5); TRAP(kzero(9, 1));
  OK(kseven(14), -2); OK(kseven(INT_MIN), INT_MIN / -7);
  return bad;
}
