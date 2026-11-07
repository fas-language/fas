/* Checks test/fas_fir/c/ubtrap.fir: each case runs in a child process; a
   defined input must return C's value, a UB input must die by SIGILL;
   exit 0 when all agree */
#include <limits.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/prctl.h>
#include <sys/wait.h>
#include <unistd.h>
int ub_add(int, int), ub_usub(int, int), ub_shl(int, int), ub_div(int, int), ub_index(long);
long ub_mul64(long, long), ub_ptradd(void *, long), ub_vla(int);
int ub_deref(void *), ub_f2i(double), ub_bool(void *), ub_switch(int);

static int bad;
/* run call in a child (not dumpable: no core dump per trap): it must
   return want or, when want_trap, die by SIGILL */
#define CASE(desc, call, want, want_trap) do { \
    pid_t p_ = fork(); \
    if (p_ == 0) { prctl(PR_SET_DUMPABLE, 0); long r_ = (long)(call); _exit(r_ == (long)(want) ? 0 : 1); } \
    int st_; waitpid(p_, &st_, 0); \
    int trapped_ = WIFSIGNALED(st_) && WTERMSIG(st_) == SIGILL; \
    int ok_ = want_trap ? trapped_ : (WIFEXITED(st_) && WEXITSTATUS(st_) == 0); \
    if (!ok_) { printf("%s: %s\n", desc, trapped_ ? "trapped" : WIFSIGNALED(st_) ? "other signal" : want_trap ? "returned" : "wrong value"); bad = 1; } \
  } while (0)
#define OK(call, want) CASE(#call, call, want, 0)
#define TRAP(call) CASE(#call, call, 0, 1)

int main(void) {
  OK(ub_add(2, 3), 5); OK(ub_add(INT_MAX, 0), INT_MAX); OK(ub_add(INT_MIN, INT_MAX), -1);
  TRAP(ub_add(INT_MAX, 1)); TRAP(ub_add(INT_MIN, -1));
  OK(ub_mul64(1L << 31, 1L << 31), 1L << 62); OK(ub_mul64(-1, LONG_MAX), -LONG_MAX);
  TRAP(ub_mul64(1L << 32, 1L << 31)); TRAP(ub_mul64(-1, LONG_MIN));
  OK(ub_usub(5, 5), 0); OK(ub_usub(-1, 1), -2); TRAP(ub_usub(0, 1));
  OK(ub_shl(1, 30), 1 << 30); OK(ub_shl(0, 31), 0); OK(ub_shl(3, 0), 3);
  TRAP(ub_shl(1, 32)); TRAP(ub_shl(1, -1)); TRAP(ub_shl(-1, 1)); TRAP(ub_shl(1, 31)); TRAP(ub_shl(3, 30));
  OK(ub_div(7, -2), -3); OK(ub_div(INT_MIN, 1), INT_MIN); OK(ub_div(INT_MIN, 2), INT_MIN / 2);
  TRAP(ub_div(1, 0)); TRAP(ub_div(INT_MIN, -1));
  OK(ub_index(0), 10); OK(ub_index(7), 17); TRAP(ub_index(8)); TRAP(ub_index(-1));
  static int x[2] = {42, 43};
  OK(ub_deref(&x[1]), 43); TRAP(ub_deref(0)); TRAP(ub_deref((char *)x + 1));
  OK(ub_ptradd((void *)0x1000, 16), 0x1010); OK(ub_ptradd((void *)-17L, 16), -1L);
  TRAP(ub_ptradd((void *)-16L, 16)); TRAP(ub_ptradd((void *)0x1000, -0x1000));
  OK(ub_f2i(2147483647.9), INT_MAX); OK(ub_f2i(-2147483648.9), INT_MIN); OK(ub_f2i(-0.5), 0);
  TRAP(ub_f2i(2147483648.0)); TRAP(ub_f2i(-2147483649.0)); TRAP(ub_f2i(0.0 / 0.0)); TRAP(ub_f2i(1e300));
  unsigned char b0 = 0, b1 = 1, b2 = 2, bff = 255;
  OK(ub_bool(&b0), 0); OK(ub_bool(&b1), 1); TRAP(ub_bool(&b2)); TRAP(ub_bool(&bff));
  OK(ub_vla(1), 1); OK(ub_vla(100), 5050); TRAP(ub_vla(0)); TRAP(ub_vla(-3));
  OK(ub_switch(0), 100); OK(ub_switch(2), 300); TRAP(ub_switch(3)); TRAP(ub_switch(-1));
  return bad;
}
