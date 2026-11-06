/* Unwinds through test/fas_fir/c/unwind.fir with the libgcc unwinder:
   from the callback the frames must be fir_cb, fir_f2, fir_f1 and main,
   each found by its FDE (_Unwind_FindEnclosingFunction gives the FDE's
   pc_begin), and in the Fir frames the restored rbp must be the frame's
   CFA - 16 (rbp-based frames). libgcc's _Unwind_GetCFA gives the CFA of
   the callee, i.e. this frame's rsp, so a frame's CFA is read in the next
   context. Exit 0 when all holds. */
#include <stdio.h>
#include <unwind.h>

long fir_f1(long n);
long fir_f2(long a, long b);
long fir_cb(long x);

static void *want[] = {(void *)fir_cb, (void *)fir_f2, (void *)fir_f1, (void *)0 /* main */};
static int frames, bad;
static unsigned long prev_rbp;

static _Unwind_Reason_Code step(struct _Unwind_Context *ctx, void *arg) {
  (void)arg;
  void *ip = (void *)_Unwind_GetIP(ctx);
  void *fn = _Unwind_FindEnclosingFunction((char *)ip - 1);
  if (frames < 4) {
    if (fn != want[frames]) {
      printf("frame %d: ip %p in %p, expected %p\n", frames, ip, fn, want[frames]);
      bad = 1;
    }
    /* frames 1 and 2 are fir_f2 and fir_f1 */
    if ((frames == 2 || frames == 3) && prev_rbp + 16 != (unsigned long)_Unwind_GetCFA(ctx)) {
      printf("frame %d: rbp %#lx, cfa %#lx\n", frames - 1, prev_rbp, (unsigned long)_Unwind_GetCFA(ctx));
      bad = 1;
    }
    prev_rbp = _Unwind_GetGR(ctx, 6);
  }
  frames++;
  return _URC_NO_REASON;
}

long fir_cb(long x) {
  _Unwind_Backtrace(step, 0);
  return x + 1;
}

int main(void) {
  want[3] = (void *)main;
  /* f1(4): u = 5, v = 20, f2(5, 20): x = 15, cb -> 16,
     s4 = 16 + 15 + 27 + (5 ^ 20 = 17) + 15 = 90; f1: 90 + 5 + 20 + 4 = 119 */
  long r = fir_f1(4);
  if (r != 119) { printf("fir_f1(4) = %ld, expected 119\n", r); bad = 1; }
  if (fir_f2(-1, 0) != -1) { printf("fir_f2 early return\n"); bad = 1; }
  if (frames < 4) { printf("only %d frames\n", frames); bad = 1; }
  return bad;
}
