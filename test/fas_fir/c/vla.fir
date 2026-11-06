; variable-size alloca (C VLAs, alloca()) and stacksave/stackrestore; vla.c
; checks the results, the alignment, and that C called afterwards still finds
; a 16-byte aligned stack (stack arguments included)
triple "x86_64-unknown-linux-gnu"
declare @c_fill (ptr, i64, i64, i64, i64, i64, i64, i64, f64) -> (i64) global
declare @c_touch (ptr, i64, i64) -> (i64) global

; n i64s on the stack, filled by C, summed here; plus what c_fill returns
func @fir_vla_sum (i64) -> (i64) global {
^e(%n: i64):
  %bytes = mul i64 %n, i64 8
  %p = alloca ptr %bytes, align 8
  %x = call (ptr, i64, i64, i64, i64, i64, i64, i64, f64) -> (i64) @c_fill(%p, %n, i64 1, i64 2, i64 3, i64 4, i64 5, i64 6, f64 0x3ff8000000000000)
  jump ^loop(i64 0, %x)
^loop(%i: i64, %s: i64):
  %done = icmp eq i1 %i, %n
  br %done, ^exit, ^body
^body:
  %q = ptradd ptr %p, %i, 8, 0
  %v = load i64 %q, align 8
  %s1 = add i64 %s, %v
  %i1 = add i64 %i, i64 1
  jump ^loop(%i1, %s1)
^exit:
  ret %s
}

; an odd-size allocation first, then one with align 64: returns its address
; mod 64 (0) plus 1000 * (the second block starts below the first's end ? 0 : 1)
func @fir_vla_align (i64, i64) -> (i64) global {
^e(%pre: i64, %n: i64):
  %a = alloca ptr %pre, align 1
  %b = alloca ptr %n, align 64
  %ai = ptrtoint i64 %a
  %bi = ptrtoint i64 %b
  %m = and i64 %bi, i64 63
  %be = add i64 %bi, %n
  %ok = icmp ule i1 %be, %ai
  %bad = select i64 %ok, i64 0, i64 1000
  %r = add i64 %m, %bad
  ret %r
}

; a VLA per iteration, freed by stackrestore: returns the number of
; iterations whose stack pointer differed from the one before the loop, or
; whose C call saw the wrong value (0 when correct; without the restore,
; iters * n * 8 bytes would overflow the stack)
func @fir_vla_loop (i64, i64) -> (i64) global {
^e(%iters: i64, %n: i64):
  %s0 = stacksave
  %s0i = ptrtoint i64 %s0
  jump ^loop(i64 0, i64 0)
^loop(%i: i64, %bad: i64):
  %done = icmp eq i1 %i, %iters
  br %done, ^exit, ^body
^body:
  %s = stacksave
  %si = ptrtoint i64 %s
  %moved = icmp ne i1 %si, %s0i
  %bytes = mul i64 %n, i64 8
  %p = alloca ptr %bytes, align 16
  %last = sub i64 %n, i64 1
  %q = ptradd ptr %p, %last, 8, 0
  store %i, %q, align 8
  %seen = call (ptr, i64, i64) -> (i64) @c_touch(%p, %n, %i)
  %wrong = icmp eq i1 %seen, i64 0
  %either = or i1 %moved, %wrong
  %inc = zext i64 %either
  %bad1 = add i64 %bad, %inc
  stackrestore %s
  %i1 = add i64 %i, i64 1
  jump ^loop(%i1, %bad1)
^exit:
  ret %bad
}

; a zero-size VLA is legal: its pointer is just not dereferenced
func @fir_vla_zero () -> (i64) global {
^e:
  %s = stacksave
  %p = alloca ptr i64 0, align 8
  stackrestore %s
  ret i64 7
}
