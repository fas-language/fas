; Call frame information (.eh_frame) of the machine-IR backend: unwind.c
; calls @fir_f1, which calls @fir_f2, which calls back into C; the callback
; walks the stack with the libgcc unwinder and checks each frame.
; @fir_f2 returns early on one path, laid out before the call, so the call
; site needs the body's rules back after an epilogue (remember/restore
; state). Values live across the calls take callee-saved registers, and
; @fir_f1 moves rsp with a run-time alloca, which the rbp-based CFA ignores.
; llvm: skip checks the native backend's frames (LLVM inlines @fir_f2)
triple "x86_64-unknown-linux-gnu"
declare @fir_cb (i64) -> (i64) global

func @fir_f2 (i64, i64) -> (i64) global {
^e(%a: i64, %b: i64):
  %neg = icmp slt i1 %a, i64 0
  br %neg, ^early(), ^late()
^early():
  ret i64 -1
^late():
  %x = mul i64 %a, i64 3
  %y = add i64 %b, i64 7
  %z = xor i64 %a, %b
  %w = sub i64 %b, %a
  %r = call (i64) -> (i64) @fir_cb(%x)
  %s1 = add i64 %r, %x
  %s2 = add i64 %s1, %y
  %s3 = add i64 %s2, %z
  %s4 = add i64 %s3, %w
  ret %s4
}

func @fir_f1 (i64) -> (i64) global {
^e(%n: i64):
  %buf = alloca ptr %n, align 16
  store %n, %buf, align 8
  %u = add i64 %n, i64 1
  %v = mul i64 %n, i64 5
  %c = call (i64, i64) -> (i64) @fir_f2(%u, %v)
  %back = load i64 %buf, align 8
  %t1 = add i64 %c, %u
  %t2 = add i64 %t1, %v
  %t3 = add i64 %t2, %back
  ret %t3
}
