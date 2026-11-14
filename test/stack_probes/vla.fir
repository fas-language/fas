; A dynamic stack allocation (Salloc) of a caller-chosen size for
; test/native_stack_probes.sh: writes and reads the lowest word, so without
; probes rsp jumps straight past a guard page. @aligned passes a
; 4 KiB-aligned agg by value, so the call aligns rsp to a page
; (Align_sp, the largest alignment the backend takes); it returns 9.
triple "x86_64-unknown-linux-gnu"

func @vla (i64) -> (i64) global {
^e(%n: i64):
  %a = alloca ptr %n, align 16
  store i64 7, %a, align 8
  %w = load i64 %a, align 8
  ret %w
}

func @take (agg(4096, 4096, [0: i64, 4088: i64])) -> (i64) local noinline {
^e(%p: ptr):
  %a = load i64 %p, align 8
  %q = ptradd ptr %p, i64 0, 1, 4088
  %b = load i64 %q, align 8
  %r = add i64 %a, %b
  ret %r
}

func @aligned () -> (i64) global {
^e:
  %s = alloca ptr i64 4096, align 4096
  store i64 4, %s, align 8
  %q = ptradd ptr %s, i64 0, 1, 4088
  store i64 5, %q, align 8
  %r = call (agg(4096, 4096, [0: i64, 4088: i64])) -> (i64) @take(%s)
  ret %r
}
