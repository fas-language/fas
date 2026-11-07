; shared: yes
; General-dynamic TLS: built with -c -fPIC into a shared library, which
; tlsgd.c loads with dlopen after threads exist. The 1 MiB .tbss array is
; far past glibc's spare static TLS, so only __tls_get_addr can reach it;
; initial-exec code would make dlopen fail.
triple "x86_64-unknown-linux-gnu"

global @gd_tls size 8 align 8 global thread_local = {bytes "\07\00\00\00\00\00\00\00"}
global @gd_big size 1048576 align 64 local thread_local = zero
alias @gd_tls_alias global = @gd_tls
; defined in the executable (exported with -rdynamic)
global @c_tls size 8 align 8 global thread_local = extern

; gd_tls += by; returns the new value
func @gd_bump (i64) -> (i64) global {
^e(%by: i64):
  %v = load i64 ptr @gd_tls, align 8
  %w = add i64 %v, %by
  store %w, ptr @gd_tls, align 8
  ret %w
}

func @gd_addr () -> (ptr) global {
^e:
  ret ptr @gd_tls_alias
}

; writes k into gd_big[i] for i = 0, 4096, 8192, ... and sums them back,
; with an address formed from an addend past the first page
func @gd_big_sum (i64) -> (i64) global {
^e(%k: i64):
  jump ^w(i64 0)
^w(%i: i64):
  %done = icmp uge i1 %i, i64 131072
  br %done, ^r0, ^wb
^wb:
  %p = ptradd ptr ptr @gd_big, %i, 8, 0
  %ki = add i64 %k, %i
  store %ki, %p, align 8
  %i2 = add i64 %i, i64 4096
  jump ^w(%i2)
^r0:
  %last = load i64 ptr @gd_big+1015808, align 8
  jump ^r(i64 0, %last)
^r(%j: i64, %acc: i64):
  %rd = icmp uge i1 %j, i64 131072
  br %rd, ^out, ^rb
^rb:
  %q = ptradd ptr ptr @gd_big, %j, 8, 0
  %x = load i64 %q, align 8
  %acc2 = add i64 %acc, %x
  %j2 = add i64 %j, i64 4096
  jump ^r(%j2, %acc2)
^out:
  ret %acc
}

; c_tls += by, across a call (the TLS address is computed by calls of
; its own, which must not disturb values live across them)
func @gd_add_c (i64) -> (i64) global {
^e(%by: i64):
  %a = mul i64 %by, i64 3
  %v = load i64 ptr @c_tls, align 8
  %t = load i64 ptr @gd_tls, align 8
  %w = add i64 %v, %by
  store %w, ptr @c_tls, align 8
  %w2 = add i64 %w, %a
  %w3 = sub i64 %w2, %a
  %z = sub i64 %t, %t
  %w4 = add i64 %w3, %z
  ret %w4
}
