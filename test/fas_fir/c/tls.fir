; Thread-local globals and a named section in a position-independent
; object (initial-exec TLS through the GOT), checked by tls.c from several
; threads: each thread sees its own copy of the Fir and C thread-locals.
triple "x86_64-unknown-linux-gnu"

; .tdata and .tbss, one exported to C
global @fir_tls size 8 align 8 global thread_local = {bytes "\07\00\00\00\00\00\00\00"}
global @fir_scratch size 32 align 16 local thread_local = zero
alias @fir_tls_alias global = @fir_tls
; defined in C: `__thread long c_tls = 100;`
global @c_tls size 8 align 8 global thread_local = extern

; thread_local in named sections (SPEC §4): a TLS template section with
; bytes and one with zeros only
global @fir_sec_tls size 8 align 8 global thread_local section ".tdata.fir_sec" = {bytes "\03\00\00\00\00\00\00\00"}
global @fir_sec_zero size 16 align 16 global thread_local section ".tbss.fir_sec" = zero

; C walks the registry between __start_fir_reg and __stop_fir_reg
global @fir_reg_a size 8 align 8 global section "fir_reg" = {bytes "\01\00\00\00\00\00\00\00"}
global @fir_reg_b size 8 align 8 local section "fir_reg" readonly = {bytes "\02\00\00\00\00\00\00\00"}

; fir_tls += by; returns the new value
func @fir_bump (i64) -> (i64) global {
^e(%by: i64):
  %v = load i64 ptr @fir_tls, align 8
  %w = add i64 %v, %by
  store %w, ptr @fir_tls, align 8
  ret %w
}

func @fir_addr () -> (ptr) global {
^e:
  ret ptr @fir_tls
}

; c_tls += by through the scratch slot in .tbss; returns c_tls
func @fir_add_c (i64) -> (i64) global {
^e(%by: i64):
  %s = ptradd ptr ptr @fir_scratch, i64 0, 1, 16
  %old = load i64 %s, align 8
  %v = load i64 ptr @c_tls, align 8
  %w = add i64 %v, %by
  %w2 = add i64 %w, %old
  store %w2, ptr @c_tls, align 8
  store i64 0, %s, align 8
  ret %w2
}

; fir_sec_tls += fir_sec_zero[1] + by; fir_sec_zero[1] = by; returns fir_sec_tls
func @fir_sec_bump (i64) -> (i64) global {
^e(%by: i64):
  %z = ptradd ptr ptr @fir_sec_zero, i64 0, 1, 8
  %old = load i64 %z, align 8
  %v = load i64 ptr @fir_sec_tls, align 8
  %w = add i64 %v, %old
  %w2 = add i64 %w, %by
  store %w2, ptr @fir_sec_tls, align 8
  store %by, %z, align 8
  ret %w2
}
