; Promises on declarations (SPEC §5): memory(none), memory(read),
; memory(argmem), willreturn. Each kept promise lets nothing change; the
; functions are defined in promises.c, which checks fir_run's result.
triple "x86_64-unknown-linux-gnu"

declare @c_sq (i64) -> (i64) global memory(none) willreturn
declare @c_sum (ptr, i64) -> (i64) global memory(read)
declare @c_bump (ptr) -> () global memory(argmem) willreturn

global @data size 24 align 8 local = {bytes "\01\00\00\00\00\00\00\00\02\00\00\00\00\00\00\00\03\00\00\00\00\00\00\00"}

; sq(5) + sum(data) before and after bump(&data[1]):
; 25 + 6 + (6 + 1) = 38
func @fir_run () -> (i64) global {
^e:
  %a = call (i64) -> (i64) @c_sq(i64 5)
  %b = call (ptr, i64) -> (i64) @c_sum(ptr @data, i64 3)
  %p = ptradd ptr ptr @data, i64 1, 8, 0
  call (ptr) -> () @c_bump(%p)
  %c = call (ptr, i64) -> (i64) @c_sum(ptr @data, i64 3)
  %s1 = add i64 %a, %b
  %s2 = add i64 %s1, %c
  ret %s2
}
