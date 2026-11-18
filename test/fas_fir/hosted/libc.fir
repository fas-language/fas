; fas-fir --hosted: @main is called by libc's startup and calls the C
; library (printf is variadic; calloc/free from libc.a; stdout is flushed by
; libc's exit after @main returns, so the output appears only on the C runtime)
; exit: 7
triple "x86_64-unknown-linux-gnu"
declare @printf (ptr, ...) -> (i32) global
declare @puts (ptr) -> (i32) global
declare @calloc (i64, i64) -> (ptr) global
declare @free (ptr) -> () global
global @fmt size 12 align 1 local readonly = {bytes "sum %ld %s\0a\00"}
global @word size 3 align 1 local readonly = {bytes "ok\00"}
global @done size 5 align 1 local readonly = {bytes "done\00"}
func @main (i32, ptr) -> (i32) global {
^e(%argc: i32, %argv: ptr):
  %p = call (i64, i64) -> (ptr) @calloc(i64 4, i64 8)
  %q = ptradd ptr %p, i64 0, 1, 16
  store i64 40, %q, align 8
  %z = load i64 %p, align 8
  %v = load i64 %q, align 8
  %s = add i64 %z, %v
  %n = call (ptr, ...) -> (i32) @printf(ptr @fmt, %s, ptr @word)
  call (ptr) -> () @free(%p)
  %m = call (ptr) -> (i32) @puts(ptr @done)
  ret i32 7
}
