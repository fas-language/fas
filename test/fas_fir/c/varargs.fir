; exported variadic function called from C, and a C va_list read by va_arg
triple "x86_64-unknown-linux-gnu"
; sum over n (i64, f64) pairs read through the given va_list
func @fir_vlist (i32, ptr) -> (i64) global {
^e(%n: i32, %ap: ptr):
  jump ^loop(i32 0, i64 0)
^loop(%i: i32, %acc: i64):
  %done = icmp eq i1 %i, %n
  br %done, ^exit, ^body
^body:
  %x = va_arg i64 %ap
  %f = va_arg f64 %ap
  %fi = fptosi i64 %f
  %t = mul i64 %acc, i64 7
  %t1 = add i64 %t, %x
  %t2 = sub i64 %t1, %fi
  %i2 = add i32 %i, i32 1
  jump ^loop(%i2, %t2)
^exit:
  ret %acc
}
func @fir_vsum (i32, ...) -> (i64) global {
^e(%n: i32):
  %ap = alloca ptr i64 24, align 8
  va_start %ap
  %r = call (i32, ptr) -> (i64) @fir_vlist(%n, %ap)
  ret %r
}
