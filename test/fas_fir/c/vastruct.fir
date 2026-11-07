; Structs by value through `...` (SysV 3.5.7), both directions: Fir reads them
; with `va_arg agg(...)` and passes them with `agg(...) %p` call arguments.
; nl leading longs and nd leading doubles shift the eightbytes so that some
; structs no longer fit the remaining registers and go to the overflow area
; whole while later, smaller ones still come from registers. MEMORY class
; (24 bytes) always goes through the overflow area. Checked by vastruct.c.
triple "x86_64-unknown-linux-gnu"
declare @c_vr (i32, i32, ...) -> (i64) global
global @g_ii size 16 align 8 local readonly = {bytes "\e9\03\00\00\00\00\00\00\ea\03\00\00\00\00\00\00"}
global @g_dd size 16 align 8 local readonly = {bytes "\00\00\00\00\00\00\04\40\00\00\00\00\00\00\0a\c0"}
global @g_id size 16 align 8 local readonly = {bytes "\eb\03\00\00\00\00\00\00\00\00\00\00\00\00\13\40"}
global @g_ffi size 12 align 4 local readonly = {bytes "\00\00\b0\40\00\00\d0\c0\ec\03\00\00"}
global @g_big size 24 align 8 local readonly = {bytes "\ed\03\00\00\00\00\00\00\ee\03\00\00\00\00\00\00\ef\03\00\00\00\00\00\00"}
global @g_s1 size 4 align 4 local readonly = {bytes "\f0\03\00\00"}
global @g_dd2 size 16 align 8 local readonly = {bytes "\00\00\00\00\00\00\1d\40\00\00\00\00\00\00\21\40"}
global @g_ii2 size 16 align 8 local readonly = {bytes "\f1\03\00\00\00\00\00\00\f2\03\00\00\00\00\00\00"}

; h = h*31 + v over every value read; floating values as (long)(x*4)
func @fir_vlist (i32, i32, ptr) -> (i64) global {
^e(%nl: i32, %nd: i32, %ap: ptr):
  jump ^ll(i32 0, i64 7)
^ll(%i: i32, %h: i64):
  %ld = icmp eq i1 %i, %nl
  br %ld, ^dl0, ^lb
^lb:
  %x = va_arg i64 %ap
  %lh1 = mul i64 %h, i64 31
  %lh2 = add i64 %lh1, %x
  %i1 = add i32 %i, i32 1
  jump ^ll(%i1, %lh2)
^dl0:
  jump ^dl(i32 0, %h)
^dl(%j: i32, %g: i64):
  %dd = icmp eq i1 %j, %nd
  br %dd, ^seq, ^db
^db:
  %y = va_arg f64 %ap
  %y4 = fmul f64 %y, f64 0x4010000000000000
  %yi = fptosi i64 %y4
  %dh1 = mul i64 %g, i64 31
  %dh2 = add i64 %dh1, %yi
  %j1 = add i32 %j, i32 1
  jump ^dl(%j1, %dh2)
^seq:
  %p1 = va_arg agg(16, 8, [0: i64, 8: i64]) %ap
  %v2 = load i64 %p1, align 8
  %t3 = mul i64 %g, i64 31
  %h4 = add i64 %t3, %v2
  %q5 = ptradd ptr %p1, i64 0, 1, 8
  %v6 = load i64 %q5, align 8
  %t7 = mul i64 %h4, i64 31
  %h8 = add i64 %t7, %v6
  %p9 = va_arg agg(16, 8, [0: f64, 8: f64]) %ap
  %v10 = load f64 %p9, align 8
  %m11 = fmul f64 %v10, f64 0x4010000000000000
  %c12 = fptosi i64 %m11
  %t13 = mul i64 %h8, i64 31
  %h14 = add i64 %t13, %c12
  %q15 = ptradd ptr %p9, i64 0, 1, 8
  %v16 = load f64 %q15, align 8
  %m17 = fmul f64 %v16, f64 0x4010000000000000
  %c18 = fptosi i64 %m17
  %t19 = mul i64 %h14, i64 31
  %h20 = add i64 %t19, %c18
  %p21 = va_arg agg(16, 8, [0: i64, 8: f64]) %ap
  %v22 = load i64 %p21, align 8
  %t23 = mul i64 %h20, i64 31
  %h24 = add i64 %t23, %v22
  %q25 = ptradd ptr %p21, i64 0, 1, 8
  %v26 = load f64 %q25, align 8
  %m27 = fmul f64 %v26, f64 0x4010000000000000
  %c28 = fptosi i64 %m27
  %t29 = mul i64 %h24, i64 31
  %h30 = add i64 %t29, %c28
  %p31 = va_arg agg(12, 4, [0: f32, 4: f32, 8: i32]) %ap
  %v32 = load f32 %p31, align 4
  %e33 = fpext f64 %v32
  %m34 = fmul f64 %e33, f64 0x4010000000000000
  %c35 = fptosi i64 %m34
  %t36 = mul i64 %h30, i64 31
  %h37 = add i64 %t36, %c35
  %q38 = ptradd ptr %p31, i64 0, 1, 4
  %v39 = load f32 %q38, align 4
  %e40 = fpext f64 %v39
  %m41 = fmul f64 %e40, f64 0x4010000000000000
  %c42 = fptosi i64 %m41
  %t43 = mul i64 %h37, i64 31
  %h44 = add i64 %t43, %c42
  %q45 = ptradd ptr %p31, i64 0, 1, 8
  %v46 = load i32 %q45, align 4
  %e47 = sext i64 %v46
  %t48 = mul i64 %h44, i64 31
  %h49 = add i64 %t48, %e47
  %p50 = va_arg agg(24, 8, [0: i64, 8: i64, 16: i64]) %ap
  %v51 = load i64 %p50, align 8
  %t52 = mul i64 %h49, i64 31
  %h53 = add i64 %t52, %v51
  %q54 = ptradd ptr %p50, i64 0, 1, 8
  %v55 = load i64 %q54, align 8
  %t56 = mul i64 %h53, i64 31
  %h57 = add i64 %t56, %v55
  %q58 = ptradd ptr %p50, i64 0, 1, 16
  %v59 = load i64 %q58, align 8
  %t60 = mul i64 %h57, i64 31
  %h61 = add i64 %t60, %v59
  %p62 = va_arg agg(4, 4, [0: i32]) %ap
  %v63 = load i32 %p62, align 4
  %e64 = sext i64 %v63
  %t65 = mul i64 %h61, i64 31
  %h66 = add i64 %t65, %e64
  %p67 = va_arg agg(16, 8, [0: f64, 8: f64]) %ap
  %v68 = load f64 %p67, align 8
  %m69 = fmul f64 %v68, f64 0x4010000000000000
  %c70 = fptosi i64 %m69
  %t71 = mul i64 %h66, i64 31
  %h72 = add i64 %t71, %c70
  %q73 = ptradd ptr %p67, i64 0, 1, 8
  %v74 = load f64 %q73, align 8
  %m75 = fmul f64 %v74, f64 0x4010000000000000
  %c76 = fptosi i64 %m75
  %t77 = mul i64 %h72, i64 31
  %h78 = add i64 %t77, %c76
  %p79 = va_arg agg(16, 8, [0: i64, 8: i64]) %ap
  %v80 = load i64 %p79, align 8
  %t81 = mul i64 %h78, i64 31
  %h82 = add i64 %t81, %v80
  %q83 = ptradd ptr %p79, i64 0, 1, 8
  %v84 = load i64 %q83, align 8
  %t85 = mul i64 %h82, i64 31
  %h86 = add i64 %t85, %v84
  ret %h86
}
func @fir_vr (i32, i32, ...) -> (i64) global {
^e(%nl: i32, %nd: i32):
  %ap = alloca ptr i64 24, align 8
  va_start %ap
  %r = call (i32, i32, ptr) -> (i64) @fir_vlist(%nl, %nd, %ap)
  ret %r
}
func @fir_drive_0_0 (i32) -> (i64) global {
^e(%to_c: i32):
  %z = icmp ne i1 %to_c, i32 0
  br %z, ^c, ^f
^c:
  %rc = call (i32, i32, ...) -> (i64) @c_vr(i32 0, i32 0, agg(16, 8, [0: i64, 8: i64]) ptr @g_ii, agg(16, 8, [0: f64, 8: f64]) ptr @g_dd, agg(16, 8, [0: i64, 8: f64]) ptr @g_id, agg(12, 4, [0: f32, 4: f32, 8: i32]) ptr @g_ffi, agg(24, 8, [0: i64, 8: i64, 16: i64]) ptr @g_big, agg(4, 4, [0: i32]) ptr @g_s1, agg(16, 8, [0: f64, 8: f64]) ptr @g_dd2, agg(16, 8, [0: i64, 8: i64]) ptr @g_ii2)
  ret %rc
^f:
  %rf = call (i32, i32, ...) -> (i64) @fir_vr(i32 0, i32 0, agg(16, 8, [0: i64, 8: i64]) ptr @g_ii, agg(16, 8, [0: f64, 8: f64]) ptr @g_dd, agg(16, 8, [0: i64, 8: f64]) ptr @g_id, agg(12, 4, [0: f32, 4: f32, 8: i32]) ptr @g_ffi, agg(24, 8, [0: i64, 8: i64, 16: i64]) ptr @g_big, agg(4, 4, [0: i32]) ptr @g_s1, agg(16, 8, [0: f64, 8: f64]) ptr @g_dd2, agg(16, 8, [0: i64, 8: i64]) ptr @g_ii2)
  ret %rf
}
func @fir_drive_3_0 (i32) -> (i64) global {
^e(%to_c: i32):
  %z = icmp ne i1 %to_c, i32 0
  br %z, ^c, ^f
^c:
  %rc = call (i32, i32, ...) -> (i64) @c_vr(i32 3, i32 0, i64 11, i64 22, i64 33, agg(16, 8, [0: i64, 8: i64]) ptr @g_ii, agg(16, 8, [0: f64, 8: f64]) ptr @g_dd, agg(16, 8, [0: i64, 8: f64]) ptr @g_id, agg(12, 4, [0: f32, 4: f32, 8: i32]) ptr @g_ffi, agg(24, 8, [0: i64, 8: i64, 16: i64]) ptr @g_big, agg(4, 4, [0: i32]) ptr @g_s1, agg(16, 8, [0: f64, 8: f64]) ptr @g_dd2, agg(16, 8, [0: i64, 8: i64]) ptr @g_ii2)
  ret %rc
^f:
  %rf = call (i32, i32, ...) -> (i64) @fir_vr(i32 3, i32 0, i64 11, i64 22, i64 33, agg(16, 8, [0: i64, 8: i64]) ptr @g_ii, agg(16, 8, [0: f64, 8: f64]) ptr @g_dd, agg(16, 8, [0: i64, 8: f64]) ptr @g_id, agg(12, 4, [0: f32, 4: f32, 8: i32]) ptr @g_ffi, agg(24, 8, [0: i64, 8: i64, 16: i64]) ptr @g_big, agg(4, 4, [0: i32]) ptr @g_s1, agg(16, 8, [0: f64, 8: f64]) ptr @g_dd2, agg(16, 8, [0: i64, 8: i64]) ptr @g_ii2)
  ret %rf
}
func @fir_drive_4_7 (i32) -> (i64) global {
^e(%to_c: i32):
  %z = icmp ne i1 %to_c, i32 0
  br %z, ^c, ^f
^c:
  %rc = call (i32, i32, ...) -> (i64) @c_vr(i32 4, i32 7, i64 11, i64 22, i64 33, i64 44, f64 0x3fe0000000000000, f64 0x3ff8000000000000, f64 0x4004000000000000, f64 0x400c000000000000, f64 0x4012000000000000, f64 0x4016000000000000, f64 0x401a000000000000, agg(16, 8, [0: i64, 8: i64]) ptr @g_ii, agg(16, 8, [0: f64, 8: f64]) ptr @g_dd, agg(16, 8, [0: i64, 8: f64]) ptr @g_id, agg(12, 4, [0: f32, 4: f32, 8: i32]) ptr @g_ffi, agg(24, 8, [0: i64, 8: i64, 16: i64]) ptr @g_big, agg(4, 4, [0: i32]) ptr @g_s1, agg(16, 8, [0: f64, 8: f64]) ptr @g_dd2, agg(16, 8, [0: i64, 8: i64]) ptr @g_ii2)
  ret %rc
^f:
  %rf = call (i32, i32, ...) -> (i64) @fir_vr(i32 4, i32 7, i64 11, i64 22, i64 33, i64 44, f64 0x3fe0000000000000, f64 0x3ff8000000000000, f64 0x4004000000000000, f64 0x400c000000000000, f64 0x4012000000000000, f64 0x4016000000000000, f64 0x401a000000000000, agg(16, 8, [0: i64, 8: i64]) ptr @g_ii, agg(16, 8, [0: f64, 8: f64]) ptr @g_dd, agg(16, 8, [0: i64, 8: f64]) ptr @g_id, agg(12, 4, [0: f32, 4: f32, 8: i32]) ptr @g_ffi, agg(24, 8, [0: i64, 8: i64, 16: i64]) ptr @g_big, agg(4, 4, [0: i32]) ptr @g_s1, agg(16, 8, [0: f64, 8: f64]) ptr @g_dd2, agg(16, 8, [0: i64, 8: i64]) ptr @g_ii2)
  ret %rf
}
func @fir_drive_0_7 (i32) -> (i64) global {
^e(%to_c: i32):
  %z = icmp ne i1 %to_c, i32 0
  br %z, ^c, ^f
^c:
  %rc = call (i32, i32, ...) -> (i64) @c_vr(i32 0, i32 7, f64 0x3fe0000000000000, f64 0x3ff8000000000000, f64 0x4004000000000000, f64 0x400c000000000000, f64 0x4012000000000000, f64 0x4016000000000000, f64 0x401a000000000000, agg(16, 8, [0: i64, 8: i64]) ptr @g_ii, agg(16, 8, [0: f64, 8: f64]) ptr @g_dd, agg(16, 8, [0: i64, 8: f64]) ptr @g_id, agg(12, 4, [0: f32, 4: f32, 8: i32]) ptr @g_ffi, agg(24, 8, [0: i64, 8: i64, 16: i64]) ptr @g_big, agg(4, 4, [0: i32]) ptr @g_s1, agg(16, 8, [0: f64, 8: f64]) ptr @g_dd2, agg(16, 8, [0: i64, 8: i64]) ptr @g_ii2)
  ret %rc
^f:
  %rf = call (i32, i32, ...) -> (i64) @fir_vr(i32 0, i32 7, f64 0x3fe0000000000000, f64 0x3ff8000000000000, f64 0x4004000000000000, f64 0x400c000000000000, f64 0x4012000000000000, f64 0x4016000000000000, f64 0x401a000000000000, agg(16, 8, [0: i64, 8: i64]) ptr @g_ii, agg(16, 8, [0: f64, 8: f64]) ptr @g_dd, agg(16, 8, [0: i64, 8: f64]) ptr @g_id, agg(12, 4, [0: f32, 4: f32, 8: i32]) ptr @g_ffi, agg(24, 8, [0: i64, 8: i64, 16: i64]) ptr @g_big, agg(4, 4, [0: i32]) ptr @g_s1, agg(16, 8, [0: f64, 8: f64]) ptr @g_dd2, agg(16, 8, [0: i64, 8: i64]) ptr @g_ii2)
  ret %rf
}
func @fir_drive_6_8 (i32) -> (i64) global {
^e(%to_c: i32):
  %z = icmp ne i1 %to_c, i32 0
  br %z, ^c, ^f
^c:
  %rc = call (i32, i32, ...) -> (i64) @c_vr(i32 6, i32 8, i64 11, i64 22, i64 33, i64 44, i64 55, i64 66, f64 0x3fe0000000000000, f64 0x3ff8000000000000, f64 0x4004000000000000, f64 0x400c000000000000, f64 0x4012000000000000, f64 0x4016000000000000, f64 0x401a000000000000, f64 0x401e000000000000, agg(16, 8, [0: i64, 8: i64]) ptr @g_ii, agg(16, 8, [0: f64, 8: f64]) ptr @g_dd, agg(16, 8, [0: i64, 8: f64]) ptr @g_id, agg(12, 4, [0: f32, 4: f32, 8: i32]) ptr @g_ffi, agg(24, 8, [0: i64, 8: i64, 16: i64]) ptr @g_big, agg(4, 4, [0: i32]) ptr @g_s1, agg(16, 8, [0: f64, 8: f64]) ptr @g_dd2, agg(16, 8, [0: i64, 8: i64]) ptr @g_ii2)
  ret %rc
^f:
  %rf = call (i32, i32, ...) -> (i64) @fir_vr(i32 6, i32 8, i64 11, i64 22, i64 33, i64 44, i64 55, i64 66, f64 0x3fe0000000000000, f64 0x3ff8000000000000, f64 0x4004000000000000, f64 0x400c000000000000, f64 0x4012000000000000, f64 0x4016000000000000, f64 0x401a000000000000, f64 0x401e000000000000, agg(16, 8, [0: i64, 8: i64]) ptr @g_ii, agg(16, 8, [0: f64, 8: f64]) ptr @g_dd, agg(16, 8, [0: i64, 8: f64]) ptr @g_id, agg(12, 4, [0: f32, 4: f32, 8: i32]) ptr @g_ffi, agg(24, 8, [0: i64, 8: i64, 16: i64]) ptr @g_big, agg(4, 4, [0: i32]) ptr @g_s1, agg(16, 8, [0: f64, 8: f64]) ptr @g_dd2, agg(16, 8, [0: i64, 8: i64]) ptr @g_ii2)
  ret %rf
}
