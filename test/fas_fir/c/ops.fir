; generated: every op the native backend gained through expansion (abs, bswap,
; bitrev, *_ovf, all 16 fcmp predicates, copysign, fmin, fmax, frem, ftrunc) and
; variable-size memcpy/memmove/memset; ops.c checks each against clang
triple "x86_64-unknown-linux-gnu"
func @fir_abs_i8 (i8) -> (i8) global {
^e(%a: i8):
  %r = abs i8 %a
  ret %r
}
func @fir_bitrev_i8 (i8) -> (i8) global {
^e(%a: i8):
  %r = bitrev i8 %a
  ret %r
}
func @fir_sadd_ovf_i8 (i8, i8, ptr) -> (i32) global {
^e(%a: i8, %b: i8, %p: ptr):
  %v, %o = sadd_ovf (i8, i1) %a, %b
  store %v, %p, align 1
  %z = zext i32 %o
  ret %z
}
func @fir_uadd_ovf_i8 (i8, i8, ptr) -> (i32) global {
^e(%a: i8, %b: i8, %p: ptr):
  %v, %o = uadd_ovf (i8, i1) %a, %b
  store %v, %p, align 1
  %z = zext i32 %o
  ret %z
}
func @fir_ssub_ovf_i8 (i8, i8, ptr) -> (i32) global {
^e(%a: i8, %b: i8, %p: ptr):
  %v, %o = ssub_ovf (i8, i1) %a, %b
  store %v, %p, align 1
  %z = zext i32 %o
  ret %z
}
func @fir_usub_ovf_i8 (i8, i8, ptr) -> (i32) global {
^e(%a: i8, %b: i8, %p: ptr):
  %v, %o = usub_ovf (i8, i1) %a, %b
  store %v, %p, align 1
  %z = zext i32 %o
  ret %z
}
func @fir_smul_ovf_i8 (i8, i8, ptr) -> (i32) global {
^e(%a: i8, %b: i8, %p: ptr):
  %v, %o = smul_ovf (i8, i1) %a, %b
  store %v, %p, align 1
  %z = zext i32 %o
  ret %z
}
func @fir_umul_ovf_i8 (i8, i8, ptr) -> (i32) global {
^e(%a: i8, %b: i8, %p: ptr):
  %v, %o = umul_ovf (i8, i1) %a, %b
  store %v, %p, align 1
  %z = zext i32 %o
  ret %z
}
func @fir_abs_i16 (i16) -> (i16) global {
^e(%a: i16):
  %r = abs i16 %a
  ret %r
}
func @fir_bswap_i16 (i16) -> (i16) global {
^e(%a: i16):
  %r = bswap i16 %a
  ret %r
}
func @fir_bitrev_i16 (i16) -> (i16) global {
^e(%a: i16):
  %r = bitrev i16 %a
  ret %r
}
func @fir_sadd_ovf_i16 (i16, i16, ptr) -> (i32) global {
^e(%a: i16, %b: i16, %p: ptr):
  %v, %o = sadd_ovf (i16, i1) %a, %b
  store %v, %p, align 2
  %z = zext i32 %o
  ret %z
}
func @fir_uadd_ovf_i16 (i16, i16, ptr) -> (i32) global {
^e(%a: i16, %b: i16, %p: ptr):
  %v, %o = uadd_ovf (i16, i1) %a, %b
  store %v, %p, align 2
  %z = zext i32 %o
  ret %z
}
func @fir_ssub_ovf_i16 (i16, i16, ptr) -> (i32) global {
^e(%a: i16, %b: i16, %p: ptr):
  %v, %o = ssub_ovf (i16, i1) %a, %b
  store %v, %p, align 2
  %z = zext i32 %o
  ret %z
}
func @fir_usub_ovf_i16 (i16, i16, ptr) -> (i32) global {
^e(%a: i16, %b: i16, %p: ptr):
  %v, %o = usub_ovf (i16, i1) %a, %b
  store %v, %p, align 2
  %z = zext i32 %o
  ret %z
}
func @fir_smul_ovf_i16 (i16, i16, ptr) -> (i32) global {
^e(%a: i16, %b: i16, %p: ptr):
  %v, %o = smul_ovf (i16, i1) %a, %b
  store %v, %p, align 2
  %z = zext i32 %o
  ret %z
}
func @fir_umul_ovf_i16 (i16, i16, ptr) -> (i32) global {
^e(%a: i16, %b: i16, %p: ptr):
  %v, %o = umul_ovf (i16, i1) %a, %b
  store %v, %p, align 2
  %z = zext i32 %o
  ret %z
}
func @fir_abs_i32 (i32) -> (i32) global {
^e(%a: i32):
  %r = abs i32 %a
  ret %r
}
func @fir_bswap_i32 (i32) -> (i32) global {
^e(%a: i32):
  %r = bswap i32 %a
  ret %r
}
func @fir_bitrev_i32 (i32) -> (i32) global {
^e(%a: i32):
  %r = bitrev i32 %a
  ret %r
}
func @fir_sadd_ovf_i32 (i32, i32, ptr) -> (i32) global {
^e(%a: i32, %b: i32, %p: ptr):
  %v, %o = sadd_ovf (i32, i1) %a, %b
  store %v, %p, align 4
  %z = zext i32 %o
  ret %z
}
func @fir_uadd_ovf_i32 (i32, i32, ptr) -> (i32) global {
^e(%a: i32, %b: i32, %p: ptr):
  %v, %o = uadd_ovf (i32, i1) %a, %b
  store %v, %p, align 4
  %z = zext i32 %o
  ret %z
}
func @fir_ssub_ovf_i32 (i32, i32, ptr) -> (i32) global {
^e(%a: i32, %b: i32, %p: ptr):
  %v, %o = ssub_ovf (i32, i1) %a, %b
  store %v, %p, align 4
  %z = zext i32 %o
  ret %z
}
func @fir_usub_ovf_i32 (i32, i32, ptr) -> (i32) global {
^e(%a: i32, %b: i32, %p: ptr):
  %v, %o = usub_ovf (i32, i1) %a, %b
  store %v, %p, align 4
  %z = zext i32 %o
  ret %z
}
func @fir_smul_ovf_i32 (i32, i32, ptr) -> (i32) global {
^e(%a: i32, %b: i32, %p: ptr):
  %v, %o = smul_ovf (i32, i1) %a, %b
  store %v, %p, align 4
  %z = zext i32 %o
  ret %z
}
func @fir_umul_ovf_i32 (i32, i32, ptr) -> (i32) global {
^e(%a: i32, %b: i32, %p: ptr):
  %v, %o = umul_ovf (i32, i1) %a, %b
  store %v, %p, align 4
  %z = zext i32 %o
  ret %z
}
func @fir_abs_i64 (i64) -> (i64) global {
^e(%a: i64):
  %r = abs i64 %a
  ret %r
}
func @fir_bswap_i64 (i64) -> (i64) global {
^e(%a: i64):
  %r = bswap i64 %a
  ret %r
}
func @fir_bitrev_i64 (i64) -> (i64) global {
^e(%a: i64):
  %r = bitrev i64 %a
  ret %r
}
func @fir_sadd_ovf_i64 (i64, i64, ptr) -> (i32) global {
^e(%a: i64, %b: i64, %p: ptr):
  %v, %o = sadd_ovf (i64, i1) %a, %b
  store %v, %p, align 8
  %z = zext i32 %o
  ret %z
}
func @fir_uadd_ovf_i64 (i64, i64, ptr) -> (i32) global {
^e(%a: i64, %b: i64, %p: ptr):
  %v, %o = uadd_ovf (i64, i1) %a, %b
  store %v, %p, align 8
  %z = zext i32 %o
  ret %z
}
func @fir_ssub_ovf_i64 (i64, i64, ptr) -> (i32) global {
^e(%a: i64, %b: i64, %p: ptr):
  %v, %o = ssub_ovf (i64, i1) %a, %b
  store %v, %p, align 8
  %z = zext i32 %o
  ret %z
}
func @fir_usub_ovf_i64 (i64, i64, ptr) -> (i32) global {
^e(%a: i64, %b: i64, %p: ptr):
  %v, %o = usub_ovf (i64, i1) %a, %b
  store %v, %p, align 8
  %z = zext i32 %o
  ret %z
}
func @fir_smul_ovf_i64 (i64, i64, ptr) -> (i32) global {
^e(%a: i64, %b: i64, %p: ptr):
  %v, %o = smul_ovf (i64, i1) %a, %b
  store %v, %p, align 8
  %z = zext i32 %o
  ret %z
}
func @fir_umul_ovf_i64 (i64, i64, ptr) -> (i32) global {
^e(%a: i64, %b: i64, %p: ptr):
  %v, %o = umul_ovf (i64, i1) %a, %b
  store %v, %p, align 8
  %z = zext i32 %o
  ret %z
}
func @fir_fcmp_false_f32 (f32, f32) -> (i32) global {
^e(%a: f32, %b: f32):
  %c = fcmp false i1 %a, %b
  %z = zext i32 %c
  ret %z
}
func @fir_fcmp_oeq_f32 (f32, f32) -> (i32) global {
^e(%a: f32, %b: f32):
  %c = fcmp oeq i1 %a, %b
  %z = zext i32 %c
  ret %z
}
func @fir_fcmp_ogt_f32 (f32, f32) -> (i32) global {
^e(%a: f32, %b: f32):
  %c = fcmp ogt i1 %a, %b
  %z = zext i32 %c
  ret %z
}
func @fir_fcmp_oge_f32 (f32, f32) -> (i32) global {
^e(%a: f32, %b: f32):
  %c = fcmp oge i1 %a, %b
  %z = zext i32 %c
  ret %z
}
func @fir_fcmp_olt_f32 (f32, f32) -> (i32) global {
^e(%a: f32, %b: f32):
  %c = fcmp olt i1 %a, %b
  %z = zext i32 %c
  ret %z
}
func @fir_fcmp_ole_f32 (f32, f32) -> (i32) global {
^e(%a: f32, %b: f32):
  %c = fcmp ole i1 %a, %b
  %z = zext i32 %c
  ret %z
}
func @fir_fcmp_one_f32 (f32, f32) -> (i32) global {
^e(%a: f32, %b: f32):
  %c = fcmp one i1 %a, %b
  %z = zext i32 %c
  ret %z
}
func @fir_fcmp_ord_f32 (f32, f32) -> (i32) global {
^e(%a: f32, %b: f32):
  %c = fcmp ord i1 %a, %b
  %z = zext i32 %c
  ret %z
}
func @fir_fcmp_ueq_f32 (f32, f32) -> (i32) global {
^e(%a: f32, %b: f32):
  %c = fcmp ueq i1 %a, %b
  %z = zext i32 %c
  ret %z
}
func @fir_fcmp_ugt_f32 (f32, f32) -> (i32) global {
^e(%a: f32, %b: f32):
  %c = fcmp ugt i1 %a, %b
  %z = zext i32 %c
  ret %z
}
func @fir_fcmp_uge_f32 (f32, f32) -> (i32) global {
^e(%a: f32, %b: f32):
  %c = fcmp uge i1 %a, %b
  %z = zext i32 %c
  ret %z
}
func @fir_fcmp_ult_f32 (f32, f32) -> (i32) global {
^e(%a: f32, %b: f32):
  %c = fcmp ult i1 %a, %b
  %z = zext i32 %c
  ret %z
}
func @fir_fcmp_ule_f32 (f32, f32) -> (i32) global {
^e(%a: f32, %b: f32):
  %c = fcmp ule i1 %a, %b
  %z = zext i32 %c
  ret %z
}
func @fir_fcmp_une_f32 (f32, f32) -> (i32) global {
^e(%a: f32, %b: f32):
  %c = fcmp une i1 %a, %b
  %z = zext i32 %c
  ret %z
}
func @fir_fcmp_uno_f32 (f32, f32) -> (i32) global {
^e(%a: f32, %b: f32):
  %c = fcmp uno i1 %a, %b
  %z = zext i32 %c
  ret %z
}
func @fir_fcmp_true_f32 (f32, f32) -> (i32) global {
^e(%a: f32, %b: f32):
  %c = fcmp true i1 %a, %b
  %z = zext i32 %c
  ret %z
}
func @fir_copysign_f32 (f32, f32) -> (f32) global {
^e(%a: f32, %b: f32):
  %r = copysign f32 %a, %b
  ret %r
}
func @fir_fmin_f32 (f32, f32) -> (f32) global {
^e(%a: f32, %b: f32):
  %r = fmin f32 %a, %b
  ret %r
}
func @fir_fmax_f32 (f32, f32) -> (f32) global {
^e(%a: f32, %b: f32):
  %r = fmax f32 %a, %b
  ret %r
}
func @fir_frem_f32 (f32, f32) -> (f32) global {
^e(%a: f32, %b: f32):
  %r = frem f32 %a, %b
  ret %r
}
func @fir_ftrunc_f32 (f32) -> (f32) global {
^e(%a: f32):
  %r = ftrunc f32 %a
  ret %r
}
func @fir_fcmp_false_f64 (f64, f64) -> (i32) global {
^e(%a: f64, %b: f64):
  %c = fcmp false i1 %a, %b
  %z = zext i32 %c
  ret %z
}
func @fir_fcmp_oeq_f64 (f64, f64) -> (i32) global {
^e(%a: f64, %b: f64):
  %c = fcmp oeq i1 %a, %b
  %z = zext i32 %c
  ret %z
}
func @fir_fcmp_ogt_f64 (f64, f64) -> (i32) global {
^e(%a: f64, %b: f64):
  %c = fcmp ogt i1 %a, %b
  %z = zext i32 %c
  ret %z
}
func @fir_fcmp_oge_f64 (f64, f64) -> (i32) global {
^e(%a: f64, %b: f64):
  %c = fcmp oge i1 %a, %b
  %z = zext i32 %c
  ret %z
}
func @fir_fcmp_olt_f64 (f64, f64) -> (i32) global {
^e(%a: f64, %b: f64):
  %c = fcmp olt i1 %a, %b
  %z = zext i32 %c
  ret %z
}
func @fir_fcmp_ole_f64 (f64, f64) -> (i32) global {
^e(%a: f64, %b: f64):
  %c = fcmp ole i1 %a, %b
  %z = zext i32 %c
  ret %z
}
func @fir_fcmp_one_f64 (f64, f64) -> (i32) global {
^e(%a: f64, %b: f64):
  %c = fcmp one i1 %a, %b
  %z = zext i32 %c
  ret %z
}
func @fir_fcmp_ord_f64 (f64, f64) -> (i32) global {
^e(%a: f64, %b: f64):
  %c = fcmp ord i1 %a, %b
  %z = zext i32 %c
  ret %z
}
func @fir_fcmp_ueq_f64 (f64, f64) -> (i32) global {
^e(%a: f64, %b: f64):
  %c = fcmp ueq i1 %a, %b
  %z = zext i32 %c
  ret %z
}
func @fir_fcmp_ugt_f64 (f64, f64) -> (i32) global {
^e(%a: f64, %b: f64):
  %c = fcmp ugt i1 %a, %b
  %z = zext i32 %c
  ret %z
}
func @fir_fcmp_uge_f64 (f64, f64) -> (i32) global {
^e(%a: f64, %b: f64):
  %c = fcmp uge i1 %a, %b
  %z = zext i32 %c
  ret %z
}
func @fir_fcmp_ult_f64 (f64, f64) -> (i32) global {
^e(%a: f64, %b: f64):
  %c = fcmp ult i1 %a, %b
  %z = zext i32 %c
  ret %z
}
func @fir_fcmp_ule_f64 (f64, f64) -> (i32) global {
^e(%a: f64, %b: f64):
  %c = fcmp ule i1 %a, %b
  %z = zext i32 %c
  ret %z
}
func @fir_fcmp_une_f64 (f64, f64) -> (i32) global {
^e(%a: f64, %b: f64):
  %c = fcmp une i1 %a, %b
  %z = zext i32 %c
  ret %z
}
func @fir_fcmp_uno_f64 (f64, f64) -> (i32) global {
^e(%a: f64, %b: f64):
  %c = fcmp uno i1 %a, %b
  %z = zext i32 %c
  ret %z
}
func @fir_fcmp_true_f64 (f64, f64) -> (i32) global {
^e(%a: f64, %b: f64):
  %c = fcmp true i1 %a, %b
  %z = zext i32 %c
  ret %z
}
func @fir_copysign_f64 (f64, f64) -> (f64) global {
^e(%a: f64, %b: f64):
  %r = copysign f64 %a, %b
  ret %r
}
func @fir_fmin_f64 (f64, f64) -> (f64) global {
^e(%a: f64, %b: f64):
  %r = fmin f64 %a, %b
  ret %r
}
func @fir_fmax_f64 (f64, f64) -> (f64) global {
^e(%a: f64, %b: f64):
  %r = fmax f64 %a, %b
  ret %r
}
func @fir_frem_f64 (f64, f64) -> (f64) global {
^e(%a: f64, %b: f64):
  %r = frem f64 %a, %b
  ret %r
}
func @fir_ftrunc_f64 (f64) -> (f64) global {
^e(%a: f64):
  %r = ftrunc f64 %a
  ret %r
}
func @fir_memcpy (ptr, ptr, i64) -> () global {
^e(%d: ptr, %s: ptr, %n: i64):
  memcpy %d, %s, %n
  ret
}
func @fir_memmove (ptr, ptr, i64) -> () global {
^e(%d: ptr, %s: ptr, %n: i64):
  memmove %d, %s, %n
  ret
}
func @fir_memset (ptr, i32, i32) -> () global {
^e(%d: ptr, %c: i32, %n: i32):
  %b = trunc i8 %c
  memset %d, %b, %n
  ret
}
; a constant size with a nonzero byte
func @fir_memset7 (ptr) -> () global {
^e(%d: ptr):
  memset %d, i8 -85, i64 7
  ret
}
