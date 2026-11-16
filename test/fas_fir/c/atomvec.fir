; Atomics of vectors (Fir_atomic): a vector of 1, 2, 4 or 8 bytes works
; on the integer of its bytes (and/or/xor/xchg/cmpxchg bitwise, the rest a
; lane-wise compare-and-swap loop), any other vector through libatomic on
; its bytes. Checked by atomvec.c: each op once, then threads updating the
; same objects at once.
; libs: -latomic -lpthread
triple "x86_64-unknown-linux-gnu"

; image (i32): lane-wise add as a cmpxchg loop
func @v4i8_add_loop (ptr, i64) -> () global {
^e(%p: ptr, %n: i64):
  jump ^l(i64 0)
^l(%i: i64):
  %d = icmp uge i1 %i, %n
  br %d, ^x, ^b
^b:
  %o = atomic_rmw add <4 x i8> %p, <4 x i8> [i8 1, i8 2, i8 3, i8 4], align 4 seq_cst
  %i2 = add i64 %i, i64 1
  jump ^l(%i2)
^x:
  ret
}

; libatomic (16 bytes)
func @v4i32_add_loop (ptr, i64) -> () global {
^e(%p: ptr, %n: i64):
  jump ^l(i64 0)
^l(%i: i64):
  %d = icmp uge i1 %i, %n
  br %d, ^x, ^b
^b:
  %o = atomic_rmw add <4 x i32> %p, <4 x i32> [i32 1, i32 2, i32 3, i32 4], align 16 acq_rel
  %i2 = add i64 %i, i64 1
  jump ^l(%i2)
^x:
  ret
}

; libatomic (12 bytes, not a power of two)
func @v3f32_fadd_loop (ptr, i64) -> () global {
^e(%p: ptr, %n: i64):
  jump ^l(i64 0)
^l(%i: i64):
  %d = icmp uge i1 %i, %n
  br %d, ^x, ^b
^b:
  %o = atomic_rmw fadd <3 x f32> %p, <3 x f32> [f32 0x3f800000, f32 0x40000000, f32 0x3f000000], align 4 seq_cst
  %i2 = add i64 %i, i64 1
  jump ^l(%i2)
^x:
  ret
}

; image (i16) of a bool vector: xor bitwise
func @v12b_xor_loop (ptr, i64) -> () global {
^e(%p: ptr, %n: i64):
  jump ^l(i64 0)
^l(%i: i64):
  %d = icmp uge i1 %i, %n
  br %d, ^x, ^b
^b:
  %o = atomic_rmw xor <12 x i1> %p, <12 x i1> [i1 1, i1 0, i1 1, i1 1, i1 0, i1 0, i1 0, i1 1, i1 0, i1 0, i1 1, i1 1], align 2 relaxed
  %i2 = add i64 %i, i64 1
  jump ^l(%i2)
^x:
  ret
}

; single operations: operands and old values through memory

func @v3i8_umax (ptr, ptr, ptr) -> () global {
^e(%p: ptr, %src: ptr, %out: ptr):
  %v = load <3 x i8> %src, align 1
  %o = atomic_rmw umax <3 x i8> %p, %v, align 1 seq_cst
  store %o, %out, align 1
  ret
}

func @v2i16_min (ptr, ptr, ptr) -> () global {
^e(%p: ptr, %src: ptr, %out: ptr):
  %v = load <2 x i16> %src, align 2
  %o = atomic_rmw min <2 x i16> %p, %v, align 4 seq_cst
  store %o, %out, align 2
  ret
}

func @v3f32_xchg (ptr, ptr, ptr) -> () global {
^e(%p: ptr, %src: ptr, %out: ptr):
  %v = load <3 x f32> %src, align 4
  %o = atomic_rmw xchg <3 x f32> %p, %v, align 4 seq_cst
  store %o, %out, align 4
  ret
}

func @v4i32_load (ptr, ptr) -> () global {
^e(%p: ptr, %out: ptr):
  %v = load <4 x i32> %p, align 16 acquire
  store %v, %out, align 4
  ret
}

func @v4i32_store (ptr, ptr) -> () global {
^e(%p: ptr, %src: ptr):
  %v = load <4 x i32> %src, align 4
  store %v, %p, align 16 release
  ret
}

func @v12b_load (ptr, ptr) -> () global {
^e(%p: ptr, %out: ptr):
  %v = load <12 x i1> %p, align 2 seq_cst
  store %v, %out, align 1
  ret
}

func @v12b_store (ptr, ptr) -> () global {
^e(%p: ptr, %src: ptr):
  %v = load <12 x i1> %src, align 1
  store %v, %p, align 2 seq_cst
  ret
}

; image (i64): compares every lane bit for bit
func @v2f32_cas (ptr, ptr, ptr, ptr) -> (i8) global {
^e(%p: ptr, %exp: ptr, %des: ptr, %out: ptr):
  %x = load <2 x f32> %exp, align 4
  %y = load <2 x f32> %des, align 4
  %o, %ok = cmpxchg (<2 x f32>, i1) %p, %x, %y, align 8 seq_cst, fail acquire
  store %o, %out, align 4
  %r = zext i8 %ok
  ret %r
}

; libatomic (16 bytes)
func @v4i32_cas (ptr, ptr, ptr, ptr) -> (i8) global {
^e(%p: ptr, %exp: ptr, %des: ptr, %out: ptr):
  %x = load <4 x i32> %exp, align 4
  %y = load <4 x i32> %des, align 4
  %o, %ok = cmpxchg (<4 x i32>, i1) %p, %x, %y, align 16 acq_rel, fail relaxed
  store %o, %out, align 4
  %r = zext i8 %ok
  ret %r
}
