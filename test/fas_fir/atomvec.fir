; Atomics of vectors of 1, 2, 4 or 8 bytes (Fir_atomic: on the integer of
; the bytes), one thread: lane-wise add/umin as a compare-and-swap loop,
; xor bitwise, a bool vector's load/store (the bits past its lanes zero),
; cmpxchg comparing every lane bit for bit. c/atomvec.fir has the
; libatomic ones and threads.
triple "x86_64-unknown-linux-gnu"

declare @putnum (i64) -> () global

global @a size 4 align 4 local = {bytes "\fe\10\80\01"}
global @h size 4 align 4 local = {bytes "\05\00\fd\ff"}
global @b size 2 align 2 local = {bytes "\0f\03"}
global @f size 8 align 8 local = {bytes "\00\00\c0\3f\00\00\00\80"}

func @main () -> (i32) global {
^e:
  ; lanes 0xfe 0x10 0x80 0x01 + 3 4 0x80 0xff
  %o1 = atomic_rmw add <4 x i8> ptr @a, <4 x i8> [i8 3, i8 4, i8 -128, i8 -1], align 4 seq_cst
  %a1 = load i32 ptr @a, align 4
  %a1w = zext i64 %a1
  call (i64) -> () @putnum(%a1w)                      ; 0x00001401 = 5121
  %o1i = bitcast i32 %o1
  %o1w = zext i64 %o1i
  call (i64) -> () @putnum(%o1w)                      ; 0x018010fe = 25170174
  ; umin of (5, 0xfffd) with (7, 2)
  %o2 = atomic_rmw umin <2 x i16> ptr @h, <2 x i16> [i16 7, i16 2], align 4 acq_rel
  %h1 = load i32 ptr @h, align 4
  %h1w = zext i64 %h1
  call (i64) -> () @putnum(%h1w)                      ; 0x00020005 = 131077
  ; xor lanes 0 2 3 7 10 11, then a store of all lanes set, then a load
  %o3 = atomic_rmw xor <12 x i1> ptr @b, <12 x i1> [i1 1, i1 0, i1 1, i1 1, i1 0, i1 0, i1 0, i1 1, i1 0, i1 0, i1 1, i1 1], align 2 relaxed
  %b1 = load i16 ptr @b, align 2
  %b1w = zext i64 %b1
  call (i64) -> () @putnum(%b1w)                      ; 0x0f82 = 3970
  %o3i = bitcast i12 %o3
  %o3w = zext i64 %o3i
  call (i64) -> () @putnum(%o3w)                      ; 0x30f = 783
  store <12 x i1> [i1 1, i1 1, i1 1, i1 1, i1 1, i1 1, i1 1, i1 1, i1 1, i1 1, i1 1, i1 1], ptr @b, align 2 seq_cst
  %b2 = load i16 ptr @b, align 2
  %b2w = zext i64 %b2
  call (i64) -> () @putnum(%b2w)                      ; 0x0fff = 4095
  %l = load <12 x i1> ptr @b, align 2 acquire
  %li = bitcast i12 %l
  %lw = zext i64 %li
  call (i64) -> () @putnum(%lw)                       ; 4095
  ; (1.5, -0.0): expecting (1.5, 0.0) fails, (1.5, -0.0) succeeds
  %x1, %ok1 = cmpxchg (<2 x f32>, i1) ptr @f, <2 x f32> [f32 0x3fc00000, f32 0x00000000], <2 x f32> [f32 0x40e00000, f32 0x41000000], align 8 seq_cst, fail seq_cst
  %k1 = zext i64 %ok1
  call (i64) -> () @putnum(%k1)                       ; 0
  %x2, %ok2 = cmpxchg (<2 x f32>, i1) ptr @f, <2 x f32> [f32 0x3fc00000, f32 0x80000000], <2 x f32> [f32 0x40e00000, f32 0x41000000], align 8 seq_cst, fail seq_cst
  %k2 = zext i64 %ok2
  call (i64) -> () @putnum(%k2)                       ; 1
  %f1 = load i64 ptr @f, align 8
  call (i64) -> () @putnum(%f1)                       ; 0x4100000040e00000 = 4683743613553737728
  ret i32 0
}
