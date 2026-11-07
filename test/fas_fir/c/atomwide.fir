; Atomics the backends cannot do as one instruction (Fir_atomic): odd
; widths on their container (add/sub/min/max as a cmpxchg loop), integers
; wider than 64 bits and f80 through libatomic. Checked by atomwide.c:
; each op against C on one thread, then threads of Fir code and C code
; (__atomic builtins, libatomic too) updating the same objects at once.
; libs: -latomic -lpthread
triple "x86_64-unknown-linux-gnu"

func @a33_add (ptr, i33) -> (i33) global {
^e(%p: ptr, %v: i33):
  %o = atomic_rmw add i33 %p, %v, align 8 seq_cst
  ret %o
}

func @a33_sub (ptr, i33) -> (i33) global {
^e(%p: ptr, %v: i33):
  %o = atomic_rmw sub i33 %p, %v, align 8 acq_rel
  ret %o
}

func @a33_max (ptr, i33) -> (i33) global {
^e(%p: ptr, %v: i33):
  %o = atomic_rmw max i33 %p, %v, align 8 acquire
  ret %o
}

func @a33_min (ptr, i33) -> (i33) global {
^e(%p: ptr, %v: i33):
  %o = atomic_rmw min i33 %p, %v, align 8 release
  ret %o
}

func @a33_umax (ptr, i33) -> (i33) global {
^e(%p: ptr, %v: i33):
  %o = atomic_rmw umax i33 %p, %v, align 8 relaxed
  ret %o
}

func @a33_umin (ptr, i33) -> (i33) global {
^e(%p: ptr, %v: i33):
  %o = atomic_rmw umin i33 %p, %v, align 8 seq_cst
  ret %o
}

func @a33_xchg (ptr, i33) -> (i33) global {
^e(%p: ptr, %v: i33):
  %o = atomic_rmw xchg i33 %p, %v, align 8 seq_cst
  ret %o
}

func @a33_or (ptr, i33) -> (i33) global {
^e(%p: ptr, %v: i33):
  %o = atomic_rmw or i33 %p, %v, align 8 seq_cst
  ret %o
}

func @a33_and (ptr, i33) -> (i33) global {
^e(%p: ptr, %v: i33):
  %o = atomic_rmw and i33 %p, %v, align 8 acquire
  ret %o
}

func @a33_xor (ptr, i33) -> (i33) global {
^e(%p: ptr, %v: i33):
  %o = atomic_rmw xor i33 %p, %v, align 8 relaxed
  ret %o
}

func @a7_add (ptr, i7) -> (i7) global {
^e(%p: ptr, %v: i7):
  %o = atomic_rmw add i7 %p, %v, align 1 seq_cst
  ret %o
}

func @a7_sub (ptr, i7) -> (i7) global {
^e(%p: ptr, %v: i7):
  %o = atomic_rmw sub i7 %p, %v, align 1 seq_cst
  ret %o
}

func @a7_max (ptr, i7) -> (i7) global {
^e(%p: ptr, %v: i7):
  %o = atomic_rmw max i7 %p, %v, align 1 seq_cst
  ret %o
}

func @a7_min (ptr, i7) -> (i7) global {
^e(%p: ptr, %v: i7):
  %o = atomic_rmw min i7 %p, %v, align 1 seq_cst
  ret %o
}

func @a7_umax (ptr, i7) -> (i7) global {
^e(%p: ptr, %v: i7):
  %o = atomic_rmw umax i7 %p, %v, align 1 seq_cst
  ret %o
}

func @a7_umin (ptr, i7) -> (i7) global {
^e(%p: ptr, %v: i7):
  %o = atomic_rmw umin i7 %p, %v, align 1 seq_cst
  ret %o
}

func @a1_xchg (ptr, i1) -> (i1) global {
^e(%p: ptr, %v: i1):
  %o = atomic_rmw xchg i1 %p, %v, align 1 seq_cst
  ret %o
}

func @a1_load (ptr) -> (i1) global {
^e(%p: ptr):
  %o = load i1 %p, align 1 acquire
  ret %o
}

func @a1_store (ptr, i1) -> () global {
^e(%p: ptr, %v: i1):
  store %v, %p, align 1 release
  ret
}

func @a33_load (ptr) -> (i33) global {
^e(%p: ptr):
  %o = load i33 %p, align 8 acquire
  ret %o
}

func @a33_store (ptr, i33) -> () global {
^e(%p: ptr, %v: i33):
  store %v, %p, align 8 release
  ret
}

; old value; *ok = the flag
func @a33_cas (ptr, i33, i33, ptr) -> (i33) global {
^e(%p: ptr, %x: i33, %y: i33, %okp: ptr):
  %o, %ok = cmpxchg (i33, i1) %p, %x, %y, align 8 seq_cst, fail acquire
  %okb = zext i8 %ok
  store %okb, %okp, align 1
  ret %o
}

; old value; *ok = the flag
func @a7_cas (ptr, i7, i7, ptr) -> (i7) global {
^e(%p: ptr, %x: i7, %y: i7, %okp: ptr):
  %o, %ok = cmpxchg weak (i7, i1) %p, %x, %y, align 1 acq_rel, fail relaxed
  %okb = zext i8 %ok
  store %okb, %okp, align 1
  ret %o
}

func @a100_add (ptr, i100) -> (i100) global {
^e(%p: ptr, %v: i100):
  %o = atomic_rmw add i100 %p, %v, align 16 seq_cst
  ret %o
}

func @a128_add (ptr, i128) -> (i128) global {
^e(%p: ptr, %v: i128):
  %o = atomic_rmw add i128 %p, %v, align 16 seq_cst
  ret %o
}

func @a256_add (ptr, i256) -> (i256) global {
^e(%p: ptr, %v: i256):
  %o = atomic_rmw add i256 %p, %v, align 8 seq_cst
  ret %o
}

func @a100_sub (ptr, i100) -> (i100) global {
^e(%p: ptr, %v: i100):
  %o = atomic_rmw sub i100 %p, %v, align 16 acq_rel
  ret %o
}

func @a128_sub (ptr, i128) -> (i128) global {
^e(%p: ptr, %v: i128):
  %o = atomic_rmw sub i128 %p, %v, align 16 acq_rel
  ret %o
}

func @a256_sub (ptr, i256) -> (i256) global {
^e(%p: ptr, %v: i256):
  %o = atomic_rmw sub i256 %p, %v, align 8 acq_rel
  ret %o
}

func @a100_max (ptr, i100) -> (i100) global {
^e(%p: ptr, %v: i100):
  %o = atomic_rmw max i100 %p, %v, align 16 acquire
  ret %o
}

func @a128_max (ptr, i128) -> (i128) global {
^e(%p: ptr, %v: i128):
  %o = atomic_rmw max i128 %p, %v, align 16 acquire
  ret %o
}

func @a256_max (ptr, i256) -> (i256) global {
^e(%p: ptr, %v: i256):
  %o = atomic_rmw max i256 %p, %v, align 8 acquire
  ret %o
}

func @a100_min (ptr, i100) -> (i100) global {
^e(%p: ptr, %v: i100):
  %o = atomic_rmw min i100 %p, %v, align 16 release
  ret %o
}

func @a128_min (ptr, i128) -> (i128) global {
^e(%p: ptr, %v: i128):
  %o = atomic_rmw min i128 %p, %v, align 16 release
  ret %o
}

func @a256_min (ptr, i256) -> (i256) global {
^e(%p: ptr, %v: i256):
  %o = atomic_rmw min i256 %p, %v, align 8 release
  ret %o
}

func @a100_umax (ptr, i100) -> (i100) global {
^e(%p: ptr, %v: i100):
  %o = atomic_rmw umax i100 %p, %v, align 16 relaxed
  ret %o
}

func @a128_umax (ptr, i128) -> (i128) global {
^e(%p: ptr, %v: i128):
  %o = atomic_rmw umax i128 %p, %v, align 16 relaxed
  ret %o
}

func @a256_umax (ptr, i256) -> (i256) global {
^e(%p: ptr, %v: i256):
  %o = atomic_rmw umax i256 %p, %v, align 8 relaxed
  ret %o
}

func @a100_umin (ptr, i100) -> (i100) global {
^e(%p: ptr, %v: i100):
  %o = atomic_rmw umin i100 %p, %v, align 16 seq_cst
  ret %o
}

func @a128_umin (ptr, i128) -> (i128) global {
^e(%p: ptr, %v: i128):
  %o = atomic_rmw umin i128 %p, %v, align 16 seq_cst
  ret %o
}

func @a256_umin (ptr, i256) -> (i256) global {
^e(%p: ptr, %v: i256):
  %o = atomic_rmw umin i256 %p, %v, align 8 seq_cst
  ret %o
}

func @a100_xchg (ptr, i100) -> (i100) global {
^e(%p: ptr, %v: i100):
  %o = atomic_rmw xchg i100 %p, %v, align 16 seq_cst
  ret %o
}

func @a128_xchg (ptr, i128) -> (i128) global {
^e(%p: ptr, %v: i128):
  %o = atomic_rmw xchg i128 %p, %v, align 16 seq_cst
  ret %o
}

func @a256_xchg (ptr, i256) -> (i256) global {
^e(%p: ptr, %v: i256):
  %o = atomic_rmw xchg i256 %p, %v, align 8 seq_cst
  ret %o
}

func @a100_or (ptr, i100) -> (i100) global {
^e(%p: ptr, %v: i100):
  %o = atomic_rmw or i100 %p, %v, align 16 seq_cst
  ret %o
}

func @a128_or (ptr, i128) -> (i128) global {
^e(%p: ptr, %v: i128):
  %o = atomic_rmw or i128 %p, %v, align 16 seq_cst
  ret %o
}

func @a256_or (ptr, i256) -> (i256) global {
^e(%p: ptr, %v: i256):
  %o = atomic_rmw or i256 %p, %v, align 8 seq_cst
  ret %o
}

func @a100_and (ptr, i100) -> (i100) global {
^e(%p: ptr, %v: i100):
  %o = atomic_rmw and i100 %p, %v, align 16 acquire
  ret %o
}

func @a128_and (ptr, i128) -> (i128) global {
^e(%p: ptr, %v: i128):
  %o = atomic_rmw and i128 %p, %v, align 16 acquire
  ret %o
}

func @a256_and (ptr, i256) -> (i256) global {
^e(%p: ptr, %v: i256):
  %o = atomic_rmw and i256 %p, %v, align 8 acquire
  ret %o
}

func @a100_xor (ptr, i100) -> (i100) global {
^e(%p: ptr, %v: i100):
  %o = atomic_rmw xor i100 %p, %v, align 16 relaxed
  ret %o
}

func @a128_xor (ptr, i128) -> (i128) global {
^e(%p: ptr, %v: i128):
  %o = atomic_rmw xor i128 %p, %v, align 16 relaxed
  ret %o
}

func @a256_xor (ptr, i256) -> (i256) global {
^e(%p: ptr, %v: i256):
  %o = atomic_rmw xor i256 %p, %v, align 8 relaxed
  ret %o
}

func @a100_load (ptr) -> (i100) global {
^e(%p: ptr):
  %o = load i100 %p, align 16 seq_cst
  ret %o
}

func @a100_store (ptr, i100) -> () global {
^e(%p: ptr, %v: i100):
  store %v, %p, align 16 seq_cst
  ret
}

; old value; *ok = the flag
func @a100_cas (ptr, i100, i100, ptr) -> (i100) global {
^e(%p: ptr, %x: i100, %y: i100, %okp: ptr):
  %o, %ok = cmpxchg (i100, i1) %p, %x, %y, align 16 seq_cst, fail seq_cst
  %okb = zext i8 %ok
  store %okb, %okp, align 1
  ret %o
}

func @a128_load (ptr) -> (i128) global {
^e(%p: ptr):
  %o = load i128 %p, align 16 seq_cst
  ret %o
}

func @a128_store (ptr, i128) -> () global {
^e(%p: ptr, %v: i128):
  store %v, %p, align 16 seq_cst
  ret
}

; old value; *ok = the flag
func @a128_cas (ptr, i128, i128, ptr) -> (i128) global {
^e(%p: ptr, %x: i128, %y: i128, %okp: ptr):
  %o, %ok = cmpxchg (i128, i1) %p, %x, %y, align 16 seq_cst, fail seq_cst
  %okb = zext i8 %ok
  store %okb, %okp, align 1
  ret %o
}

func @a256_load (ptr) -> (i256) global {
^e(%p: ptr):
  %o = load i256 %p, align 8 seq_cst
  ret %o
}

func @a256_store (ptr, i256) -> () global {
^e(%p: ptr, %v: i256):
  store %v, %p, align 8 seq_cst
  ret
}

; old value; *ok = the flag
func @a256_cas (ptr, i256, i256, ptr) -> (i256) global {
^e(%p: ptr, %x: i256, %y: i256, %okp: ptr):
  %o, %ok = cmpxchg (i256, i1) %p, %x, %y, align 8 seq_cst, fail seq_cst
  %okb = zext i8 %ok
  store %okb, %okp, align 1
  ret %o
}

func @f80_fadd (ptr, f80) -> (f80) global {
^e(%p: ptr, %v: f80):
  %o = atomic_rmw fadd f80 %p, %v, align 16 seq_cst
  ret %o
}

func @f80_fsub (ptr, f80) -> (f80) global {
^e(%p: ptr, %v: f80):
  %o = atomic_rmw fsub f80 %p, %v, align 16 seq_cst
  ret %o
}

func @f80_xchg (ptr, f80) -> (f80) global {
^e(%p: ptr, %v: f80):
  %o = atomic_rmw xchg f80 %p, %v, align 16 seq_cst
  ret %o
}

func @f80_load (ptr) -> (f80) global {
^e(%p: ptr):
  %o = load f80 %p, align 16 acquire
  ret %o
}

func @f80_store (ptr, f80) -> () global {
^e(%p: ptr, %v: f80):
  store %v, %p, align 16 release
  ret
}

; old value; *ok = the flag
func @f80_cas (ptr, f80, f80, ptr) -> (f80) global {
^e(%p: ptr, %x: f80, %y: f80, %okp: ptr):
  %o, %ok = cmpxchg (f80, i1) %p, %x, %y, align 16 seq_cst, fail seq_cst
  %okb = zext i8 %ok
  store %okb, %okp, align 1
  ret %o
}

; n times atomic_rmw add (seq_cst)
func @a33_add_loop (ptr, i33, i64) -> () global {
^e(%p: ptr, %v: i33, %n: i64):
  jump ^l(i64 0)
^l(%i: i64):
  %d = icmp eq i1 %i, %n
  br %d, ^x, ^b
^b:
  %o = atomic_rmw add i33 %p, %v, align 8 seq_cst
  %i1 = add i64 %i, i64 1
  jump ^l(%i1)
^x:
  ret
}

; n times atomic_rmw max (seq_cst)
func @a7_max_loop (ptr, i7, i64) -> () global {
^e(%p: ptr, %v: i7, %n: i64):
  jump ^l(i64 0)
^l(%i: i64):
  %d = icmp eq i1 %i, %n
  br %d, ^x, ^b
^b:
  %o = atomic_rmw max i7 %p, %v, align 1 seq_cst
  %i1 = add i64 %i, i64 1
  jump ^l(%i1)
^x:
  ret
}

; n times atomic_rmw add (seq_cst)
func @a128_add_loop (ptr, i128, i64) -> () global {
^e(%p: ptr, %v: i128, %n: i64):
  jump ^l(i64 0)
^l(%i: i64):
  %d = icmp eq i1 %i, %n
  br %d, ^x, ^b
^b:
  %o = atomic_rmw add i128 %p, %v, align 16 seq_cst
  %i1 = add i64 %i, i64 1
  jump ^l(%i1)
^x:
  ret
}

; n times atomic_rmw add (seq_cst)
func @a256_add_loop (ptr, i256, i64) -> () global {
^e(%p: ptr, %v: i256, %n: i64):
  jump ^l(i64 0)
^l(%i: i64):
  %d = icmp eq i1 %i, %n
  br %d, ^x, ^b
^b:
  %o = atomic_rmw add i256 %p, %v, align 8 seq_cst
  %i1 = add i64 %i, i64 1
  jump ^l(%i1)
^x:
  ret
}

; n times atomic_rmw fadd (seq_cst)
func @f80_fadd_loop (ptr, f80, i64) -> () global {
^e(%p: ptr, %v: f80, %n: i64):
  jump ^l(i64 0)
^l(%i: i64):
  %d = icmp eq i1 %i, %n
  br %d, ^x, ^b
^b:
  %o = atomic_rmw fadd f80 %p, %v, align 16 seq_cst
  %i1 = add i64 %i, i64 1
  jump ^l(%i1)
^x:
  ret
}

; n times atomic_rmw sub (seq_cst)
func @a100_sub_loop (ptr, i100, i64) -> () global {
^e(%p: ptr, %v: i100, %n: i64):
  jump ^l(i64 0)
^l(%i: i64):
  %d = icmp eq i1 %i, %n
  br %d, ^x, ^b
^b:
  %o = atomic_rmw sub i100 %p, %v, align 16 seq_cst
  %i1 = add i64 %i, i64 1
  jump ^l(%i1)
^x:
  ret
}
