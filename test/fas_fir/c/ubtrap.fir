; Trap-mode UB checks for C (what clang emits for -fsanitize=undefined
; -fsanitize-trap=all): each check is ordinary Fir, a condition and a
; branch to a block that traps. Checked by ubtrap.c, which runs each case
; in a child process: the defined inputs must give C's value, the UB
; inputs must die by SIGILL (ud2).
triple "x86_64-unknown-linux-gnu"

; signed-integer-overflow: a + b, a * b in i32
func @ub_add (i32, i32) -> (i32) global {
^e(%a: i32, %b: i32):
  %v, %o = sadd_ovf (i32, i1) %a, %b
  br %o, ^t, ^ok
^ok:
  ret %v
^t:
  trap
  unreachable
}

func @ub_mul64 (i64, i64) -> (i64) global {
^e(%a: i64, %b: i64):
  %v, %o = smul_ovf (i64, i1) %a, %b
  br %o, ^t, ^ok
^ok:
  ret %v
^t:
  trap
  unreachable
}

; unsigned-integer-overflow (clang's optional check): a - b in u32
func @ub_usub (i32, i32) -> (i32) global {
^e(%a: i32, %b: i32):
  %v, %o = usub_ovf (i32, i1) %a, %b
  br %o, ^t, ^ok
^ok:
  ret %v
^t:
  trap
  unreachable
}

; shift-exponent and shift-base: a << b for signed int: b in [0, 32) and
; no set bit shifted into or past the sign: (a >> (31 - b)) == 0 for a >= 0
func @ub_shl (i32, i32) -> (i32) global {
^e(%a: i32, %b: i32):
  %big = icmp uge i1 %b, i32 32
  br %big, ^t, ^base
^base:
  %neg = icmp slt i1 %a, i32 0
  br %neg, ^t, ^lost
^lost:
  %s = sub i32 i32 31, %b
  %hi = lshr i32 %a, %s
  %bad = icmp ne i1 %hi, i32 0
  br %bad, ^t, ^ok
^ok:
  %v = shl i32 %a, %b
  ret %v
^t:
  trap
  unreachable
}

; integer-divide-by-zero and INT_MIN / -1 (signed-integer-overflow)
func @ub_div (i32, i32) -> (i32) global {
^e(%a: i32, %b: i32):
  %z = icmp eq i1 %b, i32 0
  br %z, ^t, ^min
^min:
  %am = icmp eq i1 %a, i32 -2147483648
  %bm = icmp eq i1 %b, i32 -1
  %both = and i1 %am, %bm
  br %both, ^t, ^ok
^ok:
  %v = sdiv i32 %a, %b
  ret %v
^t:
  trap
  unreachable
}

; bounds: int t[8]; t[i] with i unsigned-compared against 8
global @tab size 32 align 4 local readonly = {bytes "\0a\00\00\00\0b\00\00\00\0c\00\00\00\0d\00\00\00\0e\00\00\00\0f\00\00\00\10\00\00\00\11\00\00\00"}
func @ub_index (i64) -> (i32) global {
^e(%i: i64):
  %out = icmp uge i1 %i, i64 8
  br %out, ^t, ^ok
^ok:
  %p = ptradd ptr ptr @tab, %i, 4, 0
  %v = load i32 %p, align 4
  ret %v
^t:
  trap
  unreachable
}

; null and alignment: *p for an int * (4-aligned)
func @ub_deref (ptr) -> (i32) global {
^e(%p: ptr):
  %pi = ptrtoint i64 %p
  %nul = icmp eq i1 %pi, i64 0
  br %nul, ^t, ^al
^al:
  %low = and i64 %pi, i64 3
  %mis = icmp ne i1 %low, i64 0
  br %mis, ^t, ^ok
^ok:
  %v = load i32 %p, align 4
  ret %v
^t:
  trap
  unreachable
}

; pointer-overflow: p + n (bytes) must not wrap the address space
func @ub_ptradd (ptr, i64) -> (i64) global {
^e(%p: ptr, %n: i64):
  %pi = ptrtoint i64 %p
  %v, %o = uadd_ovf (i64, i1) %pi, %n
  br %o, ^t, ^ok
^ok:
  ret %v
^t:
  trap
  unreachable
}

; float-cast-overflow: (int)x needs -2147483649 < x < 2147483648 (NaN fails)
func @ub_f2i (f64) -> (i32) global {
^e(%x: f64):
  %lo = fcmp ogt i1 %x, f64 0xc1e0000000200000
  %hi = fcmp olt i1 %x, f64 0x41e0000000000000
  %in = and i1 %lo, %hi
  br %in, ^ok, ^t
^ok:
  %v = fptosi i32 %x
  ret %v
^t:
  trap
  unreachable
}

; bool: a byte loaded as _Bool must be 0 or 1
func @ub_bool (ptr) -> (i32) global {
^e(%p: ptr):
  %b = load i8 %p, align 1
  %bad = icmp ugt i1 %b, i8 1
  br %bad, ^t, ^ok
^ok:
  %v = zext i32 %b
  ret %v
^t:
  trap
  unreachable
}

; vla-bound: int a[n] needs n > 0; returns the sum of a filled with 1..n
func @ub_vla (i32) -> (i64) global {
^e(%n: i32):
  %bad = icmp sle i1 %n, i32 0
  br %bad, ^t, ^ok
^ok:
  %sp = stacksave
  %nw = sext i64 %n
  %bytes = mul i64 %nw, i64 4
  %a = alloca ptr %bytes, align 4
  jump ^fill(i64 0)
^fill(%i: i64):
  %fd = icmp eq i1 %i, %nw
  br %fd, ^sum(i64 0, i64 0), ^fb
^fb:
  %q = ptradd ptr %a, %i, 4, 0
  %i1 = add i64 %i, i64 1
  %i1t = trunc i32 %i1
  store %i1t, %q, align 4
  jump ^fill(%i1)
^sum(%j: i64, %acc: i64):
  %sd = icmp eq i1 %j, %nw
  br %sd, ^done, ^sb
^sb:
  %r = ptradd ptr %a, %j, 4, 0
  %x = load i32 %r, align 4
  %xw = sext i64 %x
  %acc2 = add i64 %acc, %xw
  %j1 = add i64 %j, i64 1
  jump ^sum(%j1, %acc2)
^done:
  stackrestore %sp
  ret %acc
^t:
  trap
  unreachable
}

; unreachable (__builtin_unreachable under the check): k must be 0..2
func @ub_switch (i32) -> (i32) global {
^e(%k: i32):
  switch %k, ^t, [0: ^a, 1: ^b, 2: ^c]
^a:
  ret i32 100
^b:
  ret i32 200
^c:
  ret i32 300
^t:
  trap
  unreachable
}
