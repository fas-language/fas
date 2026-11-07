; Locals that the machine-IR backend keeps in virtual registers
; (Mir_isel.promotable_allocas) next to ones it must leave in memory:
; - loop-carried i64 / i8 / f64 / f32 / ptr locals (promoted);
; - a local read before any store on one path (promoted, starts at 0);
; - a local whose address escapes to a call (memory);
; - a local accessed by two types (memory);
; - many short-lived locals in one function, as unrolled code makes them;
; - lifetime markers (sanitizer hints) on a promoted local, which keep it promoted.
; Prints 5050, 186, 7575, 3, 30, 0, 9, 11, 258, 4950.
triple "x86_64-unknown-linux-gnu"

declare @putnum (i64) -> () global

global @cell size 8 align 8 local = zero

func @poke (ptr) -> () local {
^e(%p: ptr):
  %v = load i64 %p, align 8
  %w = add i64 %v, i64 10
  store %w, %p, align 8
  ret
}

; 1..100 summed through locals of every promotable type
func @loops () -> () local {
^e:
  %sum = alloca ptr i64 8, align 8
  %b = alloca ptr i64 1, align 1
  %d = alloca ptr i64 8, align 8
  %f = alloca ptr i64 4, align 4
  %q = alloca ptr i64 8, align 8
  %i = alloca ptr i64 8, align 8
  store i64 0, %sum, align 8
  lifetime_start 1, %b
  store i8 0, %b, align 1
  store f64 0x0000000000000000, %d, align 8
  store f32 0x00000000, %f, align 4
  store ptr @cell, %q, align 8
  store i64 1, %i, align 8
  jump ^head
^head:
  %iv = load i64 %i, align 8
  %done = icmp ugt i1 %iv, i64 100
  br %done, ^out, ^body
^body:
  %s = load i64 %sum, align 8
  %s1 = add i64 %s, %iv
  store %s1, %sum, align 8
  %bv = load i8 %b, align 1
  %i8 = trunc i8 %iv
  %b1 = add i8 %bv, %i8
  store %b1, %b, align 1
  %dv = load f64 %d, align 8
  %fi = sitofp f64 %iv
  %d1 = fadd f64 %dv, %fi
  %d2 = fadd f64 %d1, %fi
  %d3 = fadd f64 %d2, %fi
  store %d3, %d, align 8
  %fv = load f32 %f, align 4
  %one = fptrunc f32 %fi
  %neg = fsub f32 %fv, %one
  %f1 = fadd f32 %neg, %one
  %f2 = fadd f32 %f1, f32 0x3f800000
  %small = icmp ult i1 %iv, i64 4
  %f3 = select f32 %small, %f2, %fv
  store %f3, %f, align 4
  %qv = load ptr %q, align 8
  store %qv, %q, align 8
  %i1 = add i64 %iv, i64 1
  store %i1, %i, align 8
  jump ^head
^out:
  %r = load i64 %sum, align 8
  call (i64) -> () @putnum(%r)
  %rb = load i8 %b, align 1
  %rb64 = zext i64 %rb
  call (i64) -> () @putnum(%rb64)
  lifetime_end 1, %b
  %rd = load f64 %d, align 8
  %rdh = fmul f64 %rd, f64 0x3fe0000000000000
  %rdi = fptosi i64 %rdh
  call (i64) -> () @putnum(%rdi)
  %rf = load f32 %f, align 4
  %rfi = fptosi i64 %rf
  call (i64) -> () @putnum(%rfi)
  %rq = load ptr %q, align 8
  store i64 30, %rq, align 8
  %c = load i64 ptr @cell, align 8
  call (i64) -> () @putnum(%c)
  ret
}

; %x is read before any store when %flag is 0
func @maybe (i64) -> (i64) local {
^e(%flag: i64):
  %x = alloca ptr i64 8, align 8
  %set = icmp ne i1 %flag, i64 0
  br %set, ^store, ^read
^store:
  store i64 9, %x, align 8
  jump ^read
^read:
  %v = load i64 %x, align 8
  %z = icmp eq i1 %flag, i64 0
  %r = select i64 %z, i64 0, %v
  ret %r
}

; the address escapes: must stay in memory and see the callee's store
func @escape () -> (i64) local {
^e:
  %x = alloca ptr i64 8, align 8
  store i64 1, %x, align 8
  call (ptr) -> () @poke(%x)
  %v = load i64 %x, align 8
  ret %v
}

; written as i64, read back as i8 twice (little-endian bytes): memory
func @mixed () -> (i64) local {
^e:
  %x = alloca ptr i64 8, align 8
  store i64 258, %x, align 8
  %lo = load i8 %x, align 1
  %hi_p = ptradd ptr %x, i64 1, 1, 0
  %hi = load i8 %hi_p, align 1
  %lo64 = zext i64 %lo
  %hi64 = zext i64 %hi
  %h = mul i64 %hi64, i64 256
  %r = add i64 %h, %lo64
  ret %r
}

; ten short-lived locals in a row, each stored then loaded
func @unrolled () -> (i64) local {
^e:
  %a0 = alloca ptr i64 8, align 8
  %a1 = alloca ptr i64 8, align 8
  %a2 = alloca ptr i64 8, align 8
  %a3 = alloca ptr i64 8, align 8
  %a4 = alloca ptr i64 8, align 8
  %acc = alloca ptr i64 8, align 8
  store i64 0, %acc, align 8
  jump ^l(i64 0)
^l(%k: i64):
  %k1 = mul i64 %k, i64 5
  store %k1, %a0, align 8
  %v0 = load i64 %a0, align 8
  %t1 = add i64 %v0, i64 1
  store %t1, %a1, align 8
  %v1 = load i64 %a1, align 8
  %t2 = add i64 %v1, i64 1
  store %t2, %a2, align 8
  %v2 = load i64 %a2, align 8
  %t3 = add i64 %v2, i64 1
  store %t3, %a3, align 8
  %v3 = load i64 %a3, align 8
  %t4 = add i64 %v3, i64 1
  store %t4, %a4, align 8
  %v4 = load i64 %a4, align 8
  %acc0 = load i64 %acc, align 8
  %s = add i64 %acc0, %v0
  %s1 = add i64 %s, %v1
  %s2 = add i64 %s1, %v2
  %s3 = add i64 %s2, %v3
  %s4 = add i64 %s3, %v4
  store %s4, %acc, align 8
  %kn = add i64 %k, i64 1
  %more = icmp ult i1 %kn, i64 20
  br %more, ^l(%kn), ^done
^done:
  %r = load i64 %acc, align 8
  ret %r
}

func @main (i32, ptr) -> (i32) global {
^e(%argc: i32, %argv: ptr):
  call () -> () @loops()
  %m0 = call (i64) -> (i64) @maybe(i64 0)
  call (i64) -> () @putnum(%m0)
  %m1 = call (i64) -> (i64) @maybe(i64 1)
  call (i64) -> () @putnum(%m1)
  %e = call () -> (i64) @escape()
  call (i64) -> () @putnum(%e)
  %x = call () -> (i64) @mixed()
  call (i64) -> () @putnum(%x)
  %u = call () -> (i64) @unrolled()
  call (i64) -> () @putnum(%u)
  ret i32 0
}
