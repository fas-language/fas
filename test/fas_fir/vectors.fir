; Vectors through the machine-IR backend (Fir_scalarize): inserts and
; extracts at variable indices (vectors homed in stack slots), a select
; with a scalar condition on homed vectors, shuffle, reductions,
; compress/expand, masked memory, a packed bool vector and a vector call.
; Expected values are worked out by hand in the comments.
; mir-only: Fir_to_ir has no masked memory, compress/expand or most
; reductions (SPEC §12).
triple "x86_64-unknown-linux-gnu"
declare @putnum (i64) -> () global

func @p32 (i32) -> () local {
^e(%x: i32):
  %y = zext i64 %x
  call (i64) -> () @putnum(%y)
  ret
}

; vector in, vector out (hidden result pointer after scalarization)
func @twice (<4 x i32>) -> (<4 x i32>) local {
^e(%v: <4 x i32>):
  %r = add <4 x i32> %v, %v
  ret %r
}

func @main (i32, ptr) -> (i32) global {
^e(%argc: i32, %argv: ptr):
  ; homed vectors: run without arguments, argc = 1
  %h = insert <4 x i32> <4 x i32> zero, i32 5, %argc   ; [0, 5, 0, 0]
  %h2 = insert <4 x i32> %h, i32 9, i32 3              ; [0, 5, 0, 9]
  %isone = icmp eq i1 %argc, i32 1
  %s = select <4 x i32> %isone, %h2, %h                ; [0, 5, 0, 9]
  %sr = reduce add i32 %s
  call (i32) -> () @p32(%sr)                           ; 14
  %h0 = extract i32 %h, i32 3
  call (i32) -> () @p32(%h0)                           ; 0: %h is unchanged
  ; lane i = 10 i + 3, inserted at a variable index in a loop
  jump ^fill(<4 x i32> zero, i32 0)
^fill(%acc: <4 x i32>, %i: i32):
  %t = mul i32 %i, i32 10
  %val = add i32 %t, i32 3
  %acc1 = insert <4 x i32> %acc, %val, %i
  %i1 = add i32 %i, i32 1
  %more = icmp ult i1 %i1, i32 4
  br %more, ^fill(%acc1, %i1), ^filled(%acc1)
^filled(%v: <4 x i32>):                                ; [3, 13, 23, 33]
  jump ^back(i32 3)
^back(%j: i32):
  %x = extract i32 %v, %j
  call (i32) -> () @p32(%x)                            ; 33 23 13 3
  %done = icmp eq i1 %j, i32 0
  %j1 = sub i32 %j, i32 1
  br %done, ^red(), ^back(%j1)
^red():
  %ra = reduce add i32 %v
  call (i32) -> () @p32(%ra)                           ; 72
  %rm = reduce mul i32 %v
  call (i32) -> () @p32(%rm)                           ; 3*13*23*33 = 29601
  %rx = reduce xor i32 %v
  call (i32) -> () @p32(%rx)                           ; 3^13^23^33 = 56
  %ro = reduce or i32 %v
  call (i32) -> () @p32(%ro)                           ; 63
  %rs = reduce smin i32 %v
  call (i32) -> () @p32(%rs)                           ; 3
  %ru = reduce umax i32 %v
  call (i32) -> () @p32(%ru)                           ; 33
  ; shuffle of v and h2 = [0, 5, 0, 9]: lanes 7, 0, 5, 2
  %sh = shuffle <4 x i32> %v, %h2, [7, 0, 5, 2]        ; [9, 3, 5, 23]
  %sh0 = extract i32 %sh, i32 0
  call (i32) -> () @p32(%sh0)                          ; 9
  %sh3 = extract i32 %sh, i32 3
  call (i32) -> () @p32(%sh3)                          ; 23
  ; compress by a computed mask, expand by a constant one
  %ten = splat <4 x i32> i32 10
  %m = icmp ugt <4 x i1> %v, %ten                      ; [0, 1, 1, 1]
  %c = compress <4 x i32> %v, %m                       ; [13, 23, 33, 0]
  %c0 = extract i32 %c, i32 0
  %c2 = extract i32 %c, i32 2
  %c3 = extract i32 %c, i32 3
  call (i32) -> () @p32(%c0)                           ; 13
  call (i32) -> () @p32(%c2)                           ; 33
  call (i32) -> () @p32(%c3)                           ; 0
  %x1 = expand <4 x i32> %v, <4 x i1> [i1 0, i1 1, i1 0, i1 1]   ; [0, 3, 0, 13]
  %x1s = reduce add i32 %x1
  call (i32) -> () @p32(%x1s)                          ; 16
  %x13 = extract i32 %x1, i32 3
  call (i32) -> () @p32(%x13)                          ; 13
  ; masked memory
  %buf = alloca ptr i64 16, align 16
  store <4 x i32> [i32 1, i32 2, i32 3, i32 4], %buf, align 16
  masked_store %v, %buf, <4 x i1> [i1 0, i1 1, i1 0, i1 1], align 4
  %mem = load <4 x i32> %buf, align 16                 ; [1, 13, 3, 33]
  %mems = reduce add i32 %mem
  call (i32) -> () @p32(%mems)                         ; 50
  %seven = splat <4 x i32> i32 7
  %ml = masked_load <4 x i32> %buf, <4 x i1> [i1 1, i1 0, i1 0, i1 1], %seven, align 4   ; [1, 7, 7, 33]
  %mls = reduce add i32 %ml
  call (i32) -> () @p32(%mls)                          ; 48
  ; a bool vector in memory is bit-packed: [0, 1, 1, 1] is 0b1110
  %bb = alloca ptr i64 1, align 1
  store %m, %bb, align 1
  %byte = load i8 %bb, align 1
  %low = and i8 %byte, i8 15
  %low32 = zext i32 %low
  call (i32) -> () @p32(%low32)                        ; 14
  %m2 = load <4 x i1> %bb, align 1
  %hundred = splat <4 x i32> i32 100
  %sel = select <4 x i32> %m2, %v, %hundred            ; [100, 13, 23, 33]
  %sels = reduce add i32 %sel
  call (i32) -> () @p32(%sels)                         ; 169
  ; vector call
  %tw = call (<4 x i32>) -> (<4 x i32>) @twice(%v)    ; [6, 26, 46, 66]
  %tws = reduce add i32 %tw
  call (i32) -> () @p32(%tws)                          ; 144
  ret i32 0
}
