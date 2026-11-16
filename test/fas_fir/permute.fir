; permute (SPEC §8): lane k of the result is V[IDX[k]], or zero when
; IDX[k] (unsigned) is past the last lane of V; IDX may have any length.
; The result is read back at variable indices in a loop; an i8 vector
; with 255 (unsigned: out of range) and an i1 vector (always in range).
; Expected values are worked out by hand in the comments.
triple "x86_64-unknown-linux-gnu"
declare @putnum (i64) -> () global

func @p32 (i32) -> () local {
^e(%x: i32):
  %y = zext i64 %x
  call (i64) -> () @putnum(%y)
  ret
}

func @main () -> (i32) global {
^e:
  ; v = [3, 13, 23, 33]
  jump ^fill(<4 x i32> zero, i32 0)
^fill(%acc: <4 x i32>, %i: i32):
  %t = mul i32 %i, i32 10
  %val = add i32 %t, i32 3
  %acc1 = insert <4 x i32> %acc, %val, %i
  %i1 = add i32 %i, i32 1
  %more = icmp ult i1 %i1, i32 4
  br %more, ^fill(%acc1, %i1), ^filled(%acc1)
^filled(%v: <4 x i32>):
  ; idx = [3, 0, 7, 1, 2, -1] (i32, 6 lanes): 7 and -1 are out of range
  %x0 = insert <6 x i32> <6 x i32> zero, i32 3, i32 0
  %x1 = insert <6 x i32> %x0, i32 0, i32 1
  %x2 = insert <6 x i32> %x1, i32 7, i32 2
  %x3 = insert <6 x i32> %x2, i32 1, i32 3
  %x4 = insert <6 x i32> %x3, i32 2, i32 4
  %x5 = insert <6 x i32> %x4, i32 -1, i32 5
  %p = permute <6 x i32> %v, %x5                       ; [33, 3, 0, 13, 23, 0]
  jump ^out(i32 0)
^out(%j: i32):
  %y = extract i32 %p, %j
  call (i32) -> () @p32(%y)                            ; 33 3 0 13 23 0
  %j1 = add i32 %j, i32 1
  %more2 = icmp ult i1 %j1, i32 6
  br %more2, ^out(%j1), ^small()
^small():
  ; i8 indices [1, 255]: 255 is unsigned, so out of range
  %s0 = insert <2 x i8> <2 x i8> zero, i8 1, i32 0
  %s1 = insert <2 x i8> %s0, i8 -1, i32 1
  %ps = permute <2 x i32> %v, %s1                      ; [13, 0]
  %ps0 = extract i32 %ps, i32 0
  call (i32) -> () @p32(%ps0)                          ; 13
  %ps1 = extract i32 %ps, i32 1
  call (i32) -> () @p32(%ps1)                          ; 0
  ; i1 indices [1, 0, 1]: every index reaches a lane
  %b0 = insert <3 x i1> <3 x i1> zero, i1 1, i32 0
  %b1 = insert <3 x i1> %b0, i1 1, i32 2
  %pb = permute <3 x i32> %v, %b1                      ; [13, 3, 13]
  %pr = reduce add i32 %pb
  call (i32) -> () @p32(%pr)                           ; 29
  ; a 64-bit element and an i64 index past 2^32
  %w0 = insert <2 x i64> <2 x i64> zero, i64 1000000000000, i32 0
  %w1 = insert <2 x i64> %w0, i64 7, i32 1
  %k0 = insert <3 x i64> <3 x i64> zero, i64 1, i32 0
  %k1 = insert <3 x i64> %k0, i64 4294967296, i32 2
  %pw = permute <3 x i64> %w1, %k1                     ; [7, 1000000000000, 0]
  %pwr = reduce add i64 %pw
  call (i64) -> () @putnum(%pwr)                       ; 1000000000007
  ret i32 0
}
