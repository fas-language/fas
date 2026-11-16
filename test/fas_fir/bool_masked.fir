; masked_load/masked_store of a bool vector (SPEC §6): bit packed, lane k
; is bit k mod 8 of byte k / 8; only the selected bits change, the rest of
; each byte (and the bits past the last lane) are kept.
triple "x86_64-unknown-linux-gnu"

declare @putnum (i64) -> () global

global @lb size 2 align 1 local = {bytes "\a5\3c"}
global @sb size 3 align 1 local = {bytes "\a5\3c\77"}

func @main () -> (i32) global {
^e:
  ; lanes 0 1 2 9 10 from 0xa5 0x3c, the rest from the all-ones passthru
  %m = masked_load <12 x i1> ptr @lb, <12 x i1> [i1 1, i1 1, i1 1, i1 0, i1 0, i1 0, i1 0, i1 0, i1 0, i1 1, i1 1, i1 0], <12 x i1> [i1 1, i1 1, i1 1, i1 1, i1 1, i1 1, i1 1, i1 1, i1 1, i1 1, i1 1, i1 1], align 1
  %mi = bitcast i12 %m
  %m64 = zext i64 %mi
  call (i64) -> () @putnum(%m64)                     ; 3581
  ; clear lanes 1 2 3 8, then set lanes 4 9
  masked_store <12 x i1> [i1 0, i1 0, i1 0, i1 0, i1 0, i1 0, i1 0, i1 0, i1 0, i1 0, i1 0, i1 0], ptr @sb, <12 x i1> [i1 0, i1 1, i1 1, i1 1, i1 0, i1 0, i1 0, i1 0, i1 1, i1 0, i1 0, i1 0], align 1
  masked_store <12 x i1> [i1 1, i1 1, i1 1, i1 1, i1 1, i1 1, i1 1, i1 1, i1 1, i1 1, i1 1, i1 1], ptr @sb, <12 x i1> [i1 0, i1 0, i1 0, i1 0, i1 1, i1 0, i1 0, i1 0, i1 0, i1 1, i1 0, i1 0], align 1
  %b0 = load i8 ptr @sb, align 1
  %p1 = ptradd ptr ptr @sb, i64 0, 1, 1
  %b1 = load i8 %p1, align 1
  %p2 = ptradd ptr ptr @sb, i64 0, 1, 2
  %b2 = load i8 %p2, align 1
  %w0 = zext i64 %b0
  %w1 = zext i64 %b1
  %w2 = zext i64 %b2
  call (i64) -> () @putnum(%w0)                      ; 0xb1 = 177
  call (i64) -> () @putnum(%w1)                      ; 0x3e = 62
  call (i64) -> () @putnum(%w2)                      ; 0x77 = 119 (not a lane)
  ret i32 0
}
