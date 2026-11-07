; Thread-local globals and named sections through the native static link
; (no libc: _start sets up the TLS block from PT_TLS). Prints the
; initialized and zeroed thread-local values before and after stores, the
; 64-byte alignment of a thread-local, a registry summed between the
; linker's __start_/__stop_ symbols, and data in named .rodata/.bss
; sections. (c/tls.fir checks TLS across threads, against C.)
triple "x86_64-unknown-linux-gnu"

declare @putnum (i64) -> () global

; .tdata
global @counter size 8 align 8 global thread_local = {bytes "\05\00\00\00\00\00\00\00"}
global @pair size 16 align 8 local thread_local = {bytes "\0b\00\00\00\00\00\00\00\16\00\00\00\00\00\00\00"}
; .tbss
global @zeroed size 24 align 8 local thread_local = zero
global @wide size 64 align 64 local thread_local = zero
alias @pair_second local = @pair + 8

; a registry: entries in section "fas_reg", walked between the linker's
; __start_fas_reg and __stop_fas_reg
global @reg_a size 8 align 8 local section "fas_reg" = {bytes "\64\00\00\00\00\00\00\00"}
global @reg_b size 8 align 8 local section "fas_reg" readonly = {bytes "\c8\00\00\00\00\00\00\00"}
global @reg_c size 8 align 8 global section "fas_reg" = {bytes "\2c\01\00\00\00\00\00\00"}
global @__start_fas_reg size 8 align 8 global = extern
global @__stop_fas_reg size 8 align 8 global = extern

global @ro size 8 align 8 local section ".rodata.mine" readonly = {bytes "\07\00\00\00\00\00\00\00"}
global @zb size 32 align 16 local section ".bss.mine" = zero

func @bump (i64) -> (i64) local {
^e(%by: i64):
  %v = load i64 ptr @counter, align 8
  %w = add i64 %v, %by
  store %w, ptr @counter, align 8
  ret %w
}

func @main (i32, ptr) -> (i32) global {
^e(%argc: i32, %argv: ptr):
  ; 5, then 5 + 10 = 15
  %c0 = load i64 ptr @counter, align 8
  call (i64) -> () @putnum(%c0)
  %c1 = call (i64) -> (i64) @bump(i64 10)
  call (i64) -> () @putnum(%c1)
  ; 22 through the alias, then 11 at the start of the same variable
  %s = load i64 ptr @pair_second, align 8
  call (i64) -> () @putnum(%s)
  %f = load i64 ptr @pair, align 8
  call (i64) -> () @putnum(%f)
  ; .tbss starts zero; write and read back its middle word: 0, 33
  %zp = ptradd ptr ptr @zeroed, i64 0, 1, 8
  %z0 = load i64 %zp, align 8
  call (i64) -> () @putnum(%z0)
  store i64 33, %zp, align 8
  %z1 = load i64 %zp, align 8
  call (i64) -> () @putnum(%z1)
  ; @wide is 64-aligned: 0; its last word is zero: 0
  %wa = ptrtoint i64 ptr @wide
  %wm = and i64 %wa, i64 63
  call (i64) -> () @putnum(%wm)
  %wl = ptradd ptr ptr @wide, i64 0, 1, 56
  %wv = load i64 %wl, align 8
  call (i64) -> () @putnum(%wv)
  ; registry: 100 + 200 + 300 = 600, and 3 entries
  %lo = ptrtoint i64 ptr @__start_fas_reg
  %hi = ptrtoint i64 ptr @__stop_fas_reg
  %n = sub i64 %hi, %lo
  %cnt = udiv i64 %n, i64 8
  call (i64) -> () @putnum(%cnt)
  jump ^loop(ptr @__start_fas_reg, i64 0)
^loop(%p: ptr, %acc: i64):
  %pa = ptrtoint i64 %p
  %more = icmp ult i1 %pa, %hi
  br %more, ^body, ^done
^body:
  %x = load i64 %p, align 8
  %acc2 = add i64 %acc, %x
  %p2 = ptradd ptr %p, i64 0, 1, 8
  jump ^loop(%p2, %acc2)
^done:
  call (i64) -> () @putnum(%acc)
  ; named .rodata/.bss sections: 7, then 0 and 9 after a store
  %r = load i64 ptr @ro, align 8
  call (i64) -> () @putnum(%r)
  %zbp = ptradd ptr ptr @zb, i64 0, 1, 16
  %b0 = load i64 %zbp, align 8
  call (i64) -> () @putnum(%b0)
  store i64 9, %zbp, align 8
  %b1 = load i64 %zbp, align 8
  call (i64) -> () @putnum(%b1)
  ret i32 0
}
