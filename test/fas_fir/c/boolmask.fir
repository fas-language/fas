; masked_load/masked_store of a bool vector with a run-time mask (SPEC §6):
; only the bytes holding a selected lane are read or written (A1), so
; boolmask.c puts the second byte on a PROT_NONE page and selects only
; lanes of the first; and the bits of other lanes are kept.
triple "x86_64-unknown-linux-gnu"

; *p = (old & ~mask) | (v & mask) over 12 lanes; returns the lanes read
; from p where mask is set, 0 elsewhere (both as i16 bit masks)
func @bm_load (ptr, i16) -> (i16) global {
^e(%p: ptr, %mi: i16):
  %mt = trunc i12 %mi
  %m = bitcast <12 x i1> %mt
  %z = splat <12 x i1> i1 0
  %x = masked_load <12 x i1> %p, %m, %z, align 1
  %xi = bitcast i12 %x
  %r = zext i16 %xi
  ret %r
}

func @bm_store (ptr, i16, i16) -> () global {
^e(%p: ptr, %vi: i16, %mi: i16):
  %mt = trunc i12 %mi
  %m = bitcast <12 x i1> %mt
  %vt = trunc i12 %vi
  %v = bitcast <12 x i1> %vt
  masked_store %v, %p, %m, align 1
  ret
}
