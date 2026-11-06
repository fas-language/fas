; Computed goto (GNU C labels as values): blockaddr and indirect_br.
; Blocks are numbered in order of appearance, the entry being 0.
; Checked by compgoto.c against the same code written with &&label in C.
triple "x86_64-unknown-linux-gnu"

; A bytecode interpreter dispatching through a table of block addresses.
; code is i32 words: 0 HALT | 1 ADD k | 2 MUL k | 3 DECJNZ target | 4 SETCNT k.
; Each handler dispatches the next op itself (threaded code).
func @fir_interp (ptr) -> (i64) global {
^e(%code: ptr):
  %pc = alloca ptr i64 8, align 8
  %acc = alloca ptr i64 8, align 8
  %cnt = alloca ptr i64 8, align 8
  %tbl = alloca ptr i64 40, align 8
  store i64 0, %pc, align 8
  store i64 0, %acc, align 8
  store i64 0, %cnt, align 8
  store ptr blockaddr(@fir_interp, 2), %tbl, align 8
  %t1 = ptradd ptr %tbl, i64 0, 1, 8
  store ptr blockaddr(@fir_interp, 3), %t1, align 8
  %t2 = ptradd ptr %tbl, i64 0, 1, 16
  store ptr blockaddr(@fir_interp, 4), %t2, align 8
  %t3 = ptradd ptr %tbl, i64 0, 1, 24
  store ptr blockaddr(@fir_interp, 5), %t3, align 8
  %t4 = ptradd ptr %tbl, i64 0, 1, 32
  store ptr blockaddr(@fir_interp, 6), %t4, align 8
  jump ^first
^first:
  %p0 = load i64 %pc, align 8
  %o0 = ptradd ptr %code, %p0, 4, 0
  %op0 = load i32 %o0, align 4
  %op0w = sext i64 %op0
  %h0 = ptradd ptr %tbl, %op0w, 8, 0
  %d0 = load ptr %h0, align 8
  indirect_br %d0, [^halt, ^add, ^mul, ^decjnz, ^setcnt]
^halt:
  %r = load i64 %acc, align 8
  ret %r
^add:
  %pa = load i64 %pc, align 8
  %ka = ptradd ptr %code, %pa, 4, 4
  %va = load i32 %ka, align 4
  %vaw = sext i64 %va
  %aa = load i64 %acc, align 8
  %aa1 = add i64 %aa, %vaw
  store %aa1, %acc, align 8
  %pa2 = add i64 %pa, i64 2
  store %pa2, %pc, align 8
  %oa = ptradd ptr %code, %pa2, 4, 0
  %opa = load i32 %oa, align 4
  %opaw = sext i64 %opa
  %ha = ptradd ptr %tbl, %opaw, 8, 0
  %da = load ptr %ha, align 8
  indirect_br %da, [^halt, ^add, ^mul, ^decjnz, ^setcnt]
^mul:
  %pm = load i64 %pc, align 8
  %km = ptradd ptr %code, %pm, 4, 4
  %vm = load i32 %km, align 4
  %vmw = sext i64 %vm
  %am = load i64 %acc, align 8
  %am1 = mul i64 %am, %vmw
  store %am1, %acc, align 8
  %pm2 = add i64 %pm, i64 2
  store %pm2, %pc, align 8
  %om = ptradd ptr %code, %pm2, 4, 0
  %opm = load i32 %om, align 4
  %opmw = sext i64 %opm
  %hm = ptradd ptr %tbl, %opmw, 8, 0
  %dm = load ptr %hm, align 8
  indirect_br %dm, [^halt, ^add, ^mul, ^decjnz, ^setcnt]
^decjnz:
  %pd = load i64 %pc, align 8
  %c = load i64 %cnt, align 8
  %c1 = sub i64 %c, i64 1
  store %c1, %cnt, align 8
  %kd = ptradd ptr %code, %pd, 4, 4
  %vd = load i32 %kd, align 4
  %vdw = sext i64 %vd
  %pd2 = add i64 %pd, i64 2
  %z = icmp eq i1 %c1, i64 0
  %pn = select i64 %z, %pd2, %vdw
  store %pn, %pc, align 8
  jump ^first
^setcnt:
  %ps = load i64 %pc, align 8
  %ks = ptradd ptr %code, %ps, 4, 4
  %vs = load i32 %ks, align 4
  %vsw = sext i64 %vs
  store %vsw, %cnt, align 8
  %ps2 = add i64 %ps, i64 2
  store %ps2, %pc, align 8
  jump ^first
}

; GCC's relative table idiom: targets as offsets from one label,
; goto *(&&a + off[k]).
func @fir_offsets (i32) -> (i64) global {
^e(%k: i32):
  %offs = alloca ptr i64 24, align 8
  %a = ptrtoint i64 ptr blockaddr(@fir_offsets, 1)
  %b = ptrtoint i64 ptr blockaddr(@fir_offsets, 2)
  %c = ptrtoint i64 ptr blockaddr(@fir_offsets, 3)
  %ob = sub i64 %b, %a
  %oc = sub i64 %c, %a
  store i64 0, %offs, align 8
  %q1 = ptradd ptr %offs, i64 0, 1, 8
  store %ob, %q1, align 8
  %q2 = ptradd ptr %offs, i64 0, 1, 16
  store %oc, %q2, align 8
  %kw = sext i64 %k
  %qk = ptradd ptr %offs, %kw, 8, 0
  %off = load i64 %qk, align 8
  %t = ptradd ptr ptr blockaddr(@fir_offsets, 1), %off, 1, 0
  indirect_br %t, [^la, ^lb, ^lc]
^la:
  ret i64 100
^lb:
  ret i64 200
^lc:
  ret i64 300
}

; Label addresses as ordinary values: handed out by one function, compared,
; and jumped to by the function they belong to.
func @fir_label (i32) -> (ptr) global {
^e(%k: i32):
  %z = icmp eq i1 %k, i32 0
  %p = select ptr %z, ptr blockaddr(@fir_jump, 1), ptr blockaddr(@fir_jump, 2)
  ret %p
}
func @fir_jump (ptr) -> (i64) global {
^e(%p: ptr):
  indirect_br %p, [^one, ^two]
^one:
  ret i64 11
^two:
  ret i64 22
}
; 1 when equal addresses compare equal and different ones (also across
; functions) do not
func @fir_label_eq () -> (i32) global {
^e:
  %x = call (i32) -> (ptr) @fir_label(i32 0)
  %y = call (i32) -> (ptr) @fir_label(i32 0)
  %w = call (i32) -> (ptr) @fir_label(i32 1)
  %e1 = icmp eq i1 %x, %y
  %e2 = icmp ne i1 %x, %w
  %e3 = icmp ne i1 %x, ptr blockaddr(@fir_offsets, 1)
  %e4 = icmp ne i1 %x, ptr null
  %a = and i1 %e1, %e2
  %b = and i1 %e3, %e4
  %c = and i1 %a, %b
  %r = zext i32 %c
  ret %r
}

; A static table of label addresses (C: static void *t[] = {&&a, &&b, &&c}),
; with a second copy of one entry to compare
global @g_tbl size 32 align 8 local readonly = {blockaddr(@fir_static, 1), blockaddr(@fir_static, 2), blockaddr(@fir_static, 3), blockaddr(@fir_static, 2)}
func @fir_static (i32) -> (i64) global {
^e(%k: i32):
  %kw = sext i64 %k
  %q = ptradd ptr ptr @g_tbl, %kw, 8, 0
  %p = load ptr %q, align 8
  indirect_br %p, [^sa, ^sb, ^sc]
^sa:
  ret i64 1000
^sb:
  %q3 = ptradd ptr ptr @g_tbl, i64 0, 1, 24
  %p3 = load ptr %q3, align 8
  %same = icmp eq i1 %p, %p3
  %s = zext i64 %same
  %r = add i64 %s, i64 2000
  ret %r
^sc:
  %d = icmp eq i1 %p, ptr blockaddr(@fir_static, 3)
  %dz = zext i64 %d
  %r2 = add i64 %dz, i64 3000
  ret %r2
}
