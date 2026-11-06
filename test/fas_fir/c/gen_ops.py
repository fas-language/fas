# generates test/fas_fir/c/ops.fir: one exported function per op and width
o=['; generated: every op the native backend gained through expansion (abs, bswap,',
   '; bitrev, *_ovf, all 16 fcmp predicates, copysign, fmin, fmax, ftrunc) and',
   '; variable-size memcpy/memmove/memset; ops.c checks each against clang',
   'triple "x86_64-unknown-linux-gnu"']
W={8:1,16:2,32:4,64:8}
for w in W:
    t=f'i{w}'
    o.append(f'func @fir_abs_{t} ({t}) -> ({t}) global {{\n^e(%a: {t}):\n  %r = abs {t} %a\n  ret %r\n}}')
    if w>=16: o.append(f'func @fir_bswap_{t} ({t}) -> ({t}) global {{\n^e(%a: {t}):\n  %r = bswap {t} %a\n  ret %r\n}}')
    o.append(f'func @fir_bitrev_{t} ({t}) -> ({t}) global {{\n^e(%a: {t}):\n  %r = bitrev {t} %a\n  ret %r\n}}')
    for op in ['sadd','uadd','ssub','usub','smul','umul']:
        o.append(f'func @fir_{op}_ovf_{t} ({t}, {t}, ptr) -> (i32) global {{\n^e(%a: {t}, %b: {t}, %p: ptr):\n'
                 f'  %v, %o = {op}_ovf ({t}, i1) %a, %b\n  store %v, %p, align {W[w]}\n  %z = zext i32 %o\n  ret %z\n}}')
for t in ['f32','f64']:
    for p in 'false oeq ogt oge olt ole one ord ueq ugt uge ult ule une uno true'.split():
        o.append(f'func @fir_fcmp_{p}_{t} ({t}, {t}) -> (i32) global {{\n^e(%a: {t}, %b: {t}):\n  %c = fcmp {p} i1 %a, %b\n  %z = zext i32 %c\n  ret %z\n}}')
    for op in ['copysign','fmin','fmax']:
        o.append(f'func @fir_{op}_{t} ({t}, {t}) -> ({t}) global {{\n^e(%a: {t}, %b: {t}):\n  %r = {op} {t} %a, %b\n  ret %r\n}}')
    o.append(f'func @fir_ftrunc_{t} ({t}) -> ({t}) global {{\n^e(%a: {t}):\n  %r = ftrunc {t} %a\n  ret %r\n}}')
for op in ['memcpy','memmove']:
    o.append(f'func @fir_{op} (ptr, ptr, i64) -> () global {{\n^e(%d: ptr, %s: ptr, %n: i64):\n  {op} %d, %s, %n\n  ret\n}}')
o.append('func @fir_memset (ptr, i32, i32) -> () global {\n^e(%d: ptr, %c: i32, %n: i32):\n  %b = trunc i8 %c\n  memset %d, %b, %n\n  ret\n}')
o.append('; a constant size with a nonzero byte\nfunc @fir_memset7 (ptr) -> () global {\n^e(%d: ptr):\n  memset %d, i8 -85, i64 7\n  ret\n}')
open('test/fas_fir/c/ops.fir','w').write('\n'.join(o)+'\n')
