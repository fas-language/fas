#!/usr/bin/env python3
"""Seeded differential fuzzer comparing fas-as against llvm-mc-22.

Generates random valid x86-64 AT&T instructions across the entire table
(GP 8/16/32/64-bit, REX/hi8 rules, system, x87, addressing modes, edge
immediates, relocations, branch relaxation) and compares .text bytes +
relocations byte-for-byte with `llvm-mc-22 -triple=x86_64-unknown-linux-gnu -filetype=obj`.
"""

import os, random, struct, subprocess, sys, tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "tools/refcache"))
from compiler import assemble

R8 = ['%al', '%cl', '%dl', '%bl', '%spl', '%bpl', '%sil', '%dil',
      '%r8b', '%r9b', '%r10b', '%r11b', '%r12b', '%r13b', '%r14b', '%r15b']
R8_NO_REX = ['%al', '%cl', '%dl', '%bl', '%ah', '%ch', '%dh', '%bh']
R16 = ['%ax', '%cx', '%dx', '%bx', '%sp', '%bp', '%si', '%di',
       '%r8w', '%r9w', '%r10w', '%r11w', '%r12w', '%r13w', '%r14w', '%r15w']
R32 = ['%eax', '%ecx', '%edx', '%ebx', '%esp', '%ebp', '%esi', '%edi',
       '%r8d', '%r9d', '%r10d', '%r11d', '%r12d', '%r13d', '%r14d', '%r15d']
R64 = ['%rax', '%rcx', '%rdx', '%rbx', '%rsp', '%rbp', '%rsi', '%rdi',
       '%r8', '%r9', '%r10', '%r11', '%r12', '%r13', '%r14', '%r15']
R64_IDX = [r for r in R64 if r != '%rsp']
R32_IDX = [r for r in R32 if r != '%esp']
ST = [f'%st({i})' for i in range(8)]
XMM = [f'%xmm{i}' for i in range(16)]
YMM = [f'%ymm{i}' for i in range(16)]
XMM32 = [f'%xmm{i}' for i in range(32)]
YMM32 = [f'%ymm{i}' for i in range(32)]
ZMM = [f'%zmm{i}' for i in range(32)]
KREG = [f'%k{i}' for i in range(8)]
KMASK = [f'%k{i}' for i in range(1, 8)]

EDGE_IMM8 = [0, 1, -1, 2, 7, 127, -128, 255]
EDGE_IMM16 = [0, 1, -1, 127, 128, -128, -129, 32767, -32768, 65535]
EDGE_IMM32S = [0, 1, -1, 127, 128, -128, -129, 0x7fff, -0x8000, 0x7fffffff, -0x80000000]
EDGE_IMM32 = EDGE_IMM32S + [0x80000000, 0xffffffff]
EDGE_IMM64 = EDGE_IMM32 + [0x123456789abcdef0, -0x123456789abcdef0]
DISPS = [0, 1, -1, 8, -8, 127, -128, 128, -129, 0x1234, -0x1234]
CCS = ['o', 'no', 'b', 'ae', 'e', 'ne', 'be', 'a', 's', 'ns', 'p', 'np', 'l', 'ge', 'le', 'g']

def rand_mem(rng, allow_rip=True, allow_seg=True):
    kind = rng.randint(0, 7)
    seg = ''
    if allow_seg and rng.random() < 0.12:
        seg = rng.choice(['%fs:', '%gs:'])
    if kind == 0:
        return f"{seg}({rng.choice(R64)})"
    if kind == 1:
        d = rng.choice(DISPS)
        return f"{seg}{d}({rng.choice(R64)})"
    if kind == 2:
        sc = rng.choice([1, 2, 4, 8])
        d = rng.choice(DISPS)
        return f"{seg}{d}({rng.choice(R64)},{rng.choice(R64_IDX)},{sc})"
    if kind == 3:
        sc = rng.choice([1, 2, 4, 8])
        d = rng.choice(DISPS)
        return f"{seg}{d}(,{rng.choice(R64_IDX)},{sc})"
    if kind == 4 and allow_rip:
        d = rng.choice([0, 4, -4, 0x100])
        if rng.random() < 0.5:
            return f"{seg}ext_sym({ '%rip' })" if d == 0 else f"{seg}ext_sym+{d}(%rip)"
        return f"{seg}{d}(%rip)"
    if kind == 5:
        d = rng.choice([0, 16, -16, 0x200])
        return f"{seg}{d}({rng.choice(R32)})"
    if kind == 6:
        sc = rng.choice([1, 2, 4, 8])
        return f"{seg}({rng.choice(R32)},{rng.choice(R32_IDX)},{sc})"
    return f"{seg}{rng.choice([0, 0x1000, -0x100])}"

def rand_insn(rng):
    cat = rng.randint(0, 23)
    if cat == 0: # ALU 8/16/32/64
        op = rng.choice(['add', 'or', 'adc', 'sbb', 'and', 'sub', 'xor', 'cmp'])
        sz = rng.choice(['b', 'w', 'l', 'q'])
        m = op + (sz if rng.random() < 0.7 else '')
        form = rng.randint(0, 3)
        if sz == 'b':
            if form == 0: return f"{op}b ${rng.choice(EDGE_IMM8)}, {rng.choice(R8)}"
            if form == 1: return f"{op}b ${rng.choice(EDGE_IMM8)}, {rand_mem(rng)}"
            if form == 2: return f"{m} {rng.choice(R8)}, {rng.choice(R8)}"
            return f"{m} {rng.choice(R8)}, {rand_mem(rng)}"
        elif sz == 'w':
            if form == 0: return f"{op}w ${rng.choice(EDGE_IMM16)}, {rng.choice(R16)}"
            if form == 1: return f"{op}w ${rng.choice(EDGE_IMM16)}, {rand_mem(rng)}"
            if form == 2: return f"{m} {rng.choice(R16)}, {rng.choice(R16)}"
            return f"{m} {rand_mem(rng)}, {rng.choice(R16)}"
        elif sz == 'l':
            if form == 0: return f"{op}l ${rng.choice(EDGE_IMM32)}, {rng.choice(R32)}"
            if form == 1: return f"{op}l ${rng.choice(EDGE_IMM32)}, {rand_mem(rng)}"
            if form == 2: return f"{m} {rng.choice(R32)}, {rng.choice(R32)}"
            return f"{m} {rand_mem(rng)}, {rng.choice(R32)}"
        else:
            if form == 0: return f"{op}q ${rng.choice(EDGE_IMM32S)}, {rng.choice(R64)}"
            if form == 1: return f"{op}q ${rng.choice(EDGE_IMM32S)}, {rand_mem(rng)}"
            if form == 2: return f"{m} {rng.choice(R64)}, {rng.choice(R64)}"
            return f"{m} {rand_mem(rng)}, {rng.choice(R64)}"
    if cat == 1: # MOV / MOVABS / MOVSX / MOVZX / LEA
        sub = rng.randint(0, 6)
        if sub == 0: return f"movb ${rng.choice(EDGE_IMM8)}, {rng.choice(R8)}"
        if sub == 1: return f"movl ${rng.choice(EDGE_IMM32)}, {rng.choice(R32)}"
        if sub == 2: return f"movq ${rng.choice(EDGE_IMM32S)}, {rng.choice(R64)}"
        if sub == 3: return f"movabsq ${rng.choice(EDGE_IMM64)}, {rng.choice(R64)}"
        if sub == 4:
            pair = rng.choice([
                ('movsbw', R8, R16), ('movsbl', R8, R32), ('movsbq', R8, R64),
                ('movswl', R16, R32), ('movswq', R16, R64), ('movslq', R32, R64),
                ('movzbw', R8, R16), ('movzbl', R8, R32), ('movzbq', R8, R64),
                ('movzwl', R16, R32), ('movzwq', R16, R64),
            ])
            src = rng.choice(pair[1]) if rng.random() < 0.5 else rand_mem(rng)
            return f"{pair[0]} {src}, {rng.choice(pair[2])}"
        if sub == 5:
            return f"leaq {rand_mem(rng, allow_seg=False)}, {rng.choice(R64)}"
        return f"movq {rand_mem(rng)}, {rng.choice(R64)}"
    if cat == 2: # TEST
        sz = rng.choice(['b', 'w', 'l', 'q'])
        if sz == 'b': return f"testb ${rng.choice(EDGE_IMM8)}, {rng.choice(R8)}"
        if sz == 'w': return f"testw {rng.choice(R16)}, {rng.choice(R16)}"
        if sz == 'l': return f"testl ${rng.choice(EDGE_IMM32)}, {rng.choice(R32)}"
        return f"testq ${rng.choice(EDGE_IMM32S)}, {rng.choice(R64)}"
    if cat == 3: # Unary: not/neg/mul/imul/div/idiv/inc/dec
        op = rng.choice(['not', 'neg', 'mul', 'imul', 'div', 'idiv', 'inc', 'dec'])
        sz, regs = rng.choice([('b', R8), ('w', R16), ('l', R32), ('q', R64)])
        target = rng.choice(regs) if rng.random() < 0.6 else rand_mem(rng)
        return f"{op}{sz} {target}"
    if cat == 4: # IMUL 2-op / 3-op
        sz, regs, imms = rng.choice([('w', R16, EDGE_IMM16), ('l', R32, EDGE_IMM32), ('q', R64, EDGE_IMM32S)])
        form = rng.randint(0, 2)
        if form == 0: return f"imul{sz} {rng.choice(regs)}, {rng.choice(regs)}"
        if form == 1: return f"imul{sz} ${rng.choice(imms)}, {rng.choice(regs)}, {rng.choice(regs)}"
        return f"imul{sz} ${rng.choice(imms)}, {rand_mem(rng)}, {rng.choice(regs)}"
    if cat == 5: # Shifts & rotates
        op = rng.choice(['rol', 'ror', 'rcl', 'rcr', 'shl', 'shr', 'sar'])
        sz, regs = rng.choice([('b', R8), ('w', R16), ('l', R32), ('q', R64)])
        cnt = rng.choice(['$1', '$0', '$7', '$31', '%cl'])
        return f"{op}{sz} {cnt}, {rng.choice(regs)}"
    if cat == 6: # SHLD / SHRD
        op = rng.choice(['shld', 'shrd'])
        sz, regs = rng.choice([('w', R16), ('l', R32), ('q', R64)])
        cnt = rng.choice(['$1', '$5', '%cl'])
        return f"{op}{sz} {cnt}, {rng.choice(regs)}, {rng.choice(regs)}"
    if cat == 7: # PUSH / POP
        op = rng.choice(['pushq', 'popq'])
        if op == 'pushq' and rng.random() < 0.3:
            return f"pushq ${rng.choice(EDGE_IMM32S)}"
        return f"{op} {rng.choice(R64) if rng.random() < 0.7 else rand_mem(rng)}"
    if cat == 8: # XCHG / CMPXCHG / XADD / BSWAP / BT* / BSF / BSR
        sub = rng.randint(0, 5)
        if sub == 0:
            sz, regs = rng.choice([('b', R8), ('w', R16), ('l', R32), ('q', R64)])
            return f"xchg{sz} {rng.choice(regs)}, {rng.choice(regs)}"
        if sub == 1:
            op = rng.choice(['cmpxchg', 'xadd'])
            sz, regs = rng.choice([('b', R8), ('w', R16), ('l', R32), ('q', R64)])
            pfx = 'lock ' if rng.random() < 0.4 else ''
            return f"{pfx}{op}{sz} {rng.choice(regs)}, {rand_mem(rng)}"
        if sub == 2:
            return rng.choice([f"bswapl {rng.choice(R32)}", f"bswapq {rng.choice(R64)}"])
        if sub == 3:
            op = rng.choice(['bt', 'bts', 'btr', 'btc'])
            sz, regs = rng.choice([('w', R16), ('l', R32), ('q', R64)])
            if rng.random() < 0.5: return f"{op}{sz} {rng.choice(regs)}, {rng.choice(regs)}"
            return f"{op}{sz} ${rng.choice([0, 1, 7, 31])}, {rng.choice(regs)}"
        if sub == 4:
            op = rng.choice(['bsf', 'bsr'])
            sz, regs = rng.choice([('w', R16), ('l', R32), ('q', R64)])
            return f"{op}{sz} {rng.choice(regs)}, {rng.choice(regs)}"
        return f"cmpxchg16b {rand_mem(rng)}"
    if cat == 9: # CMOVcc / SETcc
        cc = rng.choice(CCS)
        if rng.random() < 0.5:
            sz, regs = rng.choice([('w', R16), ('l', R32), ('q', R64)])
            return f"cmov{cc}{sz} {rng.choice(regs)}, {rng.choice(regs)}"
        return f"set{cc} {rng.choice(R8)}"
    if cat == 10: # Branches / calls / indirect / relocs
        sub = rng.randint(0, 5)
        if sub == 0: return f"j{rng.choice(CCS)} 1f"
        if sub == 1: return "jmp 1f"
        if sub == 2: return rng.choice(["call ext_fn", "call ext_fn@PLT", "call 1f"])
        if sub == 3: return f"{rng.choice(['call', 'jmp'])} *{rng.choice(R64)}"
        if sub == 4: return f"{rng.choice(['call', 'jmp'])} *ext_sym@GOTPCREL(%rip)"
        return rng.choice([
            f"movq ext_sym@GOTPCREL(%rip), {rng.choice(R64)}",
            f"movl ext_sym@GOTPCREL(%rip), {rng.choice(R32)}",
            f"addq ext_sym@GOTPCREL(%rip), {rng.choice(R64)}",
            f"movq ext_sym@GOTTPOFF(%rip), {rng.choice(R64)}",
            f"leaq ext_sym@TLSGD(%rip), %rdi",
            f"leaq ext_sym@TLSLD(%rip), %rdi",
            f"leaq ext_sym@DTPOFF(%rax), %rax",
            f"movq $ext_sym@TPOFF, {rng.choice(R64)}",
        ])
    if cat == 11: # High 8-bit registers without REX
        op = rng.choice(['addb', 'subb', 'xorb', 'movb', 'cmpb', 'testb'])
        r1 = rng.choice(R8_NO_REX)
        r2 = rng.choice(R8_NO_REX)
        return f"{op} {r1}, {r2}"
    if cat == 12: # System & zero-op instructions
        return rng.choice([
            'nop', 'ret', 'leave', 'int3', 'clc', 'stc', 'cmc', 'cld', 'std',
            'cbw', 'cwde', 'cdqe', 'cwd', 'cdq', 'cqo',
            'movsb', 'movsq', 'rep movsb', 'rep stosq', 'repe cmpsb', 'repne scasb',
            'syscall', 'cpuid', 'rdtsc', 'rdtscp', 'xgetbv',
            'lfence', 'mfence', 'sfence', 'pause', 'ud2', 'endbr64',
            f'clflush {rand_mem(rng)}', f'prefetcht0 {rand_mem(rng)}',
        ])
    if cat == 13: # x87
        return rng.choice([
            'fldz', 'fld1', 'fldpi', 'fchs', 'fabs', 'fsqrt', 'fsin', 'fcos', 'frndint', 'fcompp', 'fucompp',
            f'flds {rand_mem(rng)}', f'fldl {rand_mem(rng)}', f'fldt {rand_mem(rng)}',
            f'fstps {rand_mem(rng)}', f'fstpl {rand_mem(rng)}', f'fstpt {rand_mem(rng)}',
            f'fildl {rand_mem(rng)}', f'fildll {rand_mem(rng)}', f'fistpl {rand_mem(rng)}', f'fistpll {rand_mem(rng)}',
            f'fadds {rand_mem(rng)}', f'faddl {rand_mem(rng)}', f'fmull {rand_mem(rng)}',
            f'fsubl {rand_mem(rng)}', f'fdivl {rand_mem(rng)}',
            f'fld {rng.choice(ST)}', f'fstp {rng.choice(ST)}', f'fxch {rng.choice(ST)}',
            f'fadd {rng.choice(ST)}, %st(0)', f'fadd %st(0), {rng.choice(ST)}',
            f'faddp %st(0), {rng.choice(ST)}', f'fmulp %st(0), {rng.choice(ST)}',
            f'fsub {rng.choice(ST)}, %st(0)', f'fsub %st(0), {rng.choice(ST)}',
            f'fsubp %st(0), {rng.choice(ST)}', f'fsubrp %st(0), {rng.choice(ST)}',
            f'fdiv {rng.choice(ST)}, %st(0)', f'fdiv %st(0), {rng.choice(ST)}',
            f'fdivp %st(0), {rng.choice(ST)}', f'fdivrp %st(0), {rng.choice(ST)}',
            f'fucomi {rng.choice(ST)}, %st(0)', f'fucomip {rng.choice(ST)}, %st(0)',
            'fnstsw %ax', f'fldcw {rand_mem(rng)}', f'fnstcw {rand_mem(rng)}',
        ])
    if cat == 14: # Alignment & relaxation stress
        return rng.choice([
            '.p2align 4',
            '.balign 8',
            'jnz 1f\n  .zero 140\n1:',
            'jmp 1f\n  .zero 140\n1:',
        ])
    if cat == 15: # POPCNT, LZCNT, TZCNT, MOVBE, ADX, RDRAND, RDSEED, CRC32
        sub = rng.randint(0, 5)
        if sub == 0:
            op = rng.choice(['popcnt', 'lzcnt', 'tzcnt'])
            sz, regs = rng.choice([('w', R16), ('l', R32), ('q', R64)])
            src = rng.choice(regs) if rng.random() < 0.6 else rand_mem(rng)
            return f"{op}{sz} {src}, {rng.choice(regs)}"
        if sub == 1:
            sz, regs = rng.choice([('w', R16), ('l', R32), ('q', R64)])
            return rng.choice([f"movbe{sz} {rand_mem(rng)}, {rng.choice(regs)}", f"movbe{sz} {rng.choice(regs)}, {rand_mem(rng)}"])
        if sub == 2:
            op = rng.choice(['adcx', 'adox'])
            sz, regs = rng.choice([('l', R32), ('q', R64)])
            return f"{op}{sz} {rng.choice(regs)}, {rng.choice(regs)}"
        if sub == 3:
            op = rng.choice(['rdrand', 'rdseed'])
            sz, regs = rng.choice([('w', R16), ('l', R32), ('q', R64)])
            return f"{op}{sz} {rng.choice(regs)}"
        return rng.choice([
            f"crc32b {rng.choice(R8)}, {rng.choice(R32)}",
            f"crc32b {rng.choice(R8)}, {rng.choice(R64)}",
            f"crc32w {rng.choice(R16)}, {rng.choice(R32)}",
            f"crc32l {rng.choice(R32)}, {rng.choice(R32)}",
            f"crc32q {rng.choice(R64)}, {rng.choice(R64)}",
        ])
    if cat == 16: # BMI1 & BMI2
        sz, regs = rng.choice([('l', R32), ('q', R64)])
        sub = rng.randint(0, 4)
        if sub == 0:
            op = rng.choice(['andn', 'mulx', 'pdep', 'pext'])
            src = rng.choice(regs) if rng.random() < 0.6 else rand_mem(rng)
            return f"{op}{sz} {src}, {rng.choice(regs)}, {rng.choice(regs)}"
        if sub == 1:
            op = rng.choice(['bextr', 'bzhi', 'shlx', 'sarx', 'shrx'])
            src = rng.choice(regs) if rng.random() < 0.6 else rand_mem(rng)
            return f"{op}{sz} {rng.choice(regs)}, {src}, {rng.choice(regs)}"
        if sub == 2:
            op = rng.choice(['blsr', 'blsmsk', 'blsi'])
            src = rng.choice(regs) if rng.random() < 0.6 else rand_mem(rng)
            return f"{op}{sz} {src}, {rng.choice(regs)}"
        return f"rorx{sz} ${rng.choice([0, 1, 7, 31])}, {rng.choice(regs)}, {rng.choice(regs)}"
    if cat == 17: # SSE..SSE4.2 FP & conversions
        sub = rng.randint(0, 4)
        if sub == 0:
            op = rng.choice(['addps', 'addpd', 'addss', 'addsd', 'subps', 'subpd', 'subss', 'subsd',
                             'mulps', 'mulpd', 'mulss', 'mulsd', 'divps', 'divpd', 'divss', 'divsd',
                             'minps', 'maxpd', 'andps', 'xorpd', 'orps', 'haddps', 'hsubpd', 'addsubps'])
            src = rng.choice(XMM) if rng.random() < 0.6 else rand_mem(rng)
            return f"{op} {src}, {rng.choice(XMM)}"
        if sub == 1:
            op = rng.choice(['movaps', 'movups', 'movapd', 'movupd', 'movss', 'movsd', 'movdqa', 'movdqu'])
            if rng.random() < 0.5:
                return f"{op} {rng.choice(XMM) if rng.random() < 0.5 else rand_mem(rng)}, {rng.choice(XMM)}"
            return f"{op} {rng.choice(XMM)}, {rand_mem(rng)}"
        if sub == 2:
            op = rng.choice(['shufps', 'shufpd', 'cmpps', 'cmppd', 'cmpss', 'cmpsd', 'blendps', 'blendpd', 'dpps', 'roundps', 'roundsd'])
            return f"{op} ${rng.choice([0, 1, 3, 7, 0xe4])}, {rng.choice(XMM)}, {rng.choice(XMM)}"
        if sub == 3:
            return rng.choice([
                f"cvtsi2ssl {rng.choice(R32)}, {rng.choice(XMM)}",
                f"cvtsi2sdq {rng.choice(R64)}, {rng.choice(XMM)}",
                f"cvttss2sil {rng.choice(XMM)}, {rng.choice(R32)}",
                f"cvttsd2siq {rng.choice(XMM)}, {rng.choice(R64)}",
                f"cvtss2sd {rng.choice(XMM)}, {rng.choice(XMM)}",
                f"cvtdq2ps {rng.choice(XMM)}, {rng.choice(XMM)}",
                f"ucomisd {rng.choice(XMM)}, {rng.choice(XMM)}",
                f"movd {rng.choice(R32)}, {rng.choice(XMM)}",
                f"movq {rng.choice(R64)}, {rng.choice(XMM)}",
                f"movq {rng.choice(XMM)}, {rng.choice(R64)}",
            ])
        return rng.choice([
            f"movmskps {rng.choice(XMM)}, {rng.choice(R32)}",
            f"pmovmskb {rng.choice(XMM)}, {rng.choice(R32)}",
            f"movsldup {rng.choice(XMM)}, {rng.choice(XMM)}",
            f"movddup {rand_mem(rng)}, {rng.choice(XMM)}",
        ])
    if cat == 18: # SSE2/SSSE3/SSE4.1/4.2 Integer SIMD + AES + PCLMUL + SHA
        sub = rng.randint(0, 4)
        if sub == 0:
            op = rng.choice(['paddb', 'paddw', 'paddd', 'paddq', 'psubd', 'pmulld', 'pmullw', 'pmuludq',
                             'pand', 'pandn', 'por', 'pxor', 'pcmpeqb', 'pcmpgtd', 'pminsd', 'pmaxud',
                             'pshufb', 'phaddd', 'pmaddubsw', 'pabsd', 'ptest', 'pmovsxbw', 'pmovzxbd'])
            src = rng.choice(XMM) if rng.random() < 0.6 else rand_mem(rng)
            return f"{op} {src}, {rng.choice(XMM)}"
        if sub == 1:
            op = rng.choice(['psllw', 'pslld', 'psllq', 'psrlw', 'psrld', 'psrlq', 'psrad', 'pslldq', 'psrldq'])
            return f"{op} ${rng.choice([0, 1, 4, 15])}, {rng.choice(XMM)}"
        if sub == 2:
            return rng.choice([
                f"pshufd $0xe4, {rng.choice(XMM)}, {rng.choice(XMM)}",
                f"palignr $4, {rng.choice(XMM)}, {rng.choice(XMM)}",
                f"pinsrd $1, {rng.choice(R32)}, {rng.choice(XMM)}",
                f"pinsrq $1, {rng.choice(R64)}, {rng.choice(XMM)}",
                f"pextrd $2, {rng.choice(XMM)}, {rng.choice(R32)}",
                f"pextrq $1, {rng.choice(XMM)}, {rng.choice(R64)}",
                f"pcmpistri $0x08, {rng.choice(XMM)}, {rng.choice(XMM)}",
            ])
        if sub == 3: # AES & PCLMUL
            return rng.choice([
                f"aesenc {rng.choice(XMM)}, {rng.choice(XMM)}",
                f"aesenclast {rand_mem(rng)}, {rng.choice(XMM)}",
                f"aesdec {rng.choice(XMM)}, {rng.choice(XMM)}",
                f"aesimc {rng.choice(XMM)}, {rng.choice(XMM)}",
                f"aeskeygenassist $1, {rng.choice(XMM)}, {rng.choice(XMM)}",
                f"pclmulqdq $0x11, {rng.choice(XMM)}, {rng.choice(XMM)}",
            ])
        # SHA
        return rng.choice([
            f"sha1rnds4 $2, {rng.choice(XMM)}, {rng.choice(XMM)}",
            f"sha1nexte {rng.choice(XMM)}, {rng.choice(XMM)}",
            f"sha1msg1 {rng.choice(XMM)}, {rng.choice(XMM)}",
            f"sha256msg1 {rng.choice(XMM)}, {rng.choice(XMM)}",
            f"sha256msg2 {rand_mem(rng)}, {rng.choice(XMM)}",
        ])
    if cat == 19: # AVX / AVX2 (128-bit & 256-bit)
        regs = rng.choice([XMM, YMM])
        sub = rng.randint(0, 4)
        if sub == 0:
            op = rng.choice(['vaddps', 'vaddpd', 'vsubps', 'vmulpd', 'vdivps', 'vxorps', 'vandpd',
                             'vpaddd', 'vpaddq', 'vpsubw', 'vpmulld', 'vpxor', 'vpand', 'vpor',
                             'vpshufb', 'vpcmpeqd', 'vpmaxsd', 'vpminud', 'vpsllvd', 'vpsrlvq', 'vpsravd'])
            src = rng.choice(regs) if rng.random() < 0.6 else rand_mem(rng)
            return f"{op} {src}, {rng.choice(regs)}, {rng.choice(regs)}"
        if sub == 1:
            op = rng.choice(['vmovaps', 'vmovups', 'vmovapd', 'vmovdqa', 'vmovdqu'])
            if rng.random() < 0.5:
                return f"{op} {rng.choice(regs) if rng.random() < 0.5 else rand_mem(rng)}, {rng.choice(regs)}"
            return f"{op} {rng.choice(regs)}, {rand_mem(rng)}"
        if sub == 2:
            op = rng.choice(['vshufps', 'vcmpps', 'vblendps', 'vpalignr'])
            return f"{op} ${rng.choice([0, 1, 5, 0x1b])}, {rng.choice(regs)}, {rng.choice(regs)}, {rng.choice(regs)}"
        if sub == 3:
            op = rng.choice(['vblendvps', 'vblendvpd', 'vpblendvb'])
            return f"{op} {rng.choice(regs)}, {rng.choice(regs)}, {rng.choice(regs)}, {rng.choice(regs)}"
        return rng.choice([
            'vzeroupper', 'vzeroall',
            f"vbroadcastss {rng.choice(XMM)}, {rng.choice(YMM)}",
            f"vpbroadcastd {rng.choice(XMM)}, {rng.choice(YMM)}",
            f"vperm2f128 $0x21, {rng.choice(YMM)}, {rng.choice(YMM)}, {rng.choice(YMM)}",
            f"vpermq $0xe4, {rng.choice(YMM)}, {rng.choice(YMM)}",
            f"vpermd {rng.choice(YMM)}, {rng.choice(YMM)}, {rng.choice(YMM)}",
            f"vinserti128 $1, {rng.choice(XMM)}, {rng.choice(YMM)}, {rng.choice(YMM)}",
            f"vextracti128 $1, {rng.choice(YMM)}, {rng.choice(XMM)}",
            f"vpslld $3, {rng.choice(regs)}, {rng.choice(regs)}",
            f"vpshufd $0x4e, {rng.choice(regs)}, {rng.choice(regs)}",
        ])
    if cat == 20: # FMA3
        fam = rng.choice(['vfmadd', 'vfmsub', 'vfnmadd', 'vfnmsub'])
        order = rng.choice(['132', '213', '231'])
        sfx = rng.choice(['ps', 'pd', 'ss', 'sd'])
        regs = YMM if (sfx in ('ps', 'pd') and rng.random() < 0.5) else XMM
        src = rng.choice(regs) if rng.random() < 0.6 else rand_mem(rng)
        return f"{fam}{order}{sfx} {src}, {rng.choice(regs)}, {rng.choice(regs)}"
    if cat == 21: # AVX scalar & AES/PCLMUL VEX
        return rng.choice([
            f"vaddss {rng.choice(XMM)}, {rng.choice(XMM)}, {rng.choice(XMM)}",
            f"vmulsd {rand_mem(rng)}, {rng.choice(XMM)}, {rng.choice(XMM)}",
            f"vcvtsi2sdq {rng.choice(R64)}, {rng.choice(XMM)}, {rng.choice(XMM)}",
            f"vaesenc {rng.choice(XMM)}, {rng.choice(XMM)}, {rng.choice(XMM)}",
            f"vpclmulqdq $0x01, {rng.choice(XMM)}, {rng.choice(XMM)}, {rng.choice(XMM)}",
        ])
    if cat == 22: # AVX-512 EVEX (zmm0..31, xmm16..31, ymm16..31, opmasking, {z}, {1toN}, disp8*N)
        sub = rng.randint(0, 5)
        mask = ''
        if rng.random() < 0.45:
            mask = '{' + rng.choice(KMASK) + '}' + ('{z}' if rng.random() < 0.4 else '')
        if sub == 0:
            op = rng.choice(['vaddps', 'vaddpd', 'vsubps', 'vmulpd', 'vdivps', 'vxorps', 'vandpd',
                             'vpaddd', 'vpaddq', 'vpsubd', 'vpmulld', 'vpmullq', 'vpxord', 'vpandq',
                             'vpminsd', 'vpmaxuq', 'vpermd', 'vpsllvd', 'vpsravq',
                             'vfmadd213ps', 'vfmadd231pd', 'vfmsub132ps', 'vfnmadd213pd'])
            regs = rng.choice([ZMM, YMM32, XMM32])
            if rng.random() < 0.5:
                src = rng.choice(regs)
            else:
                d = rng.choice([0, 64, -64, 128, 16, 32, 4, 8, 512, 10000])
                src = f"{d}({rng.choice(R64)})"
                if op in ('vaddps', 'vpaddd', 'vfmadd213ps') and rng.random() < 0.35:
                    src += '{1to16}' if regs is ZMM else ('{1to8}' if regs is YMM32 else '{1to4}')
            return f"{op} {src}, {rng.choice(regs)}, {rng.choice(regs)}{mask}"
        if sub == 1:
            op = rng.choice(['vmovaps', 'vmovups', 'vmovapd', 'vmovdqa32', 'vmovdqa64', 'vmovdqu32', 'vmovdqu64'])
            regs = rng.choice([ZMM, YMM32, XMM32])
            d = rng.choice([0, 64, -64, 128, 16, 32, 1000])
            if rng.random() < 0.5:
                return f"{op} {d}({rng.choice(R64)}), {rng.choice(regs)}{mask}"
            return f"{op} {rng.choice(regs)}, {d}({rng.choice(R64)})"
        if sub == 2:
            op = rng.choice(['vsqrtps', 'vsqrtpd', 'vpabsd', 'vpabsq'])
            return f"{op} {rng.choice(ZMM)}, {rng.choice(ZMM)}{mask}"
        if sub == 3:
            op = rng.choice(['vpslld', 'vpsrld', 'vpsrad', 'vpsllq', 'vpsrlq', 'vpsraq'])
            return f"{op} ${rng.choice([1, 3, 7])}, {rng.choice(ZMM)}, {rng.choice(ZMM)}{mask}"
        if sub == 4:
            op = rng.choice(['vpcmpd', 'vpcmpq', 'vpcmpud', 'vcmpps', 'vcmppd'])
            km = '{' + rng.choice(KMASK) + '}' if rng.random() < 0.4 else ''
            return f"{op} ${rng.choice([0, 1, 2, 4])}, {rng.choice(ZMM)}, {rng.choice(ZMM)}, {rng.choice(KREG)}{km}"
        return rng.choice([
            f"vpshufd $0xe4, {rng.choice(ZMM)}, {rng.choice(ZMM)}{mask}",
            f"vpermq $0x1b, {rng.choice(ZMM)}, {rng.choice(ZMM)}{mask}",
            f"vbroadcastss 4({rng.choice(R64)}), {rng.choice(ZMM)}{mask}",
            f"vpbroadcastq {rng.choice(R64)}, {rng.choice(ZMM)}{mask}",
        ])
    # cat == 23: AVX-512 opmask k0..k7 instructions
    return rng.choice([
        f"kmovw {rng.choice(KREG)}, {rng.choice(KREG)}",
        f"kmovw {rng.choice(R32)}, {rng.choice(KREG)}",
        f"kmovw {rng.choice(KREG)}, {rng.choice(R32)}",
        f"kmovq {rng.choice(R64)}, {rng.choice(KREG)}",
        f"kmovq {rng.choice(KREG)}, {rng.choice(R64)}",
        f"kandw {rng.choice(KREG)}, {rng.choice(KREG)}, {rng.choice(KREG)}",
        f"kandnq {rng.choice(KREG)}, {rng.choice(KREG)}, {rng.choice(KREG)}",
        f"korb {rng.choice(KREG)}, {rng.choice(KREG)}, {rng.choice(KREG)}",
        f"kxord {rng.choice(KREG)}, {rng.choice(KREG)}, {rng.choice(KREG)}",
        f"knotw {rng.choice(KREG)}, {rng.choice(KREG)}",
        f"kortestq {rng.choice(KREG)}, {rng.choice(KREG)}",
        f"kshiftlw $3, {rng.choice(KREG)}, {rng.choice(KREG)}",
        f"kshiftrq $5, {rng.choice(KREG)}, {rng.choice(KREG)}",
    ])

def parse_elf_text_and_relas(data):
    """Minimal ELF64 reader returning (.text bytes, sorted list of (off, rtype, sym_name, addend))."""
    e_shoff = struct.unpack_from('<Q', data, 40)[0]
    e_shentsize, e_shnum, e_shstrndx = struct.unpack_from('<HHH', data, 58)
    shdrs = []
    for i in range(e_shnum):
        shdrs.append(struct.unpack_from('<IIQQQQIIQQ', data, e_shoff + i * e_shentsize))
    shstr_off = shdrs[e_shstrndx][4]
    def get_str(base, idx):
        end = data.index(b'\x00', base + idx)
        return data[base + idx:end].decode('utf-8', 'replace')

    text_bytes = b''
    symtab = None
    strtab_off = 0
    rela_text = None
    for i, sh in enumerate(shdrs):
        name = get_str(shstr_off, sh[0])
        if name == '.text':
            text_bytes = data[sh[4]:sh[4] + sh[5]]
        elif name == '.symtab':
            symtab = sh
        elif name == '.strtab':
            strtab_off = sh[4]
        elif name == '.rela.text':
            rela_text = sh

    relas = []
    if rela_text is not None and symtab is not None:
        r_off, r_sz = rela_text[4], rela_text[5]
        s_off = symtab[4]
        for off in range(r_off, r_off + r_sz, 24):
            r_offset, r_info, r_addend = struct.unpack_from('<QQq', data, off)
            sym_idx = r_info >> 32
            rtype = r_info & 0xffffffff
            st_name, st_info, _, st_shndx, st_val, _ = struct.unpack_from('<IBBHQQ', data, s_off + sym_idx * 24)
            stype = st_info & 0xf
            if stype == 3: # STT_SECTION
                sname = get_str(shstr_off, shdrs[st_shndx][0])
            else:
                sname = get_str(strtab_off, st_name)
            relas.append((r_offset, rtype, sname, r_addend))
    return text_bytes, relas

def run_tool(cmd, asm_text, out_path, *, reference=False):
    if reference:
        p = assemble(cmd, ns="fas-as/fuzz-mc", input=asm_text.encode(),
                     output=out_path, scratch=os.path.dirname(out_path))
    else:
        p = subprocess.run(cmd, input=asm_text.encode(), capture_output=True)
    if p.returncode != 0:
        return None, p.stderr.decode('utf-8', 'replace')
    with open(out_path, 'rb') as f:
        return f.read(), ''

def main():
    fas_as = sys.argv[1]
    seed = int(sys.argv[2]) if len(sys.argv) > 2 else 1
    cases = int(sys.argv[3]) if len(sys.argv) > 3 else 500
    rng = random.Random(seed)

    passed = 0
    failed = 0
    with tempfile.TemporaryDirectory() as td:
        fas_o = os.path.join(td, 'fas.o')
        mc_o = os.path.join(td, 'mc.o')
        for ci in range(cases):
            lines = ['.text']
            n_insns = rng.randint(1, 8)
            for _ in range(n_insns):
                lines.append('  ' + rand_insn(rng))
            lines.append('1:')
            lines.append('  nop')
            asm = '\n'.join(lines) + '\n'

            mc_data, mc_err = run_tool(['llvm-mc-22', '-triple=x86_64-unknown-linux-gnu', '-filetype=obj', '-o', mc_o, '-'], asm, mc_o, reference=True)
            if mc_data is None:
                # Skip invalid random combination rejected by llvm-mc
                continue
            fas_data, fas_err = run_tool([fas_as, '-o', fas_o, '-'], asm, fas_o)
            if fas_data is None:
                failed += 1
                if failed <= 10:
                    print(f"FAIL case {ci} (fas-as error):\n{asm}--- stderr ---\n{fas_err}")
                continue
            mt, mr = parse_elf_text_and_relas(mc_data)
            ft, fr = parse_elf_text_and_relas(fas_data)
            if mt != ft or mr != fr:
                failed += 1
                if failed <= 10:
                    print(f"DIFF case {ci}:\n{asm}  mc  text: {mt.hex()}\n  fas text: {ft.hex()}\n  mc  relas: {mr}\n  fas relas: {fr}")
            else:
                passed += 1
    print(f"fuzz_as (seed={seed}): passed={passed} failed={failed}")
    return 0 if failed == 0 else 1

if __name__ == '__main__':
    sys.exit(main())
