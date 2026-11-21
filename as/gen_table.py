#!/usr/bin/env python3
"""Generate as/table.fas: sorted mnemonic index + compact x86-64 InstrDesc table.

Milestone 1: complete GP integer (8/16/32/64-bit), system instructions, and x87.
"""

from collections import defaultdict

# Operand type constants (must match as/encode.fas)
OT_NONE = 0
OT_R8 = 1
OT_R16 = 2
OT_R32 = 3
OT_R64 = 4
OT_RM8 = 5
OT_RM16 = 6
OT_RM32 = 7
OT_RM64 = 8
OT_M = 9
OT_IMM8 = 10
OT_IMM8S = 11
OT_IMM16 = 12
OT_IMM32 = 13
OT_IMM32S = 14
OT_IMM64 = 15
OT_AL = 16
OT_AX = 17
OT_EAX = 18
OT_RAX = 19
OT_CL = 20
OT_DX = 21
OT_IMM1 = 22
OT_REL8 = 23
OT_REL32 = 24
OT_RELAX = 25
OT_SREG = 26
OT_ST = 27
OT_ST0 = 28
OT_XMM = 29
OT_XM = 30
OT_YMM = 31
OT_YM = 32
OT_MMX = 33
OT_MM = 34
OT_CR = 35
OT_DR = 36
OT_IND_RM64 = 37
OT_M_ANY = 38

# Encoding flags
EF_REXW = 1 << 0
EF_66 = 1 << 1
EF_F2 = 1 << 2
EF_F3 = 1 << 3
EF_MODRM = 1 << 4
EF_REG_EXT = 1 << 5
EF_PLUS_R = 1 << 6
EF_OP0_REG = 1 << 7
EF_OP0_RM = 1 << 8
EF_OP1_REG = 1 << 9
EF_OP1_RM = 1 << 10
EF_OP2_REG = 1 << 11
EF_OP2_RM = 1 << 12
EF_IMM_U8 = 1 << 13
EF_IMM_S8 = 1 << 14
EF_IMM_16 = 1 << 15
EF_IMM_32 = 1 << 16
EF_IMM_32S = 1 << 17
EF_IMM_64 = 1 << 18
EF_VEX = 1 << 19
EF_VEX256 = 1 << 20
EF_VEX_W1 = 1 << 21
EF_VEX_VVVV_0 = 1 << 22
EF_VEX_VVVV_1 = 1 << 23
EF_VEX_VVVV_2 = 1 << 24
EF_IS_JCC = 1 << 25
EF_GOTPCRELX = 1 << 26
EF_FIXED2 = 1 << 27
EF_FIXED3 = 1 << 28
EF_PLUS_ST = 1 << 29
EF_IMM_REG = 1 << 30

MAP_1 = 0
MAP_0F = 1
MAP_0F38 = 2
MAP_0F3A = 3

entries = defaultdict(list)

def add(mnem, ops, omap, opcode, ext, flags):
    op4 = list(ops) + [OT_NONE] * (4 - len(ops))
    entries[mnem].append((len(ops), op4, omap, opcode, ext, flags))

def add_alias(mnem, *aliases):
    for a in aliases:
        entries[a].extend(entries[mnem])

# Condition codes (cc -> nibble 0..15)
CC_MAP = {
    'o': 0x0, 'no': 0x1,
    'b': 0x2, 'nae': 0x2, 'c': 0x2,
    'ae': 0x3, 'nb': 0x3, 'nc': 0x3,
    'e': 0x4, 'z': 0x4,
    'ne': 0x5, 'nz': 0x5,
    'be': 0x6, 'na': 0x6,
    'a': 0x7, 'nbe': 0x7,
    's': 0x8, 'ns': 0x9,
    'p': 0xa, 'pe': 0xa,
    'np': 0xb, 'po': 0xb,
    'l': 0xc, 'nge': 0xc,
    'ge': 0xd, 'nl': 0xd,
    'le': 0xe, 'ng': 0xe,
    'g': 0xf, 'nle': 0xf,
}

# ---------- 1. Standard ALU: add, or, adc, sbb, and, sub, xor, cmp ----------
ALU_OPS = [
    ('add', 0), ('or', 1), ('adc', 2), ('sbb', 3),
    ('and', 4), ('sub', 5), ('xor', 6), ('cmp', 7),
]
for name, ext in ALU_OPS:
    base = ext * 8
    # 8-bit: imm->al (04+base), imm->rm8 (80 /ext), r8->rm8 (base+0), rm8->r8 (base+2)
    for m in (name + 'b', name):
        add(m, [OT_IMM8, OT_AL], MAP_1, base + 4, 0, EF_IMM_U8)
        add(m, [OT_IMM8, OT_RM8], MAP_1, 0x80, ext, EF_MODRM | EF_REG_EXT | EF_OP1_RM | EF_IMM_U8)
        add(m, [OT_R8, OT_RM8], MAP_1, base + 0, 0, EF_MODRM | EF_OP0_REG | EF_OP1_RM)
        add(m, [OT_RM8, OT_R8], MAP_1, base + 2, 0, EF_MODRM | EF_OP0_RM | EF_OP1_REG)
    # 16-bit
    for m in (name + 'w', name):
        add(m, [OT_IMM8S, OT_RM16], MAP_1, 0x83, ext, EF_66 | EF_MODRM | EF_REG_EXT | EF_OP1_RM | EF_IMM_S8)
        add(m, [OT_IMM16, OT_AX], MAP_1, base + 5, 0, EF_66 | EF_IMM_16)
        add(m, [OT_IMM16, OT_RM16], MAP_1, 0x81, ext, EF_66 | EF_MODRM | EF_REG_EXT | EF_OP1_RM | EF_IMM_16)
        add(m, [OT_R16, OT_RM16], MAP_1, base + 1, 0, EF_66 | EF_MODRM | EF_OP0_REG | EF_OP1_RM)
        add(m, [OT_RM16, OT_R16], MAP_1, base + 3, 0, EF_66 | EF_MODRM | EF_OP0_RM | EF_OP1_REG)
    # 32-bit
    for m in (name + 'l', name):
        add(m, [OT_IMM8S, OT_RM32], MAP_1, 0x83, ext, EF_MODRM | EF_REG_EXT | EF_OP1_RM | EF_IMM_S8)
        add(m, [OT_IMM32, OT_EAX], MAP_1, base + 5, 0, EF_IMM_32)
        add(m, [OT_IMM32, OT_RM32], MAP_1, 0x81, ext, EF_MODRM | EF_REG_EXT | EF_OP1_RM | EF_IMM_32)
        add(m, [OT_R32, OT_RM32], MAP_1, base + 1, 0, EF_MODRM | EF_OP0_REG | EF_OP1_RM)
        add(m, [OT_RM32, OT_R32], MAP_1, base + 3, 0, EF_MODRM | EF_OP0_RM | EF_OP1_REG | EF_GOTPCRELX)
    # 64-bit
    for m in (name + 'q', name):
        add(m, [OT_IMM8S, OT_RM64], MAP_1, 0x83, ext, EF_REXW | EF_MODRM | EF_REG_EXT | EF_OP1_RM | EF_IMM_S8)
        add(m, [OT_IMM32S, OT_RAX], MAP_1, base + 5, 0, EF_REXW | EF_IMM_32S)
        add(m, [OT_IMM32S, OT_RM64], MAP_1, 0x81, ext, EF_REXW | EF_MODRM | EF_REG_EXT | EF_OP1_RM | EF_IMM_32S)
        add(m, [OT_R64, OT_RM64], MAP_1, base + 1, 0, EF_REXW | EF_MODRM | EF_OP0_REG | EF_OP1_RM)
        add(m, [OT_RM64, OT_R64], MAP_1, base + 3, 0, EF_REXW | EF_MODRM | EF_OP0_RM | EF_OP1_REG | EF_GOTPCRELX)

# ---------- 2. MOV / MOVABS / MOVSX / MOVZX / MOVSXD / LEA ----------
for m in ('movb', 'mov'):
    add(m, [OT_IMM8, OT_R8], MAP_1, 0xb0, 0, EF_PLUS_R | EF_OP1_RM | EF_IMM_U8)
    add(m, [OT_IMM8, OT_RM8], MAP_1, 0xc6, 0, EF_MODRM | EF_REG_EXT | EF_OP1_RM | EF_IMM_U8)
    add(m, [OT_R8, OT_RM8], MAP_1, 0x88, 0, EF_MODRM | EF_OP0_REG | EF_OP1_RM)
    add(m, [OT_RM8, OT_R8], MAP_1, 0x8a, 0, EF_MODRM | EF_OP0_RM | EF_OP1_REG)
for m in ('movw', 'mov'):
    add(m, [OT_IMM16, OT_R16], MAP_1, 0xb8, 0, EF_66 | EF_PLUS_R | EF_OP1_RM | EF_IMM_16)
    add(m, [OT_IMM16, OT_RM16], MAP_1, 0xc7, 0, EF_66 | EF_MODRM | EF_REG_EXT | EF_OP1_RM | EF_IMM_16)
    add(m, [OT_R16, OT_RM16], MAP_1, 0x89, 0, EF_66 | EF_MODRM | EF_OP0_REG | EF_OP1_RM)
    add(m, [OT_RM16, OT_R16], MAP_1, 0x8b, 0, EF_66 | EF_MODRM | EF_OP0_RM | EF_OP1_REG)
    add(m, [OT_SREG, OT_RM16], MAP_1, 0x8c, 0, EF_66 | EF_MODRM | EF_OP0_REG | EF_OP1_RM)
    add(m, [OT_RM16, OT_SREG], MAP_1, 0x8e, 0, EF_MODRM | EF_OP0_RM | EF_OP1_REG)
for m in ('movl', 'mov'):
    add(m, [OT_IMM32, OT_R32], MAP_1, 0xb8, 0, EF_PLUS_R | EF_OP1_RM | EF_IMM_32)
    add(m, [OT_IMM32, OT_RM32], MAP_1, 0xc7, 0, EF_MODRM | EF_REG_EXT | EF_OP1_RM | EF_IMM_32)
    add(m, [OT_R32, OT_RM32], MAP_1, 0x89, 0, EF_MODRM | EF_OP0_REG | EF_OP1_RM)
    add(m, [OT_RM32, OT_R32], MAP_1, 0x8b, 0, EF_MODRM | EF_OP0_RM | EF_OP1_REG | EF_GOTPCRELX)
    add(m, [OT_SREG, OT_RM32], MAP_1, 0x8c, 0, EF_MODRM | EF_OP0_REG | EF_OP1_RM)
for m in ('movq', 'mov'):
    # Note: llvm-mc uses C7 /0 for movq $imm32s, %r64 (and 48 B8 for movabsq $imm64, %r64 or when imm doesn't fit in i32!)
    add(m, [OT_IMM32S, OT_RM64], MAP_1, 0xc7, 0, EF_REXW | EF_MODRM | EF_REG_EXT | EF_OP1_RM | EF_IMM_32S)
    add(m, [OT_IMM64, OT_R64], MAP_1, 0xb8, 0, EF_REXW | EF_PLUS_R | EF_OP1_RM | EF_IMM_64)
    add(m, [OT_R64, OT_RM64], MAP_1, 0x89, 0, EF_REXW | EF_MODRM | EF_OP0_REG | EF_OP1_RM)
    add(m, [OT_RM64, OT_R64], MAP_1, 0x8b, 0, EF_REXW | EF_MODRM | EF_OP0_RM | EF_OP1_REG | EF_GOTPCRELX)
    add(m, [OT_SREG, OT_RM64], MAP_1, 0x8c, 0, EF_REXW | EF_MODRM | EF_OP0_REG | EF_OP1_RM)
    add(m, [OT_CR, OT_R64], MAP_0F, 0x20, 0, EF_MODRM | EF_OP0_REG | EF_OP1_RM)
    add(m, [OT_R64, OT_CR], MAP_0F, 0x22, 0, EF_MODRM | EF_OP0_RM | EF_OP1_REG)
    add(m, [OT_DR, OT_R64], MAP_0F, 0x21, 0, EF_MODRM | EF_OP0_REG | EF_OP1_RM)
    add(m, [OT_R64, OT_DR], MAP_0F, 0x23, 0, EF_MODRM | EF_OP0_RM | EF_OP1_REG)
for m in ('movabsq', 'movabs'):
    add(m, [OT_IMM64, OT_R64], MAP_1, 0xb8, 0, EF_REXW | EF_PLUS_R | EF_OP1_RM | EF_IMM_64)

# movsx / movzx / movsxd
add('movsbw', [OT_RM8, OT_R16], MAP_0F, 0xbe, 0, EF_66 | EF_MODRM | EF_OP0_RM | EF_OP1_REG)
add('movsbl', [OT_RM8, OT_R32], MAP_0F, 0xbe, 0, EF_MODRM | EF_OP0_RM | EF_OP1_REG)
add('movsbq', [OT_RM8, OT_R64], MAP_0F, 0xbe, 0, EF_REXW | EF_MODRM | EF_OP0_RM | EF_OP1_REG)
add('movswl', [OT_RM16, OT_R32], MAP_0F, 0xbf, 0, EF_MODRM | EF_OP0_RM | EF_OP1_REG)
add('movswq', [OT_RM16, OT_R64], MAP_0F, 0xbf, 0, EF_REXW | EF_MODRM | EF_OP0_RM | EF_OP1_REG)
add('movslq', [OT_RM32, OT_R64], MAP_1, 0x63, 0, EF_REXW | EF_MODRM | EF_OP0_RM | EF_OP1_REG)
add_alias('movslq', 'movsxd', 'movsxdq')
add('movsx', [OT_RM8, OT_R16], MAP_0F, 0xbe, 0, EF_66 | EF_MODRM | EF_OP0_RM | EF_OP1_REG)
add('movsx', [OT_RM8, OT_R32], MAP_0F, 0xbe, 0, EF_MODRM | EF_OP0_RM | EF_OP1_REG)
add('movsx', [OT_RM8, OT_R64], MAP_0F, 0xbe, 0, EF_REXW | EF_MODRM | EF_OP0_RM | EF_OP1_REG)
add('movsx', [OT_RM16, OT_R32], MAP_0F, 0xbf, 0, EF_MODRM | EF_OP0_RM | EF_OP1_REG)
add('movsx', [OT_RM16, OT_R64], MAP_0F, 0xbf, 0, EF_REXW | EF_MODRM | EF_OP0_RM | EF_OP1_REG)
add('movsx', [OT_RM32, OT_R64], MAP_1, 0x63, 0, EF_REXW | EF_MODRM | EF_OP0_RM | EF_OP1_REG)

add('movzbw', [OT_RM8, OT_R16], MAP_0F, 0xb6, 0, EF_66 | EF_MODRM | EF_OP0_RM | EF_OP1_REG)
add('movzbl', [OT_RM8, OT_R32], MAP_0F, 0xb6, 0, EF_MODRM | EF_OP0_RM | EF_OP1_REG)
add('movzbq', [OT_RM8, OT_R64], MAP_0F, 0xb6, 0, EF_REXW | EF_MODRM | EF_OP0_RM | EF_OP1_REG)
add('movzwl', [OT_RM16, OT_R32], MAP_0F, 0xb7, 0, EF_MODRM | EF_OP0_RM | EF_OP1_REG)
add('movzwq', [OT_RM16, OT_R64], MAP_0F, 0xb7, 0, EF_REXW | EF_MODRM | EF_OP0_RM | EF_OP1_REG)
add('movzx', [OT_RM8, OT_R16], MAP_0F, 0xb6, 0, EF_66 | EF_MODRM | EF_OP0_RM | EF_OP1_REG)
add('movzx', [OT_RM8, OT_R32], MAP_0F, 0xb6, 0, EF_MODRM | EF_OP0_RM | EF_OP1_REG)
add('movzx', [OT_RM8, OT_R64], MAP_0F, 0xb6, 0, EF_REXW | EF_MODRM | EF_OP0_RM | EF_OP1_REG)
add('movzx', [OT_RM16, OT_R32], MAP_0F, 0xb7, 0, EF_MODRM | EF_OP0_RM | EF_OP1_REG)
add('movzx', [OT_RM16, OT_R64], MAP_0F, 0xb7, 0, EF_REXW | EF_MODRM | EF_OP0_RM | EF_OP1_REG)

# lea
for m in ('leaw', 'lea'):
    add(m, [OT_M, OT_R16], MAP_1, 0x8d, 0, EF_66 | EF_MODRM | EF_OP0_RM | EF_OP1_REG)
for m in ('leal', 'lea'):
    add(m, [OT_M, OT_R32], MAP_1, 0x8d, 0, EF_MODRM | EF_OP0_RM | EF_OP1_REG)
for m in ('leaq', 'lea'):
    add(m, [OT_M, OT_R64], MAP_1, 0x8d, 0, EF_REXW | EF_MODRM | EF_OP0_RM | EF_OP1_REG)

# ---------- 3. TEST ----------
for m in ('testb', 'test'):
    add(m, [OT_IMM8, OT_AL], MAP_1, 0xa8, 0, EF_IMM_U8)
    add(m, [OT_IMM8, OT_RM8], MAP_1, 0xf6, 0, EF_MODRM | EF_REG_EXT | EF_OP1_RM | EF_IMM_U8)
    add(m, [OT_R8, OT_RM8], MAP_1, 0x84, 0, EF_MODRM | EF_OP0_REG | EF_OP1_RM)
    add(m, [OT_RM8, OT_R8], MAP_1, 0x84, 0, EF_MODRM | EF_OP0_RM | EF_OP1_REG)
for m in ('testw', 'test'):
    add(m, [OT_IMM16, OT_AX], MAP_1, 0xa9, 0, EF_66 | EF_IMM_16)
    add(m, [OT_IMM16, OT_RM16], MAP_1, 0xf7, 0, EF_66 | EF_MODRM | EF_REG_EXT | EF_OP1_RM | EF_IMM_16)
    add(m, [OT_R16, OT_RM16], MAP_1, 0x85, 0, EF_66 | EF_MODRM | EF_OP0_REG | EF_OP1_RM)
    add(m, [OT_RM16, OT_R16], MAP_1, 0x85, 0, EF_66 | EF_MODRM | EF_OP0_RM | EF_OP1_REG)
for m in ('testl', 'test'):
    add(m, [OT_IMM32, OT_EAX], MAP_1, 0xa9, 0, EF_IMM_32)
    add(m, [OT_IMM32, OT_RM32], MAP_1, 0xf7, 0, EF_MODRM | EF_REG_EXT | EF_OP1_RM | EF_IMM_32)
    add(m, [OT_R32, OT_RM32], MAP_1, 0x85, 0, EF_MODRM | EF_OP0_REG | EF_OP1_RM)
    add(m, [OT_RM32, OT_R32], MAP_1, 0x85, 0, EF_MODRM | EF_OP0_RM | EF_OP1_REG)
for m in ('testq', 'test'):
    add(m, [OT_IMM32S, OT_RAX], MAP_1, 0xa9, 0, EF_REXW | EF_IMM_32S)
    add(m, [OT_IMM32S, OT_RM64], MAP_1, 0xf7, 0, EF_REXW | EF_MODRM | EF_REG_EXT | EF_OP1_RM | EF_IMM_32S)
    add(m, [OT_R64, OT_RM64], MAP_1, 0x85, 0, EF_REXW | EF_MODRM | EF_OP0_REG | EF_OP1_RM)
    add(m, [OT_RM64, OT_R64], MAP_1, 0x85, 0, EF_REXW | EF_MODRM | EF_OP0_RM | EF_OP1_REG)

# ---------- 4. Unary: not, neg, mul, imul (1-op), div, idiv, inc, dec ----------
for name, ext in [('not', 2), ('neg', 3), ('mul', 4), ('imul', 5), ('div', 6), ('idiv', 7)]:
    for m in (name + 'b', name):
        add(m, [OT_RM8], MAP_1, 0xf6, ext, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
    for m in (name + 'w', name):
        add(m, [OT_RM16], MAP_1, 0xf7, ext, EF_66 | EF_MODRM | EF_REG_EXT | EF_OP0_RM)
    for m in (name + 'l', name):
        add(m, [OT_RM32], MAP_1, 0xf7, ext, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
    for m in (name + 'q', name):
        add(m, [OT_RM64], MAP_1, 0xf7, ext, EF_REXW | EF_MODRM | EF_REG_EXT | EF_OP0_RM)

for name, ext in [('inc', 0), ('dec', 1)]:
    for m in (name + 'b', name):
        add(m, [OT_RM8], MAP_1, 0xfe, ext, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
    for m in (name + 'w', name):
        add(m, [OT_RM16], MAP_1, 0xff, ext, EF_66 | EF_MODRM | EF_REG_EXT | EF_OP0_RM)
    for m in (name + 'l', name):
        add(m, [OT_RM32], MAP_1, 0xff, ext, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
    for m in (name + 'q', name):
        add(m, [OT_RM64], MAP_1, 0xff, ext, EF_REXW | EF_MODRM | EF_REG_EXT | EF_OP0_RM)

# IMUL 2-op and 3-op forms
for m in ('imulw', 'imul'):
    add(m, [OT_RM16, OT_R16], MAP_0F, 0xaf, 0, EF_66 | EF_MODRM | EF_OP0_RM | EF_OP1_REG)
    add(m, [OT_IMM8S, OT_RM16, OT_R16], MAP_1, 0x6b, 0, EF_66 | EF_MODRM | EF_OP1_RM | EF_OP2_REG | EF_IMM_S8)
    add(m, [OT_IMM16, OT_RM16, OT_R16], MAP_1, 0x69, 0, EF_66 | EF_MODRM | EF_OP1_RM | EF_OP2_REG | EF_IMM_16)
    add(m, [OT_IMM8S, OT_R16], MAP_1, 0x6b, 0, EF_66 | EF_MODRM | EF_OP1_RM | EF_OP1_REG | EF_IMM_S8)
    add(m, [OT_IMM16, OT_R16], MAP_1, 0x69, 0, EF_66 | EF_MODRM | EF_OP1_RM | EF_OP1_REG | EF_IMM_16)
for m in ('imull', 'imul'):
    add(m, [OT_RM32, OT_R32], MAP_0F, 0xaf, 0, EF_MODRM | EF_OP0_RM | EF_OP1_REG)
    add(m, [OT_IMM8S, OT_RM32, OT_R32], MAP_1, 0x6b, 0, EF_MODRM | EF_OP1_RM | EF_OP2_REG | EF_IMM_S8)
    add(m, [OT_IMM32, OT_RM32, OT_R32], MAP_1, 0x69, 0, EF_MODRM | EF_OP1_RM | EF_OP2_REG | EF_IMM_32)
    add(m, [OT_IMM8S, OT_R32], MAP_1, 0x6b, 0, EF_MODRM | EF_OP1_RM | EF_OP1_REG | EF_IMM_S8)
    add(m, [OT_IMM32, OT_R32], MAP_1, 0x69, 0, EF_MODRM | EF_OP1_RM | EF_OP1_REG | EF_IMM_32)
for m in ('imulq', 'imul'):
    add(m, [OT_RM64, OT_R64], MAP_0F, 0xaf, 0, EF_REXW | EF_MODRM | EF_OP0_RM | EF_OP1_REG)
    add(m, [OT_IMM8S, OT_RM64, OT_R64], MAP_1, 0x6b, 0, EF_REXW | EF_MODRM | EF_OP1_RM | EF_OP2_REG | EF_IMM_S8)
    add(m, [OT_IMM32S, OT_RM64, OT_R64], MAP_1, 0x69, 0, EF_REXW | EF_MODRM | EF_OP1_RM | EF_OP2_REG | EF_IMM_32S)
    add(m, [OT_IMM8S, OT_R64], MAP_1, 0x6b, 0, EF_REXW | EF_MODRM | EF_OP1_RM | EF_OP1_REG | EF_IMM_S8)
    add(m, [OT_IMM32S, OT_R64], MAP_1, 0x69, 0, EF_REXW | EF_MODRM | EF_OP1_RM | EF_OP1_REG | EF_IMM_32S)

# ---------- 5. Shifts & Rotates ----------
SHIFT_OPS = [
    ('rol', 0), ('ror', 1), ('rcl', 2), ('rcr', 3),
    ('shl', 4), ('sal', 4), ('shr', 5), ('sar', 7),
]
for name, ext in SHIFT_OPS:
    for m in (name + 'b', name):
        add(m, [OT_RM8], MAP_1, 0xd0, ext, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
        add(m, [OT_IMM1, OT_RM8], MAP_1, 0xd0, ext, EF_MODRM | EF_REG_EXT | EF_OP1_RM)
        add(m, [OT_CL, OT_RM8], MAP_1, 0xd2, ext, EF_MODRM | EF_REG_EXT | EF_OP1_RM)
        add(m, [OT_IMM8, OT_RM8], MAP_1, 0xc0, ext, EF_MODRM | EF_REG_EXT | EF_OP1_RM | EF_IMM_U8)
    for m in (name + 'w', name):
        add(m, [OT_RM16], MAP_1, 0xd1, ext, EF_66 | EF_MODRM | EF_REG_EXT | EF_OP0_RM)
        add(m, [OT_IMM1, OT_RM16], MAP_1, 0xd1, ext, EF_66 | EF_MODRM | EF_REG_EXT | EF_OP1_RM)
        add(m, [OT_CL, OT_RM16], MAP_1, 0xd3, ext, EF_66 | EF_MODRM | EF_REG_EXT | EF_OP1_RM)
        add(m, [OT_IMM8, OT_RM16], MAP_1, 0xc1, ext, EF_66 | EF_MODRM | EF_REG_EXT | EF_OP1_RM | EF_IMM_U8)
    for m in (name + 'l', name):
        add(m, [OT_RM32], MAP_1, 0xd1, ext, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
        add(m, [OT_IMM1, OT_RM32], MAP_1, 0xd1, ext, EF_MODRM | EF_REG_EXT | EF_OP1_RM)
        add(m, [OT_CL, OT_RM32], MAP_1, 0xd3, ext, EF_MODRM | EF_REG_EXT | EF_OP1_RM)
        add(m, [OT_IMM8, OT_RM32], MAP_1, 0xc1, ext, EF_MODRM | EF_REG_EXT | EF_OP1_RM | EF_IMM_U8)
    for m in (name + 'q', name):
        add(m, [OT_RM64], MAP_1, 0xd1, ext, EF_REXW | EF_MODRM | EF_REG_EXT | EF_OP0_RM)
        add(m, [OT_IMM1, OT_RM64], MAP_1, 0xd1, ext, EF_REXW | EF_MODRM | EF_REG_EXT | EF_OP1_RM)
        add(m, [OT_CL, OT_RM64], MAP_1, 0xd3, ext, EF_REXW | EF_MODRM | EF_REG_EXT | EF_OP1_RM)
        add(m, [OT_IMM8, OT_RM64], MAP_1, 0xc1, ext, EF_REXW | EF_MODRM | EF_REG_EXT | EF_OP1_RM | EF_IMM_U8)

# shld / shrd
for name, op_imm, op_cl in [('shld', 0xa4, 0xa5), ('shrd', 0xac, 0xad)]:
    for m in (name + 'w', name):
        add(m, [OT_IMM8, OT_R16, OT_RM16], MAP_0F, op_imm, 0, EF_66 | EF_MODRM | EF_OP1_REG | EF_OP2_RM | EF_IMM_U8)
        add(m, [OT_CL, OT_R16, OT_RM16], MAP_0F, op_cl, 0, EF_66 | EF_MODRM | EF_OP1_REG | EF_OP2_RM)
        add(m, [OT_R16, OT_RM16], MAP_0F, op_cl, 0, EF_66 | EF_MODRM | EF_OP0_REG | EF_OP1_RM)
    for m in (name + 'l', name):
        add(m, [OT_IMM8, OT_R32, OT_RM32], MAP_0F, op_imm, 0, EF_MODRM | EF_OP1_REG | EF_OP2_RM | EF_IMM_U8)
        add(m, [OT_CL, OT_R32, OT_RM32], MAP_0F, op_cl, 0, EF_MODRM | EF_OP1_REG | EF_OP2_RM)
        add(m, [OT_R32, OT_RM32], MAP_0F, op_cl, 0, EF_MODRM | EF_OP0_REG | EF_OP1_RM)
    for m in (name + 'q', name):
        add(m, [OT_IMM8, OT_R64, OT_RM64], MAP_0F, op_imm, 0, EF_REXW | EF_MODRM | EF_OP1_REG | EF_OP2_RM | EF_IMM_U8)
        add(m, [OT_CL, OT_R64, OT_RM64], MAP_0F, op_cl, 0, EF_REXW | EF_MODRM | EF_OP1_REG | EF_OP2_RM)
        add(m, [OT_R64, OT_RM64], MAP_0F, op_cl, 0, EF_REXW | EF_MODRM | EF_OP0_REG | EF_OP1_RM)

# ---------- 6. Push / Pop ----------
for m in ('pushq', 'push'):
    add(m, [OT_R64], MAP_1, 0x50, 0, EF_PLUS_R | EF_OP0_RM)
    add(m, [OT_RM64], MAP_1, 0xff, 6, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
    add(m, [OT_IMM8S], MAP_1, 0x6a, 0, EF_IMM_S8)
    add(m, [OT_IMM32S], MAP_1, 0x68, 0, EF_IMM_32S)
for m in ('pushw', 'push'):
    add(m, [OT_R16], MAP_1, 0x50, 0, EF_66 | EF_PLUS_R | EF_OP0_RM)
    add(m, [OT_RM16], MAP_1, 0xff, 6, EF_66 | EF_MODRM | EF_REG_EXT | EF_OP0_RM)
for m in ('popq', 'pop'):
    add(m, [OT_R64], MAP_1, 0x58, 0, EF_PLUS_R | EF_OP0_RM)
    add(m, [OT_RM64], MAP_1, 0x8f, 0, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
for m in ('popw', 'pop'):
    add(m, [OT_R16], MAP_1, 0x58, 0, EF_66 | EF_PLUS_R | EF_OP0_RM)
    add(m, [OT_RM16], MAP_1, 0x8f, 0, EF_66 | EF_MODRM | EF_REG_EXT | EF_OP0_RM)

# ---------- 7. XCHG / CMPXCHG / XADD / BSWAP / BIT OPS ----------
for m in ('xchgb', 'xchg'):
    add(m, [OT_RM8, OT_R8], MAP_1, 0x86, 0, EF_MODRM | EF_OP0_RM | EF_OP1_REG)
    add(m, [OT_R8, OT_RM8], MAP_1, 0x86, 0, EF_MODRM | EF_OP0_REG | EF_OP1_RM)
for m in ('xchgw', 'xchg'):
    add(m, [OT_R16, OT_AX], MAP_1, 0x90, 0, EF_66 | EF_PLUS_R | EF_OP0_RM)
    add(m, [OT_AX, OT_R16], MAP_1, 0x90, 0, EF_66 | EF_PLUS_R | EF_OP1_RM)
    add(m, [OT_RM16, OT_R16], MAP_1, 0x87, 0, EF_66 | EF_MODRM | EF_OP0_RM | EF_OP1_REG)
    add(m, [OT_R16, OT_RM16], MAP_1, 0x87, 0, EF_66 | EF_MODRM | EF_OP0_REG | EF_OP1_RM)
for m in ('xchgl', 'xchg'):
    add(m, [OT_R32, OT_EAX], MAP_1, 0x90, 0, EF_PLUS_R | EF_OP0_RM)
    add(m, [OT_EAX, OT_R32], MAP_1, 0x90, 0, EF_PLUS_R | EF_OP1_RM)
    add(m, [OT_RM32, OT_R32], MAP_1, 0x87, 0, EF_MODRM | EF_OP0_RM | EF_OP1_REG)
    add(m, [OT_R32, OT_RM32], MAP_1, 0x87, 0, EF_MODRM | EF_OP0_REG | EF_OP1_RM)
for m in ('xchgq', 'xchg'):
    add(m, [OT_R64, OT_RAX], MAP_1, 0x90, 0, EF_REXW | EF_PLUS_R | EF_OP0_RM)
    add(m, [OT_RAX, OT_R64], MAP_1, 0x90, 0, EF_REXW | EF_PLUS_R | EF_OP1_RM)
    add(m, [OT_RM64, OT_R64], MAP_1, 0x87, 0, EF_REXW | EF_MODRM | EF_OP0_RM | EF_OP1_REG)
    add(m, [OT_R64, OT_RM64], MAP_1, 0x87, 0, EF_REXW | EF_MODRM | EF_OP0_REG | EF_OP1_RM)

for name, op8, opv in [('cmpxchg', 0xb0, 0xb1), ('xadd', 0xc0, 0xc1)]:
    for m in (name + 'b', name):
        add(m, [OT_R8, OT_RM8], MAP_0F, op8, 0, EF_MODRM | EF_OP0_REG | EF_OP1_RM)
    for m in (name + 'w', name):
        add(m, [OT_R16, OT_RM16], MAP_0F, opv, 0, EF_66 | EF_MODRM | EF_OP0_REG | EF_OP1_RM)
    for m in (name + 'l', name):
        add(m, [OT_R32, OT_RM32], MAP_0F, opv, 0, EF_MODRM | EF_OP0_REG | EF_OP1_RM)
    for m in (name + 'q', name):
        add(m, [OT_R64, OT_RM64], MAP_0F, opv, 0, EF_REXW | EF_MODRM | EF_OP0_REG | EF_OP1_RM)

add('cmpxchg8b', [OT_M], MAP_0F, 0xc7, 1, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
add('cmpxchg16b', [OT_M], MAP_0F, 0xc7, 1, EF_REXW | EF_MODRM | EF_REG_EXT | EF_OP0_RM)

for m in ('bswapl', 'bswap'):
    add(m, [OT_R32], MAP_0F, 0xc8, 0, EF_PLUS_R | EF_OP0_RM)
for m in ('bswapq', 'bswap'):
    add(m, [OT_R64], MAP_0F, 0xc8, 0, EF_REXW | EF_PLUS_R | EF_OP0_RM)

# bt / bts / btr / btc
for name, op_r, ext_i in [('bt', 0xa3, 4), ('bts', 0xab, 5), ('btr', 0xb3, 6), ('btc', 0xbb, 7)]:
    for m in (name + 'w', name):
        add(m, [OT_R16, OT_RM16], MAP_0F, op_r, 0, EF_66 | EF_MODRM | EF_OP0_REG | EF_OP1_RM)
        add(m, [OT_IMM8, OT_RM16], MAP_0F, 0xba, ext_i, EF_66 | EF_MODRM | EF_REG_EXT | EF_OP1_RM | EF_IMM_U8)
    for m in (name + 'l', name):
        add(m, [OT_R32, OT_RM32], MAP_0F, op_r, 0, EF_MODRM | EF_OP0_REG | EF_OP1_RM)
        add(m, [OT_IMM8, OT_RM32], MAP_0F, 0xba, ext_i, EF_MODRM | EF_REG_EXT | EF_OP1_RM | EF_IMM_U8)
    for m in (name + 'q', name):
        add(m, [OT_R64, OT_RM64], MAP_0F, op_r, 0, EF_REXW | EF_MODRM | EF_OP0_REG | EF_OP1_RM)
        add(m, [OT_IMM8, OT_RM64], MAP_0F, 0xba, ext_i, EF_REXW | EF_MODRM | EF_REG_EXT | EF_OP1_RM | EF_IMM_U8)

# bsf / bsr
for name, op in [('bsf', 0xbc), ('bsr', 0xbd)]:
    for m in (name + 'w', name):
        add(m, [OT_RM16, OT_R16], MAP_0F, op, 0, EF_66 | EF_MODRM | EF_OP0_RM | EF_OP1_REG)
    for m in (name + 'l', name):
        add(m, [OT_RM32, OT_R32], MAP_0F, op, 0, EF_MODRM | EF_OP0_RM | EF_OP1_REG)
    for m in (name + 'q', name):
        add(m, [OT_RM64, OT_R64], MAP_0F, op, 0, EF_REXW | EF_MODRM | EF_OP0_RM | EF_OP1_REG)

# ---------- 8. CMOVcc, SETcc, Jcc, JMP, CALL, RET, LOOP ----------
for cc, nib in CC_MAP.items():
    for m in ('cmov' + cc + 'w', 'cmov' + cc):
        add(m, [OT_RM16, OT_R16], MAP_0F, 0x40 + nib, 0, EF_66 | EF_MODRM | EF_OP0_RM | EF_OP1_REG)
    for m in ('cmov' + cc + 'l', 'cmov' + cc):
        add(m, [OT_RM32, OT_R32], MAP_0F, 0x40 + nib, 0, EF_MODRM | EF_OP0_RM | EF_OP1_REG)
    for m in ('cmov' + cc + 'q', 'cmov' + cc):
        add(m, [OT_RM64, OT_R64], MAP_0F, 0x40 + nib, 0, EF_REXW | EF_MODRM | EF_OP0_RM | EF_OP1_REG)
    for m in ('set' + cc, 'set' + cc + 'b'):
        add(m, [OT_RM8], MAP_0F, 0x90 + nib, 0, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
    add('j' + cc, [OT_RELAX], MAP_1, 0x70 + nib, 0x80 + nib, EF_IS_JCC)

for m in ('jmp', 'jmpq'):
    add(m, [OT_RELAX], MAP_1, 0xeb, 0xe9, 0)
    add(m, [OT_IND_RM64], MAP_1, 0xff, 4, EF_MODRM | EF_REG_EXT | EF_OP0_RM | EF_GOTPCRELX)

for m in ('call', 'callq'):
    add(m, [OT_REL32], MAP_1, 0xe8, 0, 0)
    add(m, [OT_IND_RM64], MAP_1, 0xff, 2, EF_MODRM | EF_REG_EXT | EF_OP0_RM | EF_GOTPCRELX)

for m in ('ret', 'retq'):
    add(m, [], MAP_1, 0xc3, 0, 0)
    add(m, [OT_IMM16], MAP_1, 0xc2, 0, EF_IMM_16)

add('loop', [OT_REL8], MAP_1, 0xe2, 0, 0)
add('loope', [OT_REL8], MAP_1, 0xe1, 0, 0)
add('loopz', [OT_REL8], MAP_1, 0xe1, 0, 0)
add('loopne', [OT_REL8], MAP_1, 0xe0, 0, 0)
add('loopnz', [OT_REL8], MAP_1, 0xe0, 0, 0)
add('jrcxz', [OT_REL8], MAP_1, 0xe3, 0, 0)

# ---------- 9. String, Flag, Sign-extend, Misc & System Instructions ----------
ZERO_OP_1 = [
    ('nop', 0x90, 0), ('leave', 0xc9, 0), ('leaveq', 0xc9, 0),
    ('int3', 0xcc, 0), ('hlt', 0xf4, 0), ('cmc', 0xf5, 0),
    ('clc', 0xf8, 0), ('stc', 0xf9, 0), ('cli', 0xfa, 0), ('sti', 0xfb, 0),
    ('cld', 0xfc, 0), ('std', 0xfd, 0), ('lahf', 0x9f, 0), ('sahf', 0x9e, 0),
    ('pushf', 0x9c, 0), ('pushfq', 0x9c, 0), ('pushfw', 0x9c, EF_66),
    ('popf', 0x9d, 0), ('popfq', 0x9d, 0), ('popfw', 0x9d, EF_66),
    ('cbw', 0x98, EF_66), ('cbtw', 0x98, EF_66),
    ('cwde', 0x98, 0), ('cwtl', 0x98, 0),
    ('cdqe', 0x98, EF_REXW), ('cltq', 0x98, EF_REXW),
    ('cwd', 0x99, EF_66), ('cwtd', 0x99, EF_66),
    ('cdq', 0x99, 0), ('cltd', 0x99, 0),
    ('cqo', 0x99, EF_REXW), ('cqto', 0x99, EF_REXW),
    ('movsb', 0xa4, 0), ('movsw', 0xa5, EF_66), ('movsl', 0xa5, 0), ('movsq', 0xa5, EF_REXW),
    ('cmpsb', 0xa6, 0), ('cmpsw', 0xa7, EF_66), ('cmpsl', 0xa7, 0), ('cmpsq', 0xa7, EF_REXW),
    ('stosb', 0xaa, 0), ('stosw', 0xab, EF_66), ('stosl', 0xab, 0), ('stosq', 0xab, EF_REXW),
    ('lodsb', 0xac, 0), ('lodsw', 0xad, EF_66), ('lodsl', 0xad, 0), ('lodsq', 0xad, EF_REXW),
    ('scasb', 0xae, 0), ('scasw', 0xaf, EF_66), ('scasl', 0xaf, 0), ('scasq', 0xaf, EF_REXW),
    ('xlatb', 0xd7, 0), ('xlat', 0xd7, 0),
    ('iret', 0xcf, 0), ('iretl', 0xcf, 0), ('iretq', 0xcf, EF_REXW),
    ('pause', 0x90, EF_F3),
    ('wait', 0x9b, 0), ('fwait', 0x9b, 0),
]
for m, op, fl in ZERO_OP_1:
    add(m, [], MAP_1, op, 0, fl)

# Multi-byte nop forms (nopl/nopw/nopq r/m)
for m in ('nopw', 'nop'):
    add(m, [OT_RM16], MAP_0F, 0x1f, 0, EF_66 | EF_MODRM | EF_REG_EXT | EF_OP0_RM)
for m in ('nopl', 'nop'):
    add(m, [OT_RM32], MAP_0F, 0x1f, 0, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
for m in ('nopq', 'nop'):
    add(m, [OT_RM64], MAP_0F, 0x1f, 0, EF_REXW | EF_MODRM | EF_REG_EXT | EF_OP0_RM)

add('int', [OT_IMM8], MAP_1, 0xcd, 0, EF_IMM_U8)

# 2-byte 0F xx zero-op system instructions
ZERO_OP_0F = [
    ('syscall', 0x05, 0), ('sysret', 0x07, 0), ('sysretl', 0x07, 0), ('sysretq', 0x07, EF_REXW),
    ('ud2', 0x0b, 0), ('cpuid', 0xa2, 0), ('rdtsc', 0x31, 0),
    ('wrmsr', 0x30, 0), ('rdmsr', 0x32, 0), ('rdpmc', 0x33, 0),
    ('wbinvd', 0x09, 0), ('invd', 0x08, 0), ('clts', 0x06, 0),
    ('emms', 0x77, 0),
]
for m, op, fl in ZERO_OP_0F:
    add(m, [], MAP_0F, op, 0, fl)

# 3-byte 0F xx yy zero-op system instructions
ZERO_OP_3 = [
    ('lfence', 0xae, 0xe8, 0),
    ('mfence', 0xae, 0xf0, 0),
    ('sfence', 0xae, 0xf8, 0),
    ('rdtscp', 0x01, 0xf9, 0),
    ('xgetbv', 0x01, 0xd0, 0),
    ('xsetbv', 0x01, 0xd1, 0),
    ('swapgs', 0x01, 0xf8, 0),
    ('endbr64', 0x1e, 0xfa, EF_F3),
    ('endbr32', 0x1e, 0xfb, EF_F3),
    ('stac', 0x01, 0xcb, 0),
    ('clac', 0x01, 0xca, 0),
]
for m, op, ext, fl in ZERO_OP_3:
    add(m, [], MAP_0F, op, ext, fl | EF_FIXED3)

# Memory-operand system instructions
add('clflush', [OT_M], MAP_0F, 0xae, 7, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
add('clflushopt', [OT_M], MAP_0F, 0xae, 7, EF_66 | EF_MODRM | EF_REG_EXT | EF_OP0_RM)
add('prefetchnta', [OT_M], MAP_0F, 0x18, 0, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
add('prefetcht0', [OT_M], MAP_0F, 0x18, 1, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
add('prefetcht1', [OT_M], MAP_0F, 0x18, 2, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
add('prefetcht2', [OT_M], MAP_0F, 0x18, 3, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
add('prefetchw', [OT_M], MAP_0F, 0x0d, 1, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
add('invlpg', [OT_M], MAP_0F, 0x01, 7, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
add('fxsave', [OT_M], MAP_0F, 0xae, 0, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
add('fxsave64', [OT_M], MAP_0F, 0xae, 0, EF_REXW | EF_MODRM | EF_REG_EXT | EF_OP0_RM)
add('fxrstor', [OT_M], MAP_0F, 0xae, 1, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
add('fxrstor64', [OT_M], MAP_0F, 0xae, 1, EF_REXW | EF_MODRM | EF_REG_EXT | EF_OP0_RM)
add('ldmxcsr', [OT_M], MAP_0F, 0xae, 2, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
add('stmxcsr', [OT_M], MAP_0F, 0xae, 3, EF_MODRM | EF_REG_EXT | EF_OP0_RM)

# I/O port instructions
for m in ('inb', 'in'):
    add(m, [OT_IMM8, OT_AL], MAP_1, 0xe4, 0, EF_IMM_U8)
    add(m, [OT_DX, OT_AL], MAP_1, 0xec, 0, 0)
for m in ('inw', 'in'):
    add(m, [OT_IMM8, OT_AX], MAP_1, 0xe5, 0, EF_66 | EF_IMM_U8)
    add(m, [OT_DX, OT_AX], MAP_1, 0xed, 0, EF_66)
for m in ('inl', 'in'):
    add(m, [OT_IMM8, OT_EAX], MAP_1, 0xe5, 0, EF_IMM_U8)
    add(m, [OT_DX, OT_EAX], MAP_1, 0xed, 0, 0)
for m in ('outb', 'out'):
    add(m, [OT_AL, OT_IMM8], MAP_1, 0xe6, 0, EF_IMM_U8)
    add(m, [OT_AL, OT_DX], MAP_1, 0xee, 0, 0)
for m in ('outw', 'out'):
    add(m, [OT_AX, OT_IMM8], MAP_1, 0xe7, 0, EF_66 | EF_IMM_U8)
    add(m, [OT_AX, OT_DX], MAP_1, 0xef, 0, EF_66)
for m in ('outl', 'out'):
    add(m, [OT_EAX, OT_IMM8], MAP_1, 0xe7, 0, EF_IMM_U8)
    add(m, [OT_EAX, OT_DX], MAP_1, 0xef, 0, 0)

# ---------- 10. x87 Floating-Point Instructions ----------
X87_ZERO = [
    ('fchs', 0xd9, 0xe0), ('fabs', 0xd9, 0xe1), ('ftst', 0xd9, 0xe4), ('fxam', 0xd9, 0xe5),
    ('fld1', 0xd9, 0xe8), ('fldl2t', 0xd9, 0xe9), ('fldl2e', 0xd9, 0xea), ('fldpi', 0xd9, 0xeb),
    ('fldlg2', 0xd9, 0xec), ('fldln2', 0xd9, 0xed), ('fldz', 0xd9, 0xee),
    ('f2xm1', 0xd9, 0xf0), ('fyl2x', 0xd9, 0xf1), ('fptan', 0xd9, 0xf2), ('fpatan', 0xd9, 0xf3),
    ('fxtract', 0xd9, 0xf4), ('fprem1', 0xd9, 0xf5), ('fdecstp', 0xd9, 0xf6), ('fincstp', 0xd9, 0xf7),
    ('fprem', 0xd9, 0xf8), ('fyl2xp1', 0xd9, 0xf9), ('fsqrt', 0xd9, 0xfa), ('fsincos', 0xd9, 0xfb),
    ('frndint', 0xd9, 0xfc), ('fscale', 0xd9, 0xfd), ('fsin', 0xd9, 0xfe), ('fcos', 0xd9, 0xff),
    ('fnop', 0xd9, 0xd0), ('fnclex', 0xdb, 0xe2), ('fninit', 0xdb, 0xe3),
    ('fcompp', 0xde, 0xd9), ('fucompp', 0xda, 0xe9),
]
for m, b1, b2 in X87_ZERO:
    add(m, [], MAP_1, b1, b2, EF_FIXED2)

# x87 memory loads/stores
add('flds', [OT_M], MAP_1, 0xd9, 0, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
add('fldl', [OT_M], MAP_1, 0xdd, 0, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
add('fldt', [OT_M], MAP_1, 0xdb, 5, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
add('fld', [OT_ST], MAP_1, 0xd9, 0xc0, EF_PLUS_ST | EF_OP0_RM)
add('fsts', [OT_M], MAP_1, 0xd9, 2, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
add('fstl', [OT_M], MAP_1, 0xdd, 2, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
add('fst', [OT_ST], MAP_1, 0xdd, 0xd0, EF_PLUS_ST | EF_OP0_RM)
add('fstps', [OT_M], MAP_1, 0xd9, 3, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
add('fstpl', [OT_M], MAP_1, 0xdd, 3, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
add('fstpt', [OT_M], MAP_1, 0xdb, 7, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
add('fstp', [OT_ST], MAP_1, 0xdd, 0xd8, EF_PLUS_ST | EF_OP0_RM)
add('filds', [OT_M], MAP_1, 0xdf, 0, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
add('fildl', [OT_M], MAP_1, 0xdb, 0, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
add('fildll', [OT_M], MAP_1, 0xdf, 5, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
add_alias('fildll', 'fildq')
add('fists', [OT_M], MAP_1, 0xdf, 2, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
add('fistl', [OT_M], MAP_1, 0xdb, 2, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
add('fistps', [OT_M], MAP_1, 0xdf, 3, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
add('fistpl', [OT_M], MAP_1, 0xdb, 3, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
add('fistpll', [OT_M], MAP_1, 0xdf, 7, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
add_alias('fistpll', 'fistpq')
add('fisttps', [OT_M], MAP_1, 0xdf, 1, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
add('fisttpl', [OT_M], MAP_1, 0xdb, 1, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
add('fisttpll', [OT_M], MAP_1, 0xdd, 1, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
add_alias('fisttpll', 'fisttpq')
add('fldcw', [OT_M], MAP_1, 0xd9, 5, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
add('fnstcw', [OT_M], MAP_1, 0xd9, 7, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
add('fnstsw', [OT_M], MAP_1, 0xdd, 7, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
add('fnstsw', [OT_AX], MAP_1, 0xdf, 0xe0, EF_FIXED2)
add('fxch', [], MAP_1, 0xd9, 0xc9, EF_FIXED2)
add('fxch', [OT_ST], MAP_1, 0xd9, 0xc8, EF_PLUS_ST | EF_OP0_RM)
add('ffree', [OT_ST], MAP_1, 0xdd, 0xc0, EF_PLUS_ST | EF_OP0_RM)

# x87 arithmetic (mem + reg forms; note AT&T sub/div quirk matches llvm-mc!)
for name, mext, r_to_st0, st0_to_r, pop_op in [
    ('fadd',  0, (0xd8, 0xc0), (0xdc, 0xc0), (0xde, 0xc0)),
    ('fmul',  1, (0xd8, 0xc8), (0xdc, 0xc8), (0xde, 0xc8)),
    ('fsub',  4, (0xd8, 0xe0), (0xdc, 0xe0), (0xde, 0xe0)),
    ('fsubr', 5, (0xd8, 0xe8), (0xdc, 0xe8), (0xde, 0xe8)),
    ('fdiv',  6, (0xd8, 0xf0), (0xdc, 0xf0), (0xde, 0xf0)),
    ('fdivr', 7, (0xd8, 0xf8), (0xdc, 0xf8), (0xde, 0xf8)),
]:
    add(name + 's', [OT_M], MAP_1, 0xd8, mext, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
    add(name + 'l', [OT_M], MAP_1, 0xdc, mext, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
    add(name, [OT_ST, OT_ST0], MAP_1, r_to_st0[0], r_to_st0[1], EF_PLUS_ST | EF_OP0_RM)
    add(name, [OT_ST0, OT_ST], MAP_1, st0_to_r[0], st0_to_r[1], EF_PLUS_ST | EF_OP1_RM)
    add(name + 'p', [], MAP_1, pop_op[0], pop_op[1] + 1, EF_FIXED2)
    add(name + 'p', [OT_ST0, OT_ST], MAP_1, pop_op[0], pop_op[1], EF_PLUS_ST | EF_OP1_RM)
    add(name + 'p', [OT_ST], MAP_1, pop_op[0], pop_op[1], EF_PLUS_ST | EF_OP0_RM)

# x87 compare
add('fcom', [], MAP_1, 0xd8, 0xd1, EF_FIXED2)
add('fcom', [OT_ST], MAP_1, 0xd8, 0xd0, EF_PLUS_ST | EF_OP0_RM)
add('fcoms', [OT_M], MAP_1, 0xd8, 2, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
add('fcoml', [OT_M], MAP_1, 0xdc, 2, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
add('fcomp', [], MAP_1, 0xd8, 0xd9, EF_FIXED2)
add('fcomp', [OT_ST], MAP_1, 0xd8, 0xd8, EF_PLUS_ST | EF_OP0_RM)
add('fcomps', [OT_M], MAP_1, 0xd8, 3, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
add('fcompl', [OT_M], MAP_1, 0xdc, 3, EF_MODRM | EF_REG_EXT | EF_OP0_RM)
add('fucom', [], MAP_1, 0xdd, 0xe1, EF_FIXED2)
add('fucom', [OT_ST], MAP_1, 0xdd, 0xe0, EF_PLUS_ST | EF_OP0_RM)
add('fucomp', [], MAP_1, 0xdd, 0xe9, EF_FIXED2)
add('fucomp', [OT_ST], MAP_1, 0xdd, 0xe8, EF_PLUS_ST | EF_OP0_RM)
add('fcomi', [OT_ST, OT_ST0], MAP_1, 0xdb, 0xf0, EF_PLUS_ST | EF_OP0_RM)
add('fcomi', [OT_ST], MAP_1, 0xdb, 0xf0, EF_PLUS_ST | EF_OP0_RM)
add('fcomip', [OT_ST, OT_ST0], MAP_1, 0xdf, 0xf0, EF_PLUS_ST | EF_OP0_RM)
add('fcomip', [OT_ST], MAP_1, 0xdf, 0xf0, EF_PLUS_ST | EF_OP0_RM)
add('fucomi', [OT_ST, OT_ST0], MAP_1, 0xdb, 0xe8, EF_PLUS_ST | EF_OP0_RM)
add('fucomi', [OT_ST], MAP_1, 0xdb, 0xe8, EF_PLUS_ST | EF_OP0_RM)
add('fucomip', [OT_ST, OT_ST0], MAP_1, 0xdf, 0xe8, EF_PLUS_ST | EF_OP0_RM)
add('fucomip', [OT_ST], MAP_1, 0xdf, 0xe8, EF_PLUS_ST | EF_OP0_RM)

# Emit as/table.fas
mnems = sorted(entries.keys())
flat_descs = []
mnem_rows = []
for m in mnems:
    start = len(flat_descs)
    rows = entries[m]
    flat_descs.extend(rows)
    mnem_rows.append((m, start, len(rows)))

# Pack all mnemonic names into one contiguous byte string for zero-allocation binary search
str_blob = bytearray()
mnem_meta = []
for m, start, count in mnem_rows:
    off = len(str_blob)
    b = m.encode('ascii')
    str_blob.extend(b)
    mnem_meta.append((off, len(b), start, count))

lines = []
lines.append("// AUTO-GENERATED by as/gen_table.py — do not hand-edit.")
lines.append("// Sorted mnemonic table + compact x86-64 instruction encoding descriptors.")
lines.append("")
lines.append("struct InstrDesc {")
lines.append("    op_count u8")
lines.append("    op0 u8")
lines.append("    op1 u8")
lines.append("    op2 u8")
lines.append("    op3 u8")
lines.append("    omap u8")
lines.append("    opcode u8")
lines.append("    ext u8")
lines.append("    flags u32")
lines.append("    pad u32")
lines.append("}")
lines.append("")
lines.append("struct MnemEntry {")
lines.append("    name_off u32")
lines.append("    name_len u16")
lines.append("    count u16")
lines.append("    start u32")
lines.append("}")
lines.append("")
lines.append(f"const MNEM_COUNT u64 = {len(mnem_meta)}")
lines.append(f"const INSTR_COUNT u64 = {len(flat_descs)}")
lines.append(f"const MNEM_STR_LEN u64 = {len(str_blob)}")
lines.append("")

# Emit MNEM_STR as byte string literal
escaped = ''.join(chr(c) for c in str_blob)
lines.append(f'const MNEM_STR arr[{len(str_blob)}, u8] = b"{escaped}"')
lines.append("")

lines.append(f"const MNEM_TABLE arr[{len(mnem_meta)}, MnemEntry] = {{")
for off, ln, start, count in mnem_meta:
    lines.append(f"    {{ {off}, {ln}, {count}, {start} }},")
lines.append("}")
lines.append("")

lines.append(f"const INSTR_TABLE arr[{len(flat_descs)}, InstrDesc] = {{")
for cnt, op4, omap, opcode, ext, flags in flat_descs:
    lines.append(f"    {{ {cnt}, {op4[0]}, {op4[1]}, {op4[2]}, {op4[3]}, {omap}, 0x{opcode:02x}, 0x{ext:02x}, 0x{flags:08x}, 0 }},")
lines.append("}")
lines.append("")

out_path = "/home/glonex/fas-experimental-backend/ld-ergo/as/table.fas"
with open(out_path, "w") as f:
    f.write("\n".join(lines))

print(f"Generated {out_path}: {len(mnem_meta)} mnemonics, {len(flat_descs)} forms, {len(str_blob)} name bytes")