#!/usr/bin/env bash
# Exercise executable linker scripts with x86-64 relocatable inputs.
set -euo pipefail
ulimit -f 524288

root=$(cd -- "$(dirname -- "$0")/.." && pwd)
ld=${FAS_LD:-$root/_build/fas-ld}
cc=${CC:-clang-22}
work=$(mktemp -d "${TMPDIR:-/tmp}/fas-ld-script.XXXXXX")
trap 'rm -rf -- "$work"' EXIT
cd "$work"

cat > main.s <<'ASM'
.globl _start
.text
_start:
    call helper
    mov %eax, %edi
    mov $60, %eax
    syscall
.globl second
second:
    mov $17, %edi
    mov $60, %eax
    syscall

.section .parts.z,"a"
.byte 0x7a
.section .parts.a,"a"
.byte 0x61
.section .both.b,"a"
.byte 0x42
.section .both.a,"a"
.byte 0x41

.section .note.test,"a",@note
.p2align 2
.long 5, 4, 1
.asciz "Test"
.p2align 2
.long 0
.p2align 2

.data
.globl data_value
.type data_value,@object
data_value:
    .quad .Llocal + 5
    .quad fallback
.Llocal:
    .byte 0
.size data_value, .-data_value

.bss
.p2align 4
.zero 16

.section .metadata,"",@progbits
.byte 0xab
.quad .Ldiscarded + 3
.section .discard.test,"a"
.byte 0
.Ldiscarded:
.globl ignored
ignored:
    .byte 0
.section .note.GNU-stack,"",@progbits
ASM
cat > helper.s <<'ASM'
.section .text.helper,"ax"
.p2align 4
.globl helper
.hidden helper
.type helper,@function
helper:
    mov $42, %eax
    ret
.size helper, .-helper
ASM
"$cc" -c main.s -o main.o
"$cc" -c helper.s -o helper.o

cat > layout.lds <<'LDS'
/* Linker-script integration fixture. */
OUTPUT_FORMAT("elf64-x86-64")
OUTPUT_ARCH(i386:x86-64)
ENTRY(_start)
alias = +data_value;
aligned_alias = ALIGN(data_value, 8);
constant = 1;
constant += 2;
PHDRS {
  code PT_LOAD FLAGS(5);
  data PT_LOAD FLAGS(6);
  note PT_NOTE FLAGS(4);
}
SECTIONS {
  . = 0x401000;
  .text : AT(ADDR(.text) - 0x200000) ALIGN(16) {
    *(.text .text.helper)
    . = ALIGN(., 16);
    before_fill = .;
    . += 8;
  } :code = 0x12345678
  # Use input order for patterns without SORT.
  .rodata : {
    begin = .;
    KEEP(*(SORT(.parts.*)))
    *(.both.a .both.b)
    end = .;
  }
  .notes : { KEEP(*(.note.*)) } :code :note
  /DISCARD/ : { *(.note.GNU-stack) *(.discard.*) }
  . = ALIGN(0x1000);
  .data : { *(.data) CONSTRUCTORS } :data
  .bss : { *(.bss) }
  after_bss = .;
  assert_dot = ASSERT(after_bss != 0, "dot after BSS");
  .info (INFO) : { *(.metadata) }
  .unused : { *(.no_such_section) }
  ASSERT(end - begin == 4, "rodata size")
  ASSERT(SIZEOF(.unused) == 0, "unused size")
}
PROVIDE(fallback = fallback_value)
PROVIDE(fallback_value = ABSOLUTE(ADDR(.data)) | 1);
PROVIDE(unreferenced = nonexistent);
. = ASSERT(constant == 3 && SIZEOF(.bss) == 16, "section checks");
LDS

export FAS_LD_THREADS=1
"$ld" --script=layout.lds --emit-relocs --discard-none --orphan-handling=error     --build-id=sha1 main.o helper.o -o image
FAS_LD_THREADS=3 "$ld" -T layout.lds -q --discard-none --orphan-handling=error     --build-id=sha1 main.o helper.o -o image.parallel
cmp image image.parallel

expect_exit() {
    local want=$1 actual=0
    shift
    "$@" || actual=$?
    if [[ $want != "$actual" ]]; then
        echo "expected exit $want, got $actual from $*" >&2
        return 1
    fi
}
expect_exit 42 ./image
"$ld" --script layout.lds -e second main.o helper.o -o override
expect_exit 17 ./override

python3 - <<'PY'
from pathlib import Path
import struct

image = Path("image").read_bytes()
entry, phoff, shoff = struct.unpack_from("<3Q", image, 24)
ph_count, _, sh_count, sh_names = struct.unpack_from("<4H", image, 56)
assert (entry, ph_count) == (0x401000, 3)
sections = [struct.unpack_from("<2I4Q2I2Q", image, shoff + i*64) for i in range(sh_count)]
names = sections[sh_names][4]
def string(table, offset):
    return image[table + offset:image.index(b"\0", table + offset)].decode()
by_name = {string(names, sh[0]): (i, sh) for i, sh in enumerate(sections)}
def content(name):
    sh = by_name[name][1]
    return image[sh[4]:sh[4]+sh[5]]

assert content(".rodata") == b"azBA"
assert struct.unpack("<BQ", content(".info")) == (0xab, 4)
assert by_name[".info"][1][2:4] == (0, 0)
assert struct.unpack("<3Q", content(".rela.info")) == (1, 0, 4)
assert by_name[".bss"][1][1] == 8
assert b"GNU\0" in content(".notes")
assert ".unused" not in by_name and ".discard.test" not in by_name
assert struct.unpack_from("<4Q", image, phoff + 8)[:3] == (0x1000, 0x401000, 0x201000)
assert struct.unpack_from("<I", image, phoff + 2*56)[0] == 4

strings = by_name[".strtab"][1][4]
symtab = content(".symtab")
symbols = list(struct.iter_unpack("<IBBHQQ", symtab))
named = {string(strings, sym[0]): (i, sym) for i, sym in enumerate(symbols) if sym[0]}
assert all(n not in named for n in ("ignored", "unreferenced", "nonexistent"))
helper = named["helper"][1]
assert helper[1:3] == (2, 2)  # LOCAL FUNC, HIDDEN
text = by_name[".text"][1]
gap = content(".text")[helper[4] + helper[5] - text[3]:]
pattern = bytes.fromhex("12345678")
assert len(gap) >= 8 and gap == (pattern * ((len(gap) + 3) // 4))[:len(gap)]
data = by_name[".data"]
assert named["alias"][1][1:5] == (0x11, 0, data[0], data[1][3])
assert named["aligned_alias"][1][1:5] == named["alias"][1][1:5]
assert named["assert_dot"][1][3:5] == (0xfff1, named["after_bss"][1][4])
assert named["constant"][1][3:5] == (0xfff1, 3)
assert named["fallback"][1][3:5] == (0xfff1, data[1][3]+1)
assert struct.unpack_from("<2Q", content(".data")) == (data[1][3]+21, data[1][3]+1)
r_off, r_info, r_add = struct.unpack_from("<3Q", content(".rela.data"))
assert (r_off, r_info & 0xffffffff, r_add) == (data[1][3], 1, 21)
assert symbols[r_info >> 32][1:5] == (3, 0, data[0], data[1][3])
assert struct.unpack_from("<Q", content(".rela.text"), 8)[0] >> 32 == named["helper"][0]
PY

sed 's/\*(\.metadata)/\*(.other)/' layout.lds > orphan.lds
expect_exit 1 "$ld" -T orphan.lds --orphan-handling=error main.o helper.o -o orphan 2>diag
grep -Fq 'orphan input section .metadata' diag
test ! -e orphan
expect_exit 3 "$ld" -T orphan.lds main.o helper.o -o orphan 2>diag
grep -Fq 'not yet supported' diag

sed 's/"rodata size"/"deliberate assertion"/; s/end - begin == 4/end - begin == 5/' layout.lds > invalid.lds
expect_exit 1 "$ld" -T invalid.lds main.o helper.o -o invalid 2>diag
grep -Fq 'assertion failed: deliberate assertion' diag
test ! -e invalid

for unsupported in \
    'MEMORY { ROM : ORIGIN = 0, LENGTH = 64K }' \
    'SECTIONS { .text : { BYTE(7) *(.text) } }' \
    'SECTIONS { .text : { file.o(.text) } }' \
    'SECTIONS { .text : { *(SORT_BY_ALIGNMENT(.text)) } }' \
    'SECTIONS { .text (NOLOAD) : { *(.text) } }' \
    'SECTIONS { .text : { *(.text) } > ROM }' \
    'SECTIONS { .text : { *(COMMON) } }' \
    'SECTIONS { .text : { *(.text) } :MISSING }' \
    'SECTIONS { .text : { *(.text) } n = LOADADDR(.text); }'
do
    printf '%s\n' "$unsupported" > unsupported.lds
    expect_exit 3 "$ld" -T unsupported.lds main.o helper.o -o unsupported 2>diag
    grep -Fq 'not yet supported' diag
    test ! -e unsupported
done
expect_exit 3 "$ld" -r -T layout.lds main.o helper.o -o unsupported 2>diag
grep -Fq 'not yet supported' diag

ar rcs helper.a helper.o
expect_exit 3 "$ld" -T layout.lds main.o helper.a -o unsupported 2>diag
grep -Fq 'lazy archive extraction' diag
"$ld" -T layout.lds main.o --whole-archive helper.a --no-whole-archive -o archive
expect_exit 42 ./archive

printf 'fas-ld script: 2 entry points, 3 PHDRs, retained relocations, deterministic output, whole archives, 2 orphan modes, ASSERT and 11 refusals: ok\n'

cat > start32.s <<'ASM'
.globl _start
.text
_start:
    call callee32
    mov %eax, %ebx
    mov $1, %eax
    int $0x80
.section .words,"a"
.long callee32 + 4
.word callee32 - 1
.section .note.GNU-stack,"",@progbits
ASM
cat > callee32.s <<'ASM'
.globl callee32
.text
callee32:
    mov $42, %eax
    ret
.section .note.GNU-stack,"",@progbits
ASM
cat > layout32.lds <<'LDS'
OUTPUT_FORMAT("elf32-i386")
OUTPUT_ARCH(i386)
ENTRY(_start)
PHDRS { code PT_LOAD FLAGS(5); }
SECTIONS {
  . = 0x1000;
  .text : { *(.text) } :code
  .words : { *(.words) LONG(0x5a5aaa55) }
  /DISCARD/ : { *(.note.GNU-stack) }
}
LDS
"$cc" -m32 -c start32.s -o start32.o
"$cc" -m32 -c callee32.s -o callee32.o
"$ld" -m elf_x86_64 -m elf_i386 -T layout32.lds --emit-relocs start32.o callee32.o -o image32
"$ld" -T layout32.lds -q start32.o callee32.o -o inferred32
cmp image32 inferred32
"${LLD:-ld.lld-22}" -m elf_i386 -T layout32.lds -q start32.o callee32.o -o lld32
for image in image32 lld32; do
    "${LLVM_OBJCOPY:-llvm-objcopy-22}" -O binary "$image" "$image.bin"
    readelf -rW "$image" > "$image.rel"
    grep -q 'R_386_PC32.*callee32' "$image.rel"
    grep -q 'R_386_32.*callee32' "$image.rel"
    grep -q 'R_386_16.*callee32' "$image.rel"
done
cmp image32.bin lld32.bin
expect_exit 3 "$ld" -m elf_x86_64 -T layout32.lds start32.o callee32.o -o wrongclass 2>diag
grep -q 'disagrees with linker script target' diag
expect_exit 1 "$ld" -m elf_i386 start32.o callee32.o -o noscript 2>diag
grep -q 'unsupported emulation elf_i386' diag
printf 'fas-ld i386 script: ELF32 inference, R_386_PC32/32/16, LONG and lld binary comparison: ok\n'

cat > bios32.s <<'ASM'
.text
.globl _start
_start: nop
.section .text32,"ax"
.byte 0xf4
.section .text64,"ax"
.byte 0x90
.section .inittext,"ax"
.byte 0x66, 0xc3
.data
.long code_end
.section .note.GNU-stack,"",@progbits
ASM
cat > bios32.lds <<'LDS'
OUTPUT_FORMAT("elf32-i386")
ENTRY(_start)
SECTIONS {
  . = 0x1000;
  .text : { *(.text) }
  .text32 : { *(.text32) }
  .text64 : { *(.text64) }
  code_end = .;
  . = ALIGN(0x1000);
  .data : { *(.data) }
}
LDS
"$cc" -m32 -c bios32.s -o bios32.o
"$ld" -T bios32.lds -q bios32.o -o bios32-fas
"${LLD:-ld.lld-22}" -T bios32.lds -q bios32.o -o bios32-lld
for image in bios32-fas bios32-lld; do
    "${LLVM_OBJCOPY:-llvm-objcopy-22}" -O binary "$image" "$image.bin"
    readelf -sW "$image" > "$image.sym"
    readelf -SW "$image" > "$image.sec"
    grep -Eq '00001003 +0 +NOTYPE +GLOBAL +DEFAULT +[0-9]+ +code_end' "$image.sym"
    grep -Eq '\.inittext +PROGBITS +00001003' "$image.sec"
done
cmp bios32-fas.bin bios32-lld.bin
expect_exit 1 "$ld" --orphan-handling=error -T bios32.lds bios32.o -o bios32-strict 2>diag
grep -Fq 'orphan input section .inittext' diag
sed '/\.data :/a\  /DISCARD/ : { *(.inittext) }' bios32.lds > discard32.lds
"$ld" -T discard32.lds bios32.o -o bios32-discard
readelf -SW bios32-discard > bios32-discard.sec
if grep -q '\.inittext' bios32-discard.sec; then exit 1; fi
printf 'fas-ld i386 real-mode script: orphan placement matches lld, strict and explicit discard tested\n'

cat > debug.s <<'ASM'
.section .debug_info,"",@progbits
.globl debug_info
debug_info:
.quad _start
.section .note.GNU-stack,"",@progbits
ASM
"$cc" -c debug.s -o debug.o
sed '/^  \/DISCARD\//i\  .debug_info 0 : { *(.debug_info) }' layout.lds > debug.lds
"$ld" -T debug.lds -q main.o helper.o debug.o -o with-debug
"$ld" -T debug.lds -q --strip-debug main.o helper.o debug.o -o strip-debug
"$ld" -T debug.lds -q -S main.o helper.o debug.o -o strip-debug-short
cmp strip-debug strip-debug-short
readelf -SWs with-debug > with-debug.elf
readelf -SWs strip-debug > strip-debug.elf
grep -Fq '.rela.debug_info' with-debug.elf
grep -Eq ' [0-9]+ +debug_info$' with-debug.elf
if grep -Fq 'debug_info' strip-debug.elf; then exit 1; fi
expect_exit 42 ./strip-debug
printf 'fas-ld script: long and short strip-debug options remove debug sections, relocations and symbols\n'

cat > pie.s <<'ASM'
.text
.globl _start
_start:
    movq helper_entry@GOTPCREL(%rip), %rax
    call *%rax
    mov %eax, %edi
    mov $60, %eax
    syscall
.section .note.GNU-stack,"",@progbits
ASM
cat > pie-helper.s <<'ASM'
.text
.globl helper_entry
helper_entry:
    mov $42, %eax
    ret
.section .note.GNU-stack,"",@progbits
ASM
cat > pie.lds <<'LDS'
ENTRY(_start)
SECTIONS {
  . = 0;
  .text : { *(.text) }
  .data : { *(.data) }
  /DISCARD/ : { *(.dynamic .dynsym .dynstr .hash .gnu.hash) }
}
LDS
"$cc" -c pie.s -o pie.o
"$cc" -c pie-helper.s -o pie-helper.o
ar rcs pie-helper.a pie-helper.o
"$ld" -pie --no-dynamic-linker -T pie.lds pie.o pie-helper.a -o pie
"${LLD:-ld.lld-22}" -pie --no-dynamic-linker -T pie.lds pie.o pie-helper.a -o pie-lld
readelf -hlWr pie > pie.elf
grep -Eq 'Type: +DYN' pie.elf
grep -q 'There are no relocations' pie.elf
if grep -q 'INTERP' pie.elf; then exit 1; fi
for image in pie pie-lld; do
    "${LLVM_OBJCOPY:-llvm-objcopy-22}" -O binary "$image" "$image.bin"
done
cmp pie.bin pie-lld.bin
expect_exit 42 ./pie
printf '.data\n.quad _start\n.section .note.GNU-stack,"",@progbits\n' > dynamic-reloc.s
"$cc" -c dynamic-reloc.s -o dynamic-reloc.o
expect_exit 3 "$ld" -pie --no-dynamic-linker -T pie.lds pie.o pie-helper.a dynamic-reloc.o -o unsupported 2>diag
grep -q 'scripted PIE requires a run-time relocation' diag
sed '/^  \.data/a\  helper_entry = .;' pie.lds > pie-overlap.lds
expect_exit 3 "$ld" -pie --no-dynamic-linker -T pie-overlap.lds pie.o pie-helper.a -o unsupported 2>diag
grep -q 'script assignment overlaps a lazy archive symbol' diag
printf 'fas-ld script: loaderless PIE executes, relaxes GOTPCRELX and extracts archives, with two refusal checks\n'

cat > gc32.s <<'ASM'
.globl _start, kept, dead, kept_target, discarded_target
.section .text.start,"ax"
_start:
    mov $1, %eax
    mov $42, %ebx
    int $0x80
.section .data.kept,"aw"
kept: .long kept_target
.section .data.kept_target,"aw"
kept_target: .long 42
.section .data.dead,"aw"
dead: .long 0
.section .data.discarded_target,"aw"
discarded_target: .long 0
.section .discard,"a"
.long discarded_target
.section .note.GNU-stack,"",@progbits
ASM
cat > gc32.lds <<'LDS'
OUTPUT_FORMAT("elf32-i386")
ENTRY(_start)
SECTIONS {
  . = 0x401000;
  .text : { *(.text.*) }
  .data : { KEEP(*(.data.kept)) *(.data.*) }
  /DISCARD/ : { *(.discard) }
}
LDS
"$cc" -m32 -c gc32.s -o gc32.o
lld=${LLD:-ld.lld-22}
for linker in "$ld" "$lld"; do
    "$linker" --gc-sections -T gc32.lds gc32.o -o "gc32.${linker##*/}"
    "${LLVM_OBJCOPY:-llvm-objcopy-22}" -O binary "gc32.${linker##*/}" "gc32.${linker##*/}.bin"
done
cmp "gc32.${ld##*/}.bin" "gc32.${lld##*/}.bin"
readelf -sW "gc32.${ld##*/}" > gc32.symbols
grep -q 'kept_target$' gc32.symbols
if grep -Eq ' (dead|discarded_target)$' gc32.symbols; then exit 1; fi
printf 'fas-ld i386 script: KEEP roots sections, GC follows relocations and discards unused sections\n'

cat > shared.lds <<'LDS'
ENTRY(entry)
PHDRS {
  load PT_LOAD FLAGS(5) FILEHDR PHDRS;
  dynamic PT_DYNAMIC FLAGS(4);
  unwind 0x6474e550;
}
SECTIONS {
  before = . - 4096;
  . = SIZEOF_HEADERS;
  .hash : { *(.hash) } :load
  .gnu.hash : { *(.gnu.hash) }
  .dynsym : { *(.dynsym) }
  .dynstr : { *(.dynstr) }
  .gnu.version : { *(.gnu.version) }
  .gnu.version_d : { *(.gnu.version_d) }
  .dynamic : { *(.dynamic) } :load :dynamic
  .data : { *(.data) *(.got.plt) } :load
  .eh_frame_hdr : { *(.eh_frame_hdr) } :load :unwind
  .eh_frame : { KEEP(*(.eh_frame)) } :load
  .text : { *(.text) } :load =0x90909090,
  /DISCARD/ : { *(.note.GNU-stack) }
}
VERSION {
  TEST_1 { global: entry; local: *; };
}
LDS
cat > shared32.s <<'ASM'
.text
.globl entry
.type entry, @function
entry:
.cfi_startproc
    call 1f
1:  popl %ecx
    addl $_GLOBAL_OFFSET_TABLE_+(.-1b), %ecx
    leal before@GOTOFF(%ecx), %eax
    ret
.cfi_endproc
.size entry, .-entry
ASM
cat > shared64.s <<'ASM'
.text
.globl entry
.type entry, @function
entry:
.cfi_startproc
    leaq before(%rip), %rax
    ret
.cfi_endproc
.size entry, .-entry
ASM
cat > shared-extra.s <<'ASM'
.text
.type local_entry, @function
local_entry:
.cfi_startproc
    ret
.cfi_endproc
.size local_entry, .-local_entry
ASM
for bits in 32 64; do
    emulation=elf_i386
    if [[ $bits == 64 ]]; then emulation=elf_x86_64; fi
    "$cc" -m"$bits" -c "shared$bits.s" -o "shared$bits.o"
    "$cc" -m"$bits" -c shared-extra.s -o "shared-extra$bits.o"
    for linker in "$ld" "$lld"; do
        so="shared$bits.${linker##*/}.so"
        "$linker" -m "$emulation" -shared -Bsymbolic --hash-style=both \
            --eh-frame-hdr -soname sample.so -T shared.lds "shared$bits.o" "shared-extra$bits.o" -o "$so"
        readelf -dWrV "$so" > "$so.meta"
        grep -q 'There are no relocations' "$so.meta"
        grep -q 'Name: TEST_1' "$so.meta"
        readelf --dyn-syms -W "$so" | grep -q 'entry@@TEST_1$'
        readelf -lW "$so" > "$so.phdrs"
        for section in text eh_frame eh_frame_hdr; do
            "${LLVM_OBJCOPY:-llvm-objcopy-22}" -O binary --only-section=".$section" "$so" "$so.$section"
        done
    done
    for section in text eh_frame eh_frame_hdr phdrs; do
        cmp "shared$bits.${ld##*/}.so.$section" "shared$bits.${lld##*/}.so.$section"
    done
done
printf '.data\n.quad entry\n' > dynamic-fixup.s
"$cc" -c dynamic-fixup.s -o dynamic-fixup.o
if "$ld" -shared -Bsymbolic -T shared.lds shared64.o dynamic-fixup.o -o invalid.so 2>invalid.err; then
    exit 1
else
    test "$?" -eq 3
fi
grep -q 'requires a run-time relocation' invalid.err
test ! -e invalid.so
printf 'fas-ld scripted i386 and x86-64 shared objects: versions, GOT displacement, unwind and dynamic-fixup refusal tested\n'
