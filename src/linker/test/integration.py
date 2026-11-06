#!/usr/bin/env python3
"""Independent ELF inspection, real toolchain fixtures, and malformed inputs.

Requires Python 3, GNU as/ld/readelf, and GCC. LLD is compared when installed.
No third-party Python modules or libraries from the linker implementation.
"""
import hashlib
import os
from pathlib import Path
import random
import shutil
import struct
import subprocess
import sys
import tempfile
import unittest

LINKER = str(Path(sys.argv.pop(1)).resolve())
REENTRY = str(Path(sys.argv.pop(1)).resolve())
FIXTURES = Path(__file__).resolve().parent / "fixtures"


def run(*args, expected=0):
    p = subprocess.run([str(a) for a in args], capture_output=True, timeout=10)
    if p.returncode != expected:
        raise AssertionError(f"{args}: exit {p.returncode}, expected {expected}\n{p.stderr.decode(errors='replace')}")
    return p


def elf_sections(data):
    shoff = struct.unpack_from("<Q", data, 40)[0]
    count, strings = struct.unpack_from("<HH", data, 60)
    raw = [struct.unpack_from("<IIQQQQIIQQ", data, shoff + n * 64) for n in range(count)]
    str_offset, str_size = raw[strings][4:6]
    names = data[str_offset:str_offset + str_size]
    return [dict(index=n, header=shoff + n * 64,
                 name=names[s[0]:].split(b"\0", 1)[0].decode(), kind=s[1], flags=s[2],
                 address=s[3], offset=s[4], size=s[5], link=s[6], info=s[7], align=s[8], entsize=s[9])
            for n, s in enumerate(raw)]


def elf_segments(data):
    offset = struct.unpack_from('<Q', data, 32)[0]
    count = struct.unpack_from('<H', data, 56)[0]
    keys = ('kind','flags','offset','address','physical','file_size','memory_size','alignment')
    return [dict(zip(keys, struct.unpack_from('<IIQQQQQQ',data,offset+n*56)))
            for n in range(count)]


def elf_symbols(data, kind=2):
    sections = elf_sections(data)
    table = next(s for s in sections if s["kind"] == kind)
    strings = sections[table["link"]]
    names = data[strings["offset"]:strings["offset"] + strings["size"]]
    result = []
    for at in range(table["offset"], table["offset"] + table["size"], 24):
        name, info, other, section, value, size = struct.unpack_from("<IBBHQQ", data, at)
        result.append(dict(name=names[name:].split(b"\0", 1)[0].decode(), info=info,
                           other=other, section=section, value=value, size=size, offset=at))
    return result


class LinkerTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        for tool in ("as", "ld", "readelf", "gcc"):
            if not shutil.which(tool):
                raise RuntimeError(f"required development tool unavailable: {tool}")
        cls.temp = tempfile.TemporaryDirectory(prefix="fas-linker-tests-")
        cls.root = Path(cls.temp.name)
        cls.start = cls.root / "start.o"
        cls.support = cls.root / "support.o"
        run("as", "--64", "-o", cls.start, FIXTURES / "start.s")
        run("as", "--64", "-o", cls.support, FIXTURES / "support.s")
        cls.simple = cls.assemble("simple", ".text\n.globl _start\n_start: call answer\nmov %eax,%edi\nmov $60,%eax\nsyscall\n")
        cls.answer = cls.assemble("answer", ".text\n.globl answer\nanswer: mov $42,%eax\nret\n")

    @classmethod
    def tearDownClass(cls):
        cls.temp.cleanup()

    @classmethod
    def assemble(cls, name, source):
        path = cls.root / (name + ".s")
        path.write_text(source + '\n.section .note.GNU-stack,"",@progbits\n')
        out = path.with_suffix(".o")
        run("as", "--64", "-o", out, path)
        return out

    def link(self, name, *objects, options=(), linker=LINKER):
        out = self.root / name
        run(linker, *options, "-o", out, *objects)
        return out

    def rejected(self, path, expected, *extra, options=()):
        output = self.root / "preserve-output"
        sentinel = b"previous valid output\x00\xff"
        output.write_bytes(sentinel)
        output.chmod(0o640)
        p = run(LINKER, *options, "-o", output, path, *extra, expected=1)
        self.assertIn(expected.encode(), p.stderr)
        self.assertNotIn(b"Fatal error", p.stderr)
        self.assertEqual(output.read_bytes(), sentinel)
        self.assertEqual(output.stat().st_mode & 0o777, 0o640)
        self.assertEqual(list(self.root.glob(".fas-ld-*.tmp")), [])

    def test_cross_object_execution_and_reference(self):
        expected = b"fas-ld: objects, pointers, data, BSS: ok\n"
        for name, objects in (("program", [self.start, self.support]), ("reverse", [self.support, self.start])):
            out = self.link(name, *objects)
            self.assertEqual(run(out).stdout, expected)
        reference = self.link("reference", self.start, self.support, linker="ld")
        self.assertEqual(run(reference).stdout, expected)
        lld = shutil.which("ld.lld")
        if not lld:
            lld = next((str(p) for p in Path("/usr/lib").glob("llvm-*/bin/ld.lld") if p.is_file()), None)
        if lld:
            self.assertEqual(run(self.link("reference-lld", self.start, self.support, linker=lld)).stdout, expected)
        else:
            print("LLD comparison skipped: ld.lld is not installed", file=sys.stderr)

    def test_output_structure_and_reproducibility(self):
        a = self.link("layout-a", self.start, self.support)
        b = self.link("layout-b", self.start, self.support)
        data = a.read_bytes()
        self.assertEqual(hashlib.sha256(data).digest(), hashlib.sha256(b.read_bytes()).digest())
        self.assertEqual(struct.unpack_from("<HH", data, 16), (2, 62))
        sections = elf_sections(data)
        phoff = struct.unpack_from("<Q", data, 32)[0]
        phnum = struct.unpack_from("<H", data, 56)[0]
        headers = [struct.unpack_from("<IIQQQQQQ", data, phoff + n * 56) for n in range(phnum)]
        loads = [h for h in headers if h[0] == 1]
        self.assertTrue(any(h[0] == 0x6474E551 and h[1] == 6 for h in headers))
        self.assertFalse(any(h[1] & 3 == 3 for h in loads))
        ranges = []
        for _, flags, offset, address, _, file_size, mem_size, align in loads:
            self.assertLessEqual(file_size, mem_size)
            self.assertEqual(offset % align, address % align)
            self.assertLessEqual(offset + file_size, len(data))
            if mem_size:
                ranges.append((address, address + mem_size))
        for a_range, b_range in zip(sorted(ranges), sorted(ranges)[1:]):
            self.assertLessEqual(a_range[1], b_range[0])
        for s in sections:
            if s["kind"] != 8:
                self.assertLessEqual(s["offset"] + s["size"], len(data))
            if s["flags"] & 2:
                self.assertEqual(s["address"] % max(s["align"], 1), 0)
                self.assertTrue(any(h[3] <= s["address"] and s["address"] + s["size"] <= h[3] + h[6] for h in loads))
        self.assertLess(len(data), 200_000, "BSS must not consume file bytes")
        symbols = elf_symbols(data)
        table = next(s for s in sections if s["kind"] == 2)
        self.assertTrue(all(s["info"] >> 4 == 0 for s in symbols[:table["info"]]))
        self.assertTrue(all(s["info"] >> 4 != 0 for s in symbols[table["info"]:]))
        by_name = {s["name"]: s for s in symbols}
        self.assertEqual(by_name["negative"]["value"], (1 << 64) - 7)
        self.assertEqual(by_name["scratch"]["size"], 1048576)
        report = run("readelf", "-aW", a)
        self.assertEqual(report.stderr, b"")

    def test_freestanding_c_objects(self):
        source = self.root / "answer.c"
        source.write_text("static int bias = 40; int answer(void) { return bias + 2; }\n")
        compilers = [("gcc", [])]
        clang = shutil.which("clang-22") or shutil.which("clang")
        if clang:
            compilers.append((clang, ["-fno-addrsig"]))
        for index, (compiler, extra) in enumerate(compilers):
            with self.subTest(compiler=compiler):
                obj = self.root / f"answer-c-{index}.o"
                run(compiler, "-c", "-O0", "-ffreestanding", "-fno-pic", "-fno-pie", "-fno-stack-protector",
                    "-fno-unwind-tables", "-fno-asynchronous-unwind-tables", "-fno-ident",
                    "-fcf-protection=none", *extra, "-o", obj, source)
                run(self.link(f"c-program-{index}", self.simple, obj), expected=42)

    def test_library_reentry_and_concurrent_invocations(self):
        run(REENTRY, self.start, self.support)

    def test_seeded_valid_object_graphs(self):
        rng = random.Random(0xFA520)
        for iteration in range(12):
            count = rng.randint(2, 8)
            constants = [rng.randint(1, 9) for _ in range(count)]
            total = sum(constants)
            objects = []
            for index, value in enumerate(constants):
                next_call = f"sub $8,%rsp\ncall fn_{index + 1}\nadd $8,%rsp\nadd ${value},%eax\n" if index + 1 < count else f"mov ${value},%eax\n"
                objects.append(self.assemble(f"graph-{iteration}-{index}",
                    f".text\n.globl fn_{index}\nfn_{index}:\n{next_call}ret\n.data\nlocal: .quad fn_{index}+1"))
            start = self.assemble(f"graph-{iteration}-start", ".text\n.globl _start\n_start: call fn_0\nmov %eax,%edi\nmov $60,%eax\nsyscall")
            rng.shuffle(objects)
            run(self.link(f"graph-{iteration}", start, *objects), expected=total)
            run(self.link(f"graph-reference-{iteration}", start, *objects, linker="ld"), expected=total)

    def test_local_symbols_do_not_conflict(self):
        a = self.assemble("local-a", ".data\nlocal: .quad 2\n.text\n.globl answer\nanswer: mov local(%rip),%eax\nret")
        b = self.assemble("local-b", ".data\nlocal: .quad 40\n.text\n.globl _start\n_start: call answer\nadd local(%rip),%eax\nmov %eax,%edi\nmov $60,%eax\nsyscall")
        run(self.link("locals", a, b), expected=42)

    def test_sized_undefined_and_unused_declarations(self):
        source = self.assemble("sized-undefined", ".text\n.globl _start, unused\n.type answer,@function\n.size answer,6\n_start: call answer\nmov %eax,%edi\nmov $60,%eax\nsyscall")
        out = self.link("sized-undefined-program", source, self.answer)
        run(out, expected=42)
        self.assertNotIn("unused", [s["name"] for s in elf_symbols(out.read_bytes())])
        run(self.link("sized-undefined-reference", source, self.answer, linker="ld"), expected=42)

    def test_nobits_alignment_and_ignored_file_offset(self):
        obj = self.assemble("nobits", '.text\n.globl _start\n_start: lea high(%rip),%rax\nand $2097151,%rax\njne bad\ncmpq $0, high(%rip)\njne bad\ncmpq $0, ro_zero(%rip)\njne bad\ncmpb $0, high+8192(%rip)\njne bad\nxor %edi,%edi\njmp done\nbad: mov $99,%edi\ndone: mov $60,%eax\nsyscall\n.section .zero,"a",@nobits\n.balign 4096\nro_zero: .skip 4097\n.bss\n.balign 2097152\nhigh: .skip 8193')
        data = bytearray(obj.read_bytes())
        bss = next(s for s in elf_sections(data) if s["name"] == ".bss")
        struct.pack_into("<Q", data, bss["header"] + 24, (1 << 64) - 1)
        mutated = self.root / "nobits-offset.o"
        mutated.write_bytes(data)
        out = self.link("nobits-program", mutated)
        run(out)
        self.assertLess(out.stat().st_size, 3 * 1024 * 1024)
        self.assertEqual(run("readelf", "-aW", out).stderr, b"")

    def test_custom_entry_and_input_after_double_dash(self):
        obj = self.assemble("entry", ".text\n.globl begin\nbegin: mov $60,%eax\nmov $19,%edi\nsyscall")
        out = self.link("entry-program", obj, options=("--entry", "begin"))
        run(out, expected=19)
        out = self.root / "double-dash"
        run(LINKER, "-o", out, "--", self.simple, self.answer)
        run(out, expected=42)
        self.rejected(obj, "undefined entry symbol _start")

    def test_relocations_with_null_symbol_and_none(self):
        obj = self.assemble("none", ".text\n.globl _start\n_start: mov $60,%eax\nxor %edi,%edi\nsyscall\n.data\nslot: .quad 0\n.reloc slot, R_X86_64_64, 0x12345678\n.quad absent")
        data = bytearray(obj.read_bytes())
        rela = next(s for s in elf_sections(data) if s["kind"] == 4)
        struct.pack_into("<Q", data, rela["offset"] + 24 + 8, 0)  # NONE with null symbol
        patched = self.root / "none-patched.o"
        patched.write_bytes(data)
        out = self.link("none-program", patched)
        run(out)
        content = out.read_bytes()
        section = next(s for s in elf_sections(content) if s["name"] == ".data")
        self.assertEqual(struct.unpack_from("<QQ", content, section["offset"]), (0x12345678, 0))

    def test_undefined_duplicate_and_unsupported_semantics(self):
        self.rejected(self.simple, "undefined symbol answer")
        self.rejected(self.simple, "duplicate symbol answer", self.answer, self.answer)
        cases = [
            ("tls", '.section .tdata,"awT",@progbits\n.type tls,@tls_object\ntls: .quad 1\n.text\nlea tls@TLSGD(%rip),%rdi', "TLS relocation requires dynamic output"),
            ("unwind", '.globl metadata\n.set metadata,0\n.section .eh_frame,"a",@progbits\n.quad 1', ".eh_frame"),
            ("wx", '.section .unsafe,"awx",@progbits\n.byte 1', "writable executable"),
        ]
        for name, source, diagnostic in cases:
            with self.subTest(name=name):
                self.rejected(self.assemble(name, source), diagnostic, self.simple, self.answer)

    def test_relocation_overflow_preserves_output(self):
        for name, source, target, diagnostic in [
            ("unsigned", ".data\n.long huge", 0x100000000, "R_X86_64_32 overflow"),
            ("signed", ".text\nmovq $huge,%rax", 0x80000000, "R_X86_64_32S overflow"),
            ("pc", ".data\n.long huge-.", 0x1000000000, "R_X86_64_PC32 overflow"),
        ]:
            with self.subTest(name=name):
                ref = self.assemble("overflow-" + name, source)
                definition = self.assemble("huge-" + name, f".globl huge\n.set huge, {target}")
                self.rejected(ref, diagnostic, self.simple, self.answer, definition)

    def test_invalid_entry_and_output_aliases(self):
        obj = self.assemble("data-entry", ".data\n.globl _start\n_start: .quad 0")
        self.rejected(obj, "must be inside an executable section")
        obj = self.assemble("absolute-entry", ".globl _start\n.set _start, 0x401000")
        self.rejected(obj, "must be defined in executable code")
        path = self.root / "alias-input.o"
        shutil.copy(self.simple, path)
        before = path.read_bytes()
        alias = self.root / "hardlink.o"
        os.link(path, alias)
        run(LINKER, "-o", alias, path, self.answer, expected=1)
        self.assertEqual(path.read_bytes(), before)
        symbolic = self.root / "symlink.o"
        symbolic.symlink_to(path)
        run(LINKER, "-o", symbolic, path, self.answer, expected=1)
        self.assertEqual(path.read_bytes(), before)
        run(LINKER, "-o", self.root, self.simple, self.answer, expected=1)
        self.assertEqual(list(self.root.glob(".fas-ld-*.tmp")), [])

    def test_relocatable_section_addresses_are_not_load_addresses(self):
        obj = self.assemble('relocatable-addresses', '''
.text
.globl _start
_start:
.cfi_startproc
mov value(%rip),%edi
mov %edi,zero(%rip)
mov zero(%rip),%edi
mov $60,%eax
syscall
.cfi_endproc
.data
.globl value
value: .long 42
.bss
.p2align 2
zero: .zero 4
.section .note.ABI-tag,"a",@note
.p2align 2
.long 4,16,1
.asciz "GNU"
.long 0,3,2,0
''')
        data = bytearray(obj.read_bytes())
        names = {'.text', '.data', '.bss', '.eh_frame', '.note.ABI-tag', '.note.GNU-stack'}
        for section in elf_sections(data):
            if section['name'] in names:
                struct.pack_into('<Q', data, section['header'] + 16, 0x1000 * section['index'])
        obj.write_bytes(data)
        tools = [('gnu', shutil.which('ld'))]
        if shutil.which('ld.lld'):
            tools.append(('lld', shutil.which('ld.lld')))
        tools.append(('fas', LINKER))
        for tag, tool in tools:
            direct = self.link('address-direct-' + tag, obj, linker=tool)
            run(direct, expected=42)
            partial = self.link('address-partial-' + tag, obj, linker=tool, options=('-r',))
            if tag == 'fas':
                self.assertTrue(all(s['address'] == 0 for s in elf_sections(partial.read_bytes())))
            for consumer_tag, consumer in tools:
                final = self.link('address-final-' + tag + '-' + consumer_tag,
                                  partial, linker=consumer)
                run(final, expected=42)

    def test_malformed_objects(self):
        original = self.simple.read_bytes()
        sections = elf_sections(original)
        text = next(s for s in sections if s["name"] == ".text")
        rela = next(s for s in sections if s["kind"] == 4)
        symtab = next(s for s in sections if s["kind"] == 2)
        strtab = sections[symtab["link"]]
        start_symbol = next(s for s in elf_symbols(original) if s["name"] == "_start")
        shoff = struct.unpack_from("<Q", original, 40)[0]
        mutations = [
            ("class", 4, "B", 1, "class"), ("endian", 5, "B", 2, "byte order"),
            ("machine", 18, "H", 183, "machine"), ("type", 16, "H", 4, "type"),
            ("ident-version", 6, "B", 2, "version"), ("version", 20, "I", 0, "version"),
            ("abi", 7, "B", 255, "ABI"), ("padding", 9, "B", 1, "padding"),
            ("phoff", 32, "Q", 64, "program headers"), ("phnum", 56, "H", 1, "program headers"),
            ("flags", 48, "I", 1, "flags"), ("ehsize", 52, "H", 1, "header"),
            ("shentsize", 58, "H", 1, "entry size"), ("extended", 60, "H", 0, "extended"),
            ("shnames", 62, "H", 65534, "name table"), ("shoff", 40, "Q", (1 << 64) - 1, "offset"),
            ("null-section", shoff + 4, "I", 1, "null section"),
            ("name-offset", text["header"], "I", 0xFFFFFFFF, "name offset"),
            ("section-type", text["header"] + 4, "I", 0x1234, "unsupported section type"),
            ("alignment", text["header"] + 48, "Q", 3, "alignment"),
            ("alignment-limit", text["header"] + 48, "Q", 1 << 40, "alignment"),
            ("offset", text["header"] + 24, "Q", len(original) + 1, "offset"),
            ("section-overlap", text["header"] + 24, "Q", 0, "overlaps"),
            ("size", text["header"] + 32, "Q", len(original), "byte range"),
            ("link", symtab["header"] + 40, "I", len(sections), "section link"),
            ("symbol-entry", symtab["header"] + 56, "Q", 1, "symbol table entry"),
            ("symbol-order", symtab["header"] + 44, "I", 0, "first global"),
            ("symbol-null", symtab["offset"] + 8, "Q", 1, "null symbol"),
            ("symbol-name", start_symbol["offset"], "I", 0xFFFFFFFF, "symbol name"),
            ("symbol-section", start_symbol["offset"] + 6, "H", 65000, "section index"),
            ("symbol-value", start_symbol["offset"] + 8, "Q", text["size"] + 1, "exceeds its section"),
            ("symbol-size", start_symbol["offset"] + 16, "Q", text["size"] + 1, "exceeds its section"),
            ("symbol-visibility", start_symbol["offset"] + 5, "B", 4, "visibility"),
            ("strtab-start", strtab["offset"], "B", 65, "string table"),
            ("strtab-end", strtab["offset"] + strtab["size"] - 1, "B", 65, "string table"),
            ("reloc-kind", rela["offset"] + 8, "I", 999, "unsupported x86-64 relocation"),
            ("reloc-symbol", rela["offset"] + 12, "I", 0xFFFFFFFF, "relocation symbol index"),
            ("reloc-offset", rela["offset"], "Q", text["size"] + 1, "relocation offset"),
            ("reloc-width", rela["offset"], "Q", text["size"] - 1, "byte range"),
            ("reloc-target", rela["header"] + 44, "I", len(sections), "target index"),
            ("reloc-link", rela["header"] + 40, "I", 0, "relocation symbol table"),
            ("reloc-entry", rela["header"] + 56, "Q", 8, "RELA entry size"),
        ]
        for name, at, fmt, value, diagnostic in mutations:
            with self.subTest(name=name):
                data = bytearray(original)
                struct.pack_into("<" + fmt, data, at, value)
                path = self.root / "malformed.o"
                path.write_bytes(data)
                self.rejected(path, diagnostic, self.answer)

    def test_overlapping_relocations(self):
        obj = self.assemble("overlap", ".data\n.quad answer\n.quad answer")
        data = bytearray(obj.read_bytes())
        rela = next(s for s in elf_sections(data) if s["kind"] == 4)
        struct.pack_into("<Q", data, rela["offset"] + 24, 1)
        path = self.root / "overlap-patched.o"
        path.write_bytes(data)
        self.rejected(path, "overlapping relocations", self.simple, self.answer)

    def test_truncation_and_seeded_mutation_campaign(self):
        original = self.simple.read_bytes()
        path = self.root / "mutated.o"
        output = self.root / "mutation-output"
        for size in list(range(64)) + [len(original) - 1, len(original) // 2]:
            path.write_bytes(original[:size])
            self.rejected(path, "fas-ld:", self.answer)
        rng = random.Random(0xFA510)
        for _ in range(256):
            data = bytearray(original)
            for _ in range(rng.randint(1, 6)):
                data[rng.randrange(len(data))] ^= rng.randrange(1, 256)
            path.write_bytes(data)
            p = subprocess.run([LINKER, "-o", str(output), str(path), str(self.answer)],
                               capture_output=True, timeout=3)
            self.assertIn(p.returncode, (0, 1), p.stderr.decode(errors="replace"))
            self.assertNotIn(b"Fatal error", p.stderr)
            self.assertEqual(list(self.root.glob(".fas-ld-*.tmp")), [])

    def test_cli_and_nonregular_input(self):
        self.assertIn(b"Usage:", run(LINKER, "--help").stdout)
        self.assertIn(b"fas-ld", run(LINKER, "--version").stdout)
        for options in ([], ["-o"], ["-e"], ["--unknown"]):
            run(LINKER, *options, expected=1)
        self.rejected("/dev/null", "regular file")
        fifo = self.root / "input-fifo"
        os.mkfifo(fifo)
        self.rejected(fifo, "regular file")

    def test_short_version_continues_linking_and_gcc_abi_probe(self):
        self.assertIn(b"fas-ld", run(LINKER, "-v").stdout)
        output = self.root / "version-output"
        args = ("-o", output, self.simple, self.answer)
        self.assertIn(b"fas-ld", run(LINKER, "-v", *args).stdout)
        run(output, expected=42)
        output.unlink()
        run(LINKER, "--version", *args)
        self.assertFalse(output.exists())
        run(LINKER, "-v", self.root / "missing-input.o", expected=1)
        # CMake identifies the ABI using GCC -v -Wl,-v. This must produce
        # an executable as well as the version text.
        tools = self.root / "version-toolchain"
        tools.mkdir()
        (tools / "ld").symlink_to(LINKER)
        source = self.root / "version-probe.c"
        source.write_text("int main(void) { return 0; }\n")
        result = run("gcc", "-B" + str(tools) + "/", "-fno-use-linker-plugin",
                     "-v", "-Wl,-v", source, "-o", output)
        self.assertIn(b"fas-ld", result.stdout)
        run(output)


if __name__ == "__main__":
    unittest.main(verbosity=2)
