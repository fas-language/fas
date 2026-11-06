#!/usr/bin/env python3
"""CFI record bounds, GC interaction, runtime debugger and independent index decode."""
import random
import shutil
import struct
import subprocess
import unittest

import integration as core

run = core.run
LINKER = core.LINKER


class UnwindTests(unittest.TestCase):
    setUpClass = classmethod(core.LinkerTests.setUpClass.__func__)
    tearDownClass = classmethod(core.LinkerTests.tearDownClass.__func__)
    assemble = classmethod(core.LinkerTests.assemble.__func__)
    link = core.LinkerTests.link
    rejected = core.LinkerTests.rejected

    def sources(self):
        first = self.root / 'unwind-leaf.c'
        first.write_text('__attribute__((noinline)) int leaf(int x) {\n'
                         '  volatile int values[32];\n  values[0] = x;\n  return values[0] + 40;\n}\n'
                         'int dead_first(void) { return 9; }\n')
        second = self.root / 'unwind-answer.c'
        second.write_text('extern int leaf(int);\n__attribute__((noinline)) int answer(void) {\n'
                          '  return leaf(2);\n}\nint dead_second(void) { return 7; }\n')
        return first, second

    def compile(self, compiler):
        objects = []
        for source in self.sources():
            obj = source.with_suffix('.o')
            run(compiler, '-c', '-O1', '-g', '-ffreestanding', '-fPIC', '-fomit-frame-pointer',
                '-fno-optimize-sibling-calls', '-ffunction-sections', '-fdata-sections',
                '-fasynchronous-unwind-tables', '-o', obj, source)
            self.assertNotIn('.debug_frame', [s['name'] for s in core.elf_sections(obj.read_bytes())])
            objects.append(obj)
        return objects

    def verify_index(self, out, expected):
        data = out.read_bytes()
        sections = core.elf_sections(data)
        eh = next(s for s in sections if s['name'] == '.eh_frame')
        self.assertEqual(len([s for s in sections if s['name'] == '.eh_frame']), 1)
        # Both indexed readers and legacy libgcc scanners must stop within the
        # section. Validate the record stream, including its final zero length.
        cursor, ending = eh['offset'], eh['offset'] + eh['size']
        while cursor + 4 <= ending:
            length = struct.unpack_from('<I', data, cursor)[0]
            if length == 0:
                break
            self.assertLessEqual(cursor + 4 + length, ending)
            cursor += 4 + length
        self.assertLess(cursor, ending, 'missing final unwind terminator')
        self.assertEqual(data[cursor:ending], bytes(ending-cursor))
        header = next(s for s in sections if s['name'] == '.eh_frame_hdr')
        at, address = header['offset'], header['address']
        self.assertEqual(data[at:at+4], bytes((1, 0x1b, 3, 0x3b)))
        self.assertEqual(address + 4 + struct.unpack_from('<i', data, at+4)[0], eh['address'])
        count = struct.unpack_from('<I', data, at+8)[0]
        self.assertEqual(count, expected)
        self.assertEqual(header['size'], 12 + count * 8)
        entries = [(address + pc, address + fde) for pc, fde in
                   (struct.unpack_from('<ii', data, at+12+n*8) for n in range(count))]
        self.assertEqual(entries, sorted(entries))
        for pc, fde in entries:
            offset = eh['offset'] + fde - eh['address']
            length, cie_pointer, initial = struct.unpack_from('<IIi', data, offset)
            self.assertGreaterEqual(length, 16)
            cie = offset + 4 - cie_pointer
            self.assertEqual(struct.unpack_from('<I', data, cie + 4)[0], 0)
            self.assertEqual(fde + 8 + initial, pc)
        phoff = struct.unpack_from('<Q', data, 32)[0]
        phnum = struct.unpack_from('<H', data, 56)[0]
        headers = [struct.unpack_from('<IIQQQQQQ', data, phoff+i*56) for i in range(phnum)]
        ph = next(p for p in headers if p[0] == 0x6474e550)
        self.assertEqual((ph[1], ph[2], ph[3], ph[5], ph[6]),
                         (4, at, address, header['size'], header['size']))
        report = run('readelf', '-aW', '--debug-dump=frames', out)
        self.assertEqual(report.stderr, b'')

    def test_default_compiler_unwind_and_gc_index(self):
        compilers = ['gcc']
        clang = shutil.which('clang-22') or shutil.which('clang')
        if clang:
            compilers.append(clang)
        for compiler in compilers:
            objects = self.compile(compiler)
            for gc, count in ((False, 4), (True, 2)):
                with self.subTest(compiler=compiler, gc=gc):
                    options = ('--gc-sections',) if gc else ()
                    out = self.link('unwind-program', self.simple, *objects, options=options)
                    run(out, expected=42)
                    self.verify_index(out, count)
                    run(self.link('unwind-gnu', self.simple, *objects, options=options, linker='ld'), expected=42)

    def test_gdb_backtrace_without_frame_pointers(self):
        if not shutil.which('gdb'):
            self.skipTest('gdb unavailable; runtime unwind gate remains unverified')
        objects = self.compile('gcc')
        out = self.link('unwind-gdb', self.simple, *objects, options=('--gc-sections',))
        report = subprocess.run(['gdb', '-nx', '-batch', '-ex', 'set debuginfod enabled off',
            '-ex', 'set disable-randomization off', '-ex', 'break leaf', '-ex', 'run',
            '-ex', 'next', '-ex', 'backtrace', '-ex', 'continue', str(out)], capture_output=True, timeout=20)
        text = report.stdout + report.stderr
        if b'ptrace: Operation not permitted' in text:
            self.skipTest('ptrace unavailable; runtime unwind gate remains unverified')
        self.assertEqual(report.returncode, 0, text.decode(errors='replace'))
        self.assertIn(b'leaf (x=x@entry=2)', text)
        self.assertIn(b'answer () at', text)
        self.assertIn(b'_start', text)
        self.assertNotIn(b'Dwarf Error', text)

    def test_unwind_terminator_and_record_padding(self):
        obj = self.assemble('unwind-padding', '.text\n.globl answer\nanswer:\n.cfi_startproc\nmov $42,%eax\nret\n.cfi_endproc')
        data = bytearray(obj.read_bytes())
        section = next(s for s in core.elf_sections(data) if s['name'] == '.eh_frame')
        at, end = section['offset'], section['offset'] + section['size']
        records = []
        while at < end:
            length = struct.unpack_from('<I', data, at)[0]
            if not length:
                break
            records.append((at, length))
            at += 4 + length
        last, length = records[-1]
        self.assertEqual(data[last+length:last+length+4], bytes(4), 'assembler final CFI padding must be NOPs')
        struct.pack_into('<I', data, last, length - 4)
        mutated = self.root / 'unwind-terminator.o'
        mutated.write_bytes(data)
        out = self.link('unwind-padding-program', self.simple, mutated)
        run(out, expected=42)
        self.verify_index(out, 1)

    def test_empty_unwind_and_dead_first_contribution(self):
        empty = self.assemble('empty-unwind', '.globl marker\n.set marker,0\n.section .eh_frame,"a",@progbits\n.long 0,0')
        dead = self.assemble('dead-unwind', '.section .text.dead,"ax"\n.globl dead\ndead:\n.cfi_startproc\nret\n.cfi_endproc')
        live = self.assemble('live-unwind', '.text\n.globl answer\nanswer:\n.cfi_startproc\nmov $42,%eax\nret\n.cfi_endproc')
        for prefix in ((empty,), (dead,), (empty, dead)):
            with self.subTest(prefix=prefix):
                out = self.link('empty-and-live', self.simple, *prefix, live, options=('--gc-sections',))
                run(out, expected=42)
                self.verify_index(out, 1)

    def test_malformed_unwind_and_mutations(self):
        obj = self.assemble('unwind-mutations', '.text\n.globl answer\nanswer:\n.cfi_startproc\nmov $42,%eax\nret\n.cfi_endproc')
        original = obj.read_bytes()
        section = next(s for s in core.elf_sections(original) if s['name'] == '.eh_frame')
        offset = section['offset']
        fde = offset + 4 + struct.unpack_from('<I', original, offset)[0]
        cases = [('nobits-type', section['header']+4, 'I', 8),
                 ('bad-length', offset, 'I', 0xffffffff), ('truncated-record', offset, 'I', section['size']),
                 ('version', offset+8, 'B', 2), ('cie-pointer', fde+4, 'I', 1),
                 ('range', fde+12, 'I', 0x7fffffff)]
        path = self.root / 'bad-unwind.o'
        for name, at, fmt, value in cases:
            with self.subTest(name=name):
                data = bytearray(original)
                struct.pack_into('<' + fmt, data, at, value)
                path.write_bytes(data)
                self.rejected(self.simple, '.eh_frame', path)
        rng = random.Random(0xCF1)
        output = self.root / 'unwind-mutation-output'
        for _ in range(256):
            data = bytearray(original)
            for _ in range(rng.randrange(1, 5)):
                data[offset+rng.randrange(section['size'])] ^= rng.randrange(1, 256)
            path.write_bytes(data)
            result = subprocess.run([LINKER, '-o', str(output), str(self.simple), str(path)], capture_output=True, timeout=3)
            self.assertIn(result.returncode, (0, 1), result.stderr.decode(errors='replace'))
            self.assertNotIn(b'Fatal error', result.stderr)


if __name__ == '__main__':
    unittest.main(verbosity=2)
