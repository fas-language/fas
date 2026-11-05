#!/usr/bin/env python3
"""GNU property aggregation checked against GNU ld and independent decoding."""
import random
import struct
import subprocess
import unittest

import integration as core

run = core.run
LINKER = core.LINKER
AND, NEEDED, USED = 0xc0000002, 0xc0008002, 0xc0010002


class PropertyTests(unittest.TestCase):
    setUpClass = classmethod(core.LinkerTests.setUpClass.__func__)
    tearDownClass = classmethod(core.LinkerTests.tearDownClass.__func__)
    assemble = classmethod(core.LinkerTests.assemble.__func__)
    link = core.LinkerTests.link
    rejected = core.LinkerTests.rejected

    def note(self, name, code, properties):
        if not properties:
            return self.assemble(name, code)
        source = code + '\n.section .note.gnu.property,"a",@note\n.p2align 3\n'
        source += f'.long 4,{16 * len(properties)},5\n.asciz "GNU"\n'
        for kind, value in sorted(properties):
            source += f'.long {kind},4,{value},0\n'
        return self.assemble(name, source)

    def properties(self, out):
        data = out.read_bytes()
        notes = [s for s in core.elf_sections(data) if s['name'] == '.note.gnu.property']
        if not notes:
            return {}
        self.assertEqual(len(notes), 1)
        s = notes[0]
        self.assertEqual((s['kind'], s['flags'], s['align']), (7, 2, 8))
        self.assertEqual(struct.unpack_from('<III4s', data, s['offset']), (4, s['size']-16, 5, b'GNU\0'))
        values = {}
        for at in range(s['offset']+16, s['offset']+s['size'], 16):
            kind, size, value, pad = struct.unpack_from('<IIII', data, at)
            self.assertEqual((size, pad), (4, 0))
            values[kind] = value
        return values

    def test_property_merge_matches_gnu_and_does_not_overclaim(self):
        rng = random.Random(0xCE7)
        for trial in range(24):
            first, second = [], []
            for kind in (AND, NEEDED, USED):
                if rng.randrange(2):
                    first.append((kind, rng.randrange(1, 16)))
                if rng.randrange(2):
                    second.append((kind, rng.randrange(1, 16)))
            start = self.note('property-start', '.text\n.globl _start\n_start: call answer\nmov %eax,%edi\nmov $60,%eax\nsyscall', first)
            answer = self.note('property-answer', '.text\n.globl answer\nanswer: mov $42,%eax\nret', second)
            for gc in ((), ('--gc-sections',)):
                with self.subTest(trial=trial, gc=gc):
                    out = self.link('properties', start, answer, options=gc)
                    reference = self.link('properties-gnu', start, answer, options=gc, linker='ld')
                    self.assertEqual(self.properties(out), self.properties(reference))
                    run(out, expected=42)
                    self.assertEqual(run('readelf', '-nW', out).stderr, b'')
        marked = self.note('marked-answer', '.text\n.globl answer\nanswer: mov $42,%eax\nret', [(AND, 3), (NEEDED, 1), (USED, 1)])
        out = self.link('unmarked-start', self.simple, marked)
        self.assertEqual(self.properties(out), {NEEDED: 1})
        data = out.read_bytes()
        note = next(s for s in core.elf_sections(data) if s['kind'] == 7)
        phoff = struct.unpack_from('<Q', data, 32)[0]
        phnum = struct.unpack_from('<H', data, 56)[0]
        headers = [struct.unpack_from('<IIQQQQQQ', data, phoff+i*56) for i in range(phnum)]
        for kind in (4, 0x6474e553):
            ph = next(p for p in headers if p[0] == kind)
            self.assertEqual((ph[1], ph[2], ph[3], ph[5], ph[6], ph[7]),
                             (4, note['offset'], note['address'], note['size'], note['size'], 8))

    def test_synthetic_common_and_predefined_do_not_clear_properties(self):
        start = self.note('properties-with-common', '.text\n.globl _start\n_start: mov $60,%eax\nmov $42,%edi\nsyscall\n.data\n.quad common,__ehdr_start\n.comm common,8,8', [(AND, 3)])
        out = self.link('synthetic-properties', start)
        self.assertEqual(self.properties(out), {AND: 3})
        run(out, expected=42)

    def test_repeated_notes_in_one_section_and_empty_descriptor(self):
        start = self.note('repeated-property-start', '.text\n.globl _start\n_start: call answer\nmov %eax,%edi\nmov $60,%eax\nsyscall', [(AND,3),(NEEDED,1)])
        for values in ((3,3),(3,1)):
            source = '.text\n.globl answer\nanswer: mov $42,%eax; ret\n.section .note.gnu.property,"a",@note\n.p2align 3\n'
            for value in values:
                source += f'.long 4,32,5\n.asciz "GNU"\n.long {AND},4,{value},0\n.long {NEEDED},4,2,0\n'
            obj = self.assemble('repeated-properties', source)
            out = self.link('repeated-property-program', start, obj)
            reference = self.link('repeated-property-reference', start, obj, linker='ld')
            self.assertEqual(self.properties(out), self.properties(reference))
            run(out, expected=42)
        empty = self.assemble('empty-property-note', '.text\n.globl answer\nanswer: mov $42,%eax; ret\n.section .note.gnu.property,"a",@note\n.p2align 3\n.long 4,0,5\n.asciz "GNU"')
        out = self.link('empty-property-program', start, empty)
        self.assertEqual(self.properties(out), {NEEDED:1})
        run(out, expected=42)

    def test_default_gcc_object_and_malformed_notes(self):
        source = self.root / 'normal.c'
        source.write_text('int answer(void){return 42;}')
        obj = source.with_suffix('.o')
        run('gcc', '-c', '-O1', '-g', '-fPIC', source, '-o', obj)
        run(self.link('normal-gcc', self.simple, obj), expected=42)
        marked = self.note('bad-property', '.text\n.globl answer\nanswer: mov $42,%eax\nret', [(AND, 3), (NEEDED, 1)])
        original = marked.read_bytes()
        s = next(s for s in core.elf_sections(original) if s['kind'] == 7)
        cases = [('owner', 12, 0), ('type', 8, 6), ('descriptor', 4, 999),
                 ('unknown', 16, 0xc0000042), ('payload-size', 20, 8),
                 ('padding', 28, 1), ('duplicate', 32, AND)]
        path = self.root / 'malformed-property.o'
        for name, at, value in cases:
            with self.subTest(name=name):
                data = bytearray(original)
                struct.pack_into('<I', data, s['offset']+at, value)
                path.write_bytes(data)
                self.rejected(self.simple, 'GNU property' if name == 'unknown' else '.note.gnu.property', path)
        rng = random.Random(0x90)
        for _ in range(128):
            data = bytearray(original)
            data[s['offset']+rng.randrange(s['size'])] ^= rng.randrange(1, 256)
            path.write_bytes(data)
            result = subprocess.run([LINKER, '-o', str(self.root/'mutation-output'), str(self.simple), str(path)], capture_output=True, timeout=3)
            self.assertIn(result.returncode, (0, 1), result.stderr)
            self.assertNotIn(b'Fatal error', result.stderr)


if __name__ == '__main__':
    unittest.main(verbosity=2)
