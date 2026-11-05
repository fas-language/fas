#!/usr/bin/env python3
"""Compressed DWARF input, relocation semantics, bounds and atomic errors."""
import struct
import unittest

import integration as core

run = core.run


class CompressedTests(unittest.TestCase):
    setUpClass = classmethod(core.LinkerTests.setUpClass.__func__)
    tearDownClass = classmethod(core.LinkerTests.tearDownClass.__func__)
    assemble = classmethod(core.LinkerTests.assemble.__func__)
    link = core.LinkerTests.link
    rejected = core.LinkerTests.rejected

    def object(self):
        source = self.root / 'compressed.c'
        source.write_text('int bias=40;\nint answer(void){return bias+2;}\n')
        obj = source.with_suffix('.o')
        run('gcc', '-c', '-O0', '-g', '-gz=zlib', '-fPIC', source, '-o', obj)
        self.assertTrue(any(s['flags'] & 0x800 for s in core.elf_sections(obj.read_bytes())))
        return obj

    def test_dwarf_relocations_and_source_lookup(self):
        obj = self.object()
        for tool in (core.LINKER, 'ld'):
            for gc in (False, True):
                out = self.link('compressed', self.simple, obj, linker=tool,
                                options=('--gc-sections',) if gc else ())
                run(out, expected=42)
                if tool == core.LINKER:
                    self.assertFalse(any(s['flags'] & 0x800 for s in core.elf_sections(out.read_bytes())))
                self.assertEqual(run('readelf', '--debug-dump=info,decodedline', out).stderr, b'')
                answer = next(s for s in core.elf_symbols(out.read_bytes()) if s['name'] == 'answer')
                self.assertIn(b'compressed.c:2', run('addr2line', '-e', out, hex(answer['value'])).stdout)

    def test_bad_headers_streams_and_limits_preserve_output(self):
        obj = self.object()
        original = obj.read_bytes()
        section = next(s for s in core.elf_sections(original) if s['flags'] & 0x800)
        shoff = struct.unpack_from('<Q', original, 40)[0]
        header = shoff + section['index'] * 64
        at = section['offset']
        mutations = [
            (at, '<I', 2, 'compression type'),
            (at + 4, '<I', 1, 'reserved field'),
            (at + 8, '<Q', 129 * 1024 * 1024, 'size'),
            (at + 16, '<Q', 3, 'alignment'),
            (header + 8, '<Q', section['flags'] | 2, 'non-allocatable'),
            (header + 32, '<Q', 23, 'compression header'),
            (at + section['size'] - 1, '<B', original[at + section['size'] - 1] ^ 1, 'checksum'),
        ]
        for offset, fmt, value, message in mutations:
            with self.subTest(message=message):
                damaged = bytearray(original)
                struct.pack_into(fmt, damaged, offset, value)
                path = self.root / 'compressed-bad.o'
                path.write_bytes(damaged)
                self.rejected(self.simple, message, path)


if __name__ == '__main__':
    unittest.main(verbosity=2)
