#!/usr/bin/env python3
"""Independent layout-symbol and diagnostic map-file checks."""
import os
import shlex
import struct
import unittest

import integration as core

run = core.run
LINKER = core.LINKER


class LayoutTests(unittest.TestCase):
    setUpClass = classmethod(core.LinkerTests.setUpClass.__func__)
    tearDownClass = classmethod(core.LinkerTests.tearDownClass.__func__)
    assemble = classmethod(core.LinkerTests.assemble.__func__)
    link = core.LinkerTests.link
    rejected = core.LinkerTests.rejected

    def test_large_nobits_address_space_without_file_materialization(self):
        size = 1024*1024*1024 - 8*1024*1024
        obj = self.assemble('large-nobits', f'''
.section .bss.large,"aw",@nobits
.p2align 21
.globl large_begin,large_end
large_begin: .skip {size}
large_end:
.data
.quad large_end
.long large_end-.
''')
        self.assertLess(obj.stat().st_size, 4096)
        for tool in (LINKER, 'ld'):
            with self.subTest(tool=tool):
                out = self.link('large-nobits-out', self.simple, self.answer, obj, linker=tool)
                data = out.read_bytes()
                self.assertLess(len(data), 3*1024*1024)
                symbols = {s['name']: s for s in core.elf_symbols(data)}
                begin, end = (symbols[n]['value'] for n in ('large_begin','large_end'))
                self.assertEqual(begin % (2*1024*1024), 0)
                self.assertEqual(end - begin, size)
                target = next(s for s in core.elf_sections(data) if s['name'] == '.data')
                self.assertEqual(struct.unpack_from('<Q', data, target['offset'])[0], end)
                displacement = struct.unpack_from('<i', data, target['offset']+8)[0]
                self.assertEqual(target['address']+8+displacement, end)
                loads = [p for p in core.elf_segments(data) if p['kind'] == 1]
                writable = next(p for p in loads if p['flags'] & 2
                                and p['address'] <= begin and end <= p['address']+p['memory_size'])
                self.assertGreaterEqual(writable['memory_size'], size)
                self.assertLess(writable['file_size'], 4096)
                self.assertEqual(run('readelf', '-aW', out).stderr, b'')
        # The image is inspected, not run: no gigabyte allocation in the test.

    def test_image_limits_fail_atomically_and_gc_can_remove_large_sections(self):
        objects = [self.assemble('oversized-image-'+str(n),
                                f'.section .bss.large{n},"aw",@nobits\n.skip {600*1024*1024}')
                   for n in range(2)]
        self.rejected(self.simple, 'BSS layout', self.answer, *objects)
        out = self.link('large-sections-gc', self.simple, self.answer, *objects,
                        options=('--gc-sections',))
        self.assertLess(out.stat().st_size, 16384)
        run(out, expected=42)
        section = self.assemble('oversized-section',
                                f'.bss\n.skip {1024*1024*1024+1}')
        self.rejected(self.simple, 'section size', self.answer, section)

    def test_predefined_symbols_match_load_extents(self):
        names = ('__executable_start', '__ehdr_start', '__bss_start',
                 '_etext', 'etext', '_edata', 'edata', '_end', 'end')
        obj = self.assemble('predefined', '.text\n.globl _start\n_start: mov $60,%eax\nxor %edi,%edi\nsyscall\n'
                            '.data\n' + '\n'.join('.quad ' + n for n in names) +
                            '\n.bss\n.balign 128\n.skip 257\n.comm common,511,256')
        out = self.link('predefined-out', obj)
        run(out)
        data = out.read_bytes()
        phoff = struct.unpack_from('<Q', data, 32)[0]
        phnum = struct.unpack_from('<H', data, 56)[0]
        loads = [struct.unpack_from('<IIQQQQQQ', data, phoff + i * 56) for i in range(phnum)]
        loads = [p for p in loads if p[0] == 1]
        base = min(p[3] for p in loads)
        file_end = max(p[3] + p[5] for p in loads)
        memory_end = max(p[3] + p[6] for p in loads)
        text_end = max(p[3] + p[6] for p in loads if p[1] & 1)
        expected = (base, base, file_end, text_end, text_end, file_end, file_end, memory_end, memory_end)
        syms = {s['name']: s for s in core.elf_symbols(data)}
        self.assertEqual(tuple(syms[n]['value'] for n in names), expected)
        section = next(s for s in core.elf_sections(data) if s['name'] == '.data')
        self.assertEqual(struct.unpack_from('<9Q', data, section['offset']), expected)
        self.assertEqual(run('readelf', '-aW', out).stderr, b'')

    def test_predefined_symbols_are_demanded_and_overridable(self):
        plain = self.link('plain-predefs', self.simple, self.answer)
        self.assertNotIn('_end', [s['name'] for s in core.elf_symbols(plain.read_bytes())])
        ref = self.assemble('weak-end', '.weak _end\n.hidden __ehdr_start\n.data\n.quad _end\n.quad __ehdr_start')
        own = self.assemble('own-end', '.globl _end\n.set _end,1234')
        for extra, value in (([], None), ([own], 1234)):
            out = self.link('own-end-' + str(value), self.simple, self.answer, ref, *extra)
            symbols = {s['name']: s for s in core.elf_symbols(out.read_bytes())}
            self.assertEqual(symbols['__ehdr_start']['other'], 2)
            self.assertEqual(symbols['__ehdr_start']['value'], 0x400000)
            if value is not None:
                self.assertEqual(symbols['_end']['value'], value)
            else:
                self.assertGreater(symbols['_end']['value'], 0x400000)
        run(LINKER, '-e', '_end', '-o', self.root / 'bad-entry', self.simple, self.answer, ref, expected=1)

    def test_maps_match_elf_and_are_deterministic(self):
        mapping = self.root / 'output.map'
        plain = self.link('map-plain', self.start, self.support)
        mapped = self.link('map-enabled', self.start, self.support, options=('-Map', mapping))
        first = mapping.read_bytes()
        self.assertEqual(plain.read_bytes(), mapped.read_bytes())
        self.link('map-enabled-again', self.start, self.support, options=('--Map=' + str(mapping),))
        self.assertEqual(first, mapping.read_bytes())
        rows = [shlex.split(line) for line in first.decode().splitlines() if not line.startswith('#')]
        data = mapped.read_bytes()
        sections = core.elf_sections(data)
        symbols = core.elf_symbols(data)[1:]
        section_rows = [r for r in rows if r[0] == 'section']
        # Map rows describe input contributions, including empty ones. Several
        # contributions can now share one conventional output section header.
        inputs = {(str(path), s['name']): s for path in (self.start, self.support)
                  for s in core.elf_sections(path.read_bytes()) if s['flags'] & 2}
        self.assertEqual(len(section_rows), len(inputs))
        covered = set()
        for row in section_rows:
            source = inputs[(row[8], row[7])]
            address, offset, file_size, size, alignment, flags = [int(v, 16) for v in row[1:7]]
            self.assertEqual([file_size, size, alignment, flags],
                [0 if source['kind'] == 8 else source['size'], source['size'], source['align'], source['flags']])
            name = '.text' if row[7].startswith('.text.') else row[7]
            target = next(s for s in sections if s['name'] == name and
                          s['address'] <= address and address+size <= s['address']+s['size'])
            self.assertEqual(address-target['address'], offset-target['offset'])
            self.assertEqual(address % alignment, 0)
            covered.add(target['index'])
        self.assertEqual(covered, {s['index'] for s in sections[1:-3]})
        symbol_rows = [r for r in rows if r[0] == 'symbol']
        self.assertEqual(len(symbol_rows), len(symbols))
        for row, s in zip(symbol_rows, symbols):
            self.assertEqual((int(row[1], 16), int(row[2], 16), row[4]), (s['value'], s['size'], s['name']))
        self.assertEqual(mapping.stat().st_mode & 0o777, 0o644)

    def test_map_archive_common_and_synthetic_provenance(self):
        member = self.assemble('map-member', '.text\n.globl answer\nanswer: mov $42,%eax\nret\n.comm shared,32,16\n.data\n.quad _end')
        archive = self.root / 'map-library.a'
        run('ar', 'rcs', archive, member)
        mapping = self.root / 'archive.map'
        out = self.link('map-archive', self.simple, archive, options=('-Map=' + str(mapping),))
        run(out, expected=42)
        text = mapping.read_text()
        self.assertIn('map-library.a(map-member.o)', text)
        self.assertIn('<common>', text)
        self.assertIn('<linker symbols>', text)

    def test_map_errors_preserve_inputs_and_outputs(self):
        output = self.root / 'preserve-executable'
        mapping = self.root / 'preserve.map'
        def reject(map_path, *objects, output_path=output):
            output.write_bytes(b'old executable')
            output.chmod(0o640)
            mapping.write_bytes(b'old map')
            p = run(LINKER, '-Map', map_path, '-o', output_path, *objects, expected=1)
            self.assertNotIn(b'Fatal error', p.stderr)
            self.assertEqual(output.read_bytes(), b'old executable')
            self.assertEqual(output.stat().st_mode & 0o777, 0o640)
            self.assertEqual(mapping.read_bytes(), b'old map')
            self.assertFalse(list(self.root.glob('.fas-ld-*.tmp')))
        reject(mapping, self.simple)  # undefined: neither output gets replaced
        reject(self.root / 'missing-directory' / 'out.map', self.simple, self.answer)
        reject(output, self.simple, self.answer)
        reject(self.root / '.' / output.name, self.simple, self.answer)
        hardlink = self.root / 'map-output-hardlink'
        os.link(output, hardlink)
        reject(hardlink, self.simple, self.answer)
        before = self.simple.read_bytes()
        reject(self.simple, self.simple, self.answer)
        self.assertEqual(self.simple.read_bytes(), before)
        reject(self.root, self.simple, self.answer)


if __name__ == '__main__':
    unittest.main(verbosity=2)
