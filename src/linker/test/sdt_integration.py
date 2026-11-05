#!/usr/bin/env python3
"""SystemTap v3 notes, section-symbol COMDAT signatures and relocation bounds."""
import random
import shutil
import struct
import subprocess
import unittest

import integration as core

run = core.run


class SdtTests(unittest.TestCase):
    setUpClass = classmethod(core.LinkerTests.setUpClass.__func__)
    tearDownClass = classmethod(core.LinkerTests.tearDownClass.__func__)
    assemble = classmethod(core.LinkerTests.assemble.__func__)
    link = core.LinkerTests.link
    rejected = core.LinkerTests.rejected

    def fixture(self):
        source = r'''
.section .text.answer,"ax",@progbits
.globl answer
answer: mov $42,%eax; ret
.section .text.dead_probe,"ax",@progbits
.byte 0x90
.globl dead_probe
dead_probe: ret
.section .stapsdt.base,"aG",@progbits,.stapsdt.base,comdat
.weak _.stapsdt.base
.hidden _.stapsdt.base
_.stapsdt.base: .byte 0
'''
        for symbol in ('answer', 'dead_probe'):
            source += f'''
.section .note.stapsdt,"",@note
.p2align 2
.long 8,3f-2f,3
.asciz "stapsdt"
2: .quad {symbol},_.stapsdt.base,0
.asciz "fas"
.asciz "{symbol}"
.asciz ""
3: .balign 4
'''
        return self.assemble('sdt-probes', source)

    def test_probe_notes_partial_interoperability_and_gc(self):
        obj = self.fixture()
        signature = next(s for s in core.elf_symbols(obj.read_bytes())
                         if s['info'] & 15 == 3 and s['section'] ==
                         next(s['index'] for s in core.elf_sections(obj.read_bytes()) if s['name']=='.stapsdt.base'))
        self.assertEqual(signature['name'], '')
        tools = [core.LINKER, 'ld'] + (['ld.lld-22'] if shutil.which('ld.lld-22') else [])
        for producer in tools:
            partial = self.link('sdt-partial.o', obj, linker=producer, options=('-r',))
            for consumer in tools:
                for gc in (False, True):
                    with self.subTest(producer=producer, consumer=consumer, gc=gc):
                        out = self.link('sdt-program', self.simple, partial, linker=consumer,
                                        options=('--gc-sections',) if gc else ())
                        run(out, expected=42)
                        report = run('readelf', '-nW', out)
                        self.assertEqual(report.stderr, b'')
                        self.assertIn(b'Provider: fas', report.stdout)
                        self.assertIn(b'Name: answer', report.stdout)
                        data = out.read_bytes()
                        note = next(s for s in core.elf_sections(data) if s['name']=='.note.stapsdt')
                        self.assertEqual((note['kind'], note['flags'], note['address']), (7,0,0))
                        symbols = {s['name'] for s in core.elf_symbols(data)}
                        if consumer == core.LINKER or not gc:
                            self.assertEqual('dead_probe' in symbols, not gc)
                        if consumer == core.LINKER and gc:
                            first_length=struct.unpack_from('<I',data,note['offset']+4)[0]
                            second=note['offset']+20+((first_length+3)//4)*4
                            self.assertEqual(struct.unpack_from('<Q',data,second+20)[0],0)

    def test_bad_descriptors_strings_and_relocation_fields(self):
        obj = self.fixture()
        original = obj.read_bytes()
        sections = core.elf_sections(original)
        note = next(s for s in sections if s['name']=='.note.stapsdt')
        at = note['offset']
        edits = [(at,'<I',7), (at+4,'<I',0xffffffff), (at+8,'<I',4),
                 (at+12,'<B',0), (at+44,'<B',0),
                 (note['header']+4,'<I',1),
                 (note['header']+8,'<Q',2)]
        rela = next(s for s in sections if s['kind']==4 and s['info']==note['index'])
        edits += [(rela['offset'],'<Q',0), (rela['offset']+8,'<I',2)]
        bad = self.root/'sdt-malformed.o'
        for offset, fmt, value in edits:
            with self.subTest(offset=offset):
                data = bytearray(original)
                struct.pack_into(fmt,data,offset,value)
                bad.write_bytes(data)
                self.rejected(self.simple, 'SDT' if offset==note['header']+8 else 'stapsdt', bad)
        rng = random.Random(0x5D7)
        for _ in range(128):
            data = bytearray(original)
            data[at+rng.randrange(note['size'])] ^= rng.randrange(1,256)
            bad.write_bytes(data)
            result = subprocess.run([core.LINKER,'-o',str(self.root/'sdt-mutated'),str(self.simple),str(bad)],
                                    capture_output=True, timeout=3)
            self.assertIn(result.returncode,(0,1),result.stderr)
            self.assertNotIn(b'Fatal error',result.stderr)


if __name__=='__main__':
    unittest.main(verbosity=2)
