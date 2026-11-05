#!/usr/bin/env python3
"""Loader execution and independent validation of initial dynamic ELF output."""
import os
from pathlib import Path
import shutil
import struct
import subprocess
import unittest

import integration as core

run = core.run
LINKER = core.LINKER
INTERPRETER = '/lib64/ld-linux-x86-64.so.2'


class DynamicTests(unittest.TestCase):
    setUpClass = classmethod(core.LinkerTests.setUpClass.__func__)
    tearDownClass = classmethod(core.LinkerTests.tearDownClass.__func__)
    assemble = classmethod(core.LinkerTests.assemble.__func__)
    link = core.LinkerTests.link
    rejected = core.LinkerTests.rejected

    def setUp(self):
        if not Path(INTERPRETER).is_file():
            self.skipTest('glibc x86-64 interpreter unavailable; loader gate remains unverified')

    def fixture(self):
        return self.assemble('dynamic-pointers', '''
.text
.globl _start,answer
.weak missing
.set constant,37
_start:
 lea answer(%rip),%rax
 cmp function_pointer(%rip),%rax
 jne failure
 cmp answer@GOTPCREL(%rip),%rax
 jne failure
 lea datum+3(%rip),%rax
 cmp data_pointer(%rip),%rax
 jne failure
 lea _DYNAMIC(%rip),%rax
 cmp dynamic_pointer(%rip),%rax
 jne failure
 lea __ehdr_start(%rip),%rax
 cmp header_pointer(%rip),%rax
 jne failure
 lea _end(%rip),%rax
 cmp end_pointer(%rip),%rax
 jne failure
 cmpq $0,weak_pointer(%rip)
 jne failure
 cmpq $37,absolute_pointer(%rip)
 jne failure
 cmpq $0,zero(%rip)
 jne failure
 lea function_pointer(%rip),%rsi
 mov $1,%eax
 mov $1,%edi
 mov $8,%edx
 syscall
 call *function_pointer(%rip)
 mov %eax,%edi
 mov $60,%eax
 syscall
failure: mov $99,%edi
 mov $60,%eax
 syscall
answer:
.cfi_startproc
 mov $42,%eax
 ret
.cfi_endproc
.data
function_pointer: .quad answer
data_pointer: .quad datum+3
dynamic_pointer: .quad _DYNAMIC
header_pointer: .quad __ehdr_start
end_pointer: .quad _end
weak_pointer: .quad missing
absolute_pointer: .quad constant
datum: .quad 0
.bss
.balign 4096
zero: .skip 1024
''')

    def decode(self, out, pie):
        data = out.read_bytes()
        self.assertEqual(struct.unpack_from('<H', data, 16)[0], 3 if pie else 2)
        sections = core.elf_sections(data)
        by_name = {s['name']: s for s in sections}
        for name, kind, entsize in (('.dynstr', 3, 0), ('.dynsym', 11, 24),
                                    ('.hash', 5, 4), ('.rela.dyn', 4, 24), ('.dynamic', 6, 16)):
            self.assertEqual((by_name[name]['kind'], by_name[name]['entsize']), (kind, entsize))
        self.assertEqual(by_name['.dynsym']['link'], by_name['.dynstr']['index'])
        self.assertEqual(by_name['.hash']['link'], by_name['.dynsym']['index'])
        self.assertEqual(by_name['.rela.dyn']['link'], by_name['.dynsym']['index'])
        self.assertEqual(by_name['.dynamic']['link'], by_name['.dynstr']['index'])
        dynamic = by_name['.dynamic']
        tags = dict(struct.unpack_from('<QQ', data, at) for at in
                    range(dynamic['offset'], dynamic['offset']+dynamic['size'], 16))
        for tag, name in ((4, '.hash'), (5, '.dynstr'), (6, '.dynsym'), (7, '.rela.dyn')):
            self.assertEqual(tags[tag], by_name[name]['address'])
        self.assertEqual((tags[9], tags[11], tags[0x6ffffffb]), (24, 24, 0x08000001 if pie else 1))
        rela = by_name['.rela.dyn']
        self.assertEqual(tags[8], rela['size'])
        relocations = [struct.unpack_from('<QQq', data, at) for at in
                       range(rela['offset'], rela['offset']+rela['size'], 24)]
        relative_count = 6 if pie else 0
        self.assertEqual(tags[0x6ffffff9], relative_count)
        self.assertEqual(len(relocations), relative_count+1)
        self.assertEqual(relocations[:relative_count], sorted(relocations[:relative_count]))
        for address, info, addend in relocations:
            if info != 8:
                self.assertEqual(info & 0xffffffff, 1)
                self.assertEqual(core.elf_symbols(data, kind=11)[info >> 32]['name'], 'missing')
            target = next(s for s in sections if s['address'] <= address < s['address']+s['size'] and s['flags'] & 2)
            self.assertTrue(target['flags'] & 1)
            self.assertEqual(struct.unpack_from('<Q', data, target['offset']+address-target['address'])[0], 0)
            self.assertGreaterEqual(addend, 0)
        phoff = struct.unpack_from('<Q', data, 32)[0]
        phnum = struct.unpack_from('<H', data, 56)[0]
        headers = [struct.unpack_from('<IIQQQQQQ', data, phoff+n*56) for n in range(phnum)]
        self.assertEqual([h[0] for h in headers[:3]], [6, 3, 1])
        self.assertEqual(headers[0][2:4], (64, 64 if pie else 0x400040))
        interp = headers[1]
        self.assertEqual(data[interp[2]:interp[2]+interp[5]], INTERPRETER.encode()+b'\0')
        phdynamic = next(h for h in headers if h[0] == 2)
        self.assertEqual((phdynamic[2], phdynamic[3], phdynamic[5]),
                         (dynamic['offset'], dynamic['address'], dynamic['size']))
        self.assertEqual(run('readelf', '-aW', out).stderr, b'')
        return {s['name']: s for s in core.elf_symbols(data)}

    def test_loader_relative_pointers_got_bss_and_layout_symbols(self):
        obj = self.fixture()
        for pie in (False, True):
            with self.subTest(pie=pie):
                options = ('--dynamic-linker', INTERPRETER) + (('-pie',) if pie else ())
                out = self.link('dynamic-pointers-program', obj, options=options)
                symbols = self.decode(out, pie)
                for _ in range(3):
                    runtime = struct.unpack('<Q', run(out, expected=42).stdout)[0]
                    if pie:
                        base = runtime - symbols['answer']['value']
                        self.assertGreater(base, 0x10000)
                        self.assertEqual(base % 4096, 0)
                    else:
                        self.assertEqual(runtime, symbols['answer']['value'])
                if pie:
                    reference = self.link('dynamic-pointers-gnu', obj, options=options, linker='ld')
                    self.assertEqual(len(run(reference, expected=42).stdout), 8)
                no_tools = dict(os.environ, PATH='/nonexistent')
                result = subprocess.run([LINKER, *options, '-o', str(self.root/'no-tools'), str(obj)],
                                        env=no_tools, capture_output=True, timeout=5)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(out.read_bytes(), (self.root/'no-tools').read_bytes())

    def test_pie_compiler_pic_unwind_and_debug(self):
        compilers = ['gcc']
        clang = shutil.which('clang-22') or shutil.which('clang')
        if clang:
            compilers.append(clang)
        source = self.root / 'pie-c.c'
        source.write_text('int datum=42; int *pointer=&datum;\n'
                          'int answer(void){return *pointer;}\n')
        for compiler in compilers:
            with self.subTest(compiler=compiler):
                obj = source.with_suffix('.o')
                run(compiler, '-c', '-O2', '-g', '-fPIC', '-fasynchronous-unwind-tables',
                    '-ffunction-sections', '-fdata-sections', source, '-o', obj)
                out = self.link('pie-c-program', self.simple, obj, options=('-pie', '--gc-sections'))
                run(out, expected=42)
                self.assertEqual(run('readelf', '-aW', '--debug-dump=frames,decodedline', out).stderr, b'')
                run(self.link('pie-c-gnu', self.simple, obj,
                              options=('-pie', '--dynamic-linker', INTERPRETER), linker='ld'), expected=42)

    def test_pie_rejects_textrel_narrow_absolute_and_invalid_interpreter(self):
        cases = [('.text\n.quad _start', 'read-only'),
                 ('.data\n.long _start', 'narrow absolute'),
                 ('.text\n.set fixed,42\n.long fixed - .', 'absolute or undefined')]
        for index, (source, diagnostic) in enumerate(cases):
            obj = self.assemble('bad-pie-'+str(index), '.text\n.globl _start\n_start: ret\n'+source)
            self.rejected(obj, diagnostic, options=('-pie',))
        self.rejected(self.simple, 'nonempty path', self.answer, options=('--dynamic-linker=',))
        self.rejected(self.simple, 'nonempty path', self.answer,
                      options=('--dynamic-linker='+'x'*4096,))
        out = self.link('pie-off', self.simple, self.answer, options=('-pie', '-no-pie'))
        self.assertEqual(struct.unpack_from('<H', out.read_bytes(), 16)[0], 2)
        run(out, expected=42)


if __name__ == '__main__':
    unittest.main(verbosity=2)
