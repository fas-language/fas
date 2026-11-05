#!/usr/bin/env python3
"""Static GOT relocation correctness with GNU assembly and GCC/Clang PIC."""
import json
import shutil
import struct
import unittest

import integration as core

run = core.run
LINKER = core.LINKER


class PicTests(unittest.TestCase):
    setUpClass = classmethod(core.LinkerTests.setUpClass.__func__)
    tearDownClass = classmethod(core.LinkerTests.tearDownClass.__func__)
    assemble = classmethod(core.LinkerTests.assemble.__func__)
    link = core.LinkerTests.link
    rejected = core.LinkerTests.rejected

    def test_gotpcrel_variants_and_slot_values(self):
        source = ('.text\n.globl _start\n_start: mov answer@GOTPCREL(%rip),%rax\ncall *%rax\n'
                  'mov %eax,%ebx\ncall *answer@GOTPCREL(%rip)\nadd %ebx,%eax\nshr %eax\n'
                  'mov %eax,%edi\nmov $60,%eax\nsyscall\n.data\n.quad _GLOBAL_OFFSET_TABLE_')
        obj = self.assemble('got-start', source)
        for mode in ('yes', 'no'):
            variant = self.root / ('got-start-' + mode + '.o')
            run('as', '--64', '-mrelax-relocations=' + mode, '-o', variant, obj.with_suffix('.s'))
            relocs = run('readelf', '-rW', variant).stdout
            if mode == 'yes':
                self.assertIn(b'R_X86_64_REX_GOTPCRELX', relocs)
                self.assertIn(b'R_X86_64_GOTPCRELX', relocs)
            else:
                self.assertIn(b'R_X86_64_GOTPCREL', relocs)
            out = self.link('got-program-' + mode, variant, self.answer)
            run(out, expected=42)
            run(self.link('got-reference-' + mode, variant, self.answer, linker='ld'), expected=42)
            data = out.read_bytes()
            got = next(s for s in core.elf_sections(data) if s['name'] == '.got')
            symbols = {s['name']: s for s in core.elf_symbols(data)}
            self.assertEqual(got['size'], 8, 'one shared slot for both references')
            self.assertEqual(struct.unpack_from('<Q', data, got['offset'])[0], symbols['answer']['value'])
            self.assertEqual(symbols['_GLOBAL_OFFSET_TABLE_']['value'], got['address'])
            self.assertEqual(run('readelf', '-aW', out).stderr, b'')

    def test_weak_zero_local_scoping_and_gc(self):
        a = self.assemble('got-local-a', '.data\nlocal: .long 20\n.section .text.answer,"ax",@progbits\n'
            '.globl answer\nanswer: mov local@GOTPCREL(%rip),%rax\nmov (%rax),%eax\nret\n'
            '.section .text.dead,"ax",@progbits\ndead: mov absent@GOTPCREL(%rip),%rax\nret')
        b = self.assemble('got-local-b', '.data\nlocal: .long 22\n.text\n.globl _start\n.weak weak_absent\n'
            '_start: mov weak_absent@GOTPCREL(%rip),%rax\ntest %rax,%rax\njne bad\n'
            'call answer\nmov local@GOTPCREL(%rip),%rdx\nadd (%rdx),%eax\nmov %eax,%edi\njmp done\n'
            'bad: mov $99,%edi\ndone: mov $60,%eax\nsyscall')
        for tool, name in ((LINKER, 'fas'), ('ld', 'gnu')):
            run(self.link('got-locals-' + name, b, a, options=('--gc-sections',), linker=tool), expected=42)
        self.rejected(b, 'GOT references undefined symbol absent', a)

    def test_pic_compiler_corpus(self):
        compilers = ['gcc']
        clang = shutil.which('clang-22') or shutil.which('clang')
        if clang:
            compilers.append(clang)
        c1 = self.root / 'pic-support.c'
        c1.write_text('int external_value = 40; int helper(int x) { return x + external_value; }\n')
        c2 = self.root / 'pic-answer.c'
        c2.write_text('extern int helper(int); extern int external_value;\n'
                      'int answer(void) { int (*p)(int) = helper; return p(external_value - 38); }\n')
        for compiler in compilers:
            for optimization in ('-O0', '-O2'):
                for no_plt in ([], ['-fno-plt']):
                    with self.subTest(compiler=compiler, optimization=optimization, no_plt=no_plt):
                        objects = []
                        for source in (c1, c2):
                            obj = source.with_suffix('.o')
                            run(compiler, '-c', optimization, '-fPIC', '-ffreestanding', '-fno-stack-protector',
                                '-fno-unwind-tables', '-fno-asynchronous-unwind-tables', '-fno-ident',
                                '-fcf-protection=none', *(['-fno-addrsig'] if compiler != 'gcc' else []),
                                *no_plt, '-o', obj, source)
                            objects.append(obj)
                        for tool, name in ((LINKER, 'fas'), ('ld', 'gnu')):
                            run(self.link('pic-corpus-' + name, self.simple, *objects, linker=tool), expected=42)

    def test_got_overflow_and_statistics(self):
        obj = self.assemble('got-overflow', '.text\nmov answer@GOTPCREL+0x100000000(%rip),%rax')
        self.rejected(self.simple, 'GOTPCRELX overflow', self.answer, obj)
        obj = self.assemble('got-stats', '.text\nmov answer@GOTPCREL(%rip),%rax\nmov answer@GOTPCREL(%rip),%rdx')
        output = self.root / 'got-stat-output'
        result = run(LINKER, '--stats', '-o', output, self.simple, self.answer, obj)
        stats = json.loads(result.stderr)
        self.assertEqual(stats['counts']['got_entries'], 1)
        run(output, expected=42)


if __name__ == '__main__':
    unittest.main(verbosity=2)
