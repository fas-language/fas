#!/usr/bin/env python3
"""COMDAT and section collection against GNU ld, including malformed groups."""
import random
import struct
import unittest

import integration as core

run = core.run
LINKER = core.LINKER


class SectionTests(unittest.TestCase):
    setUpClass = classmethod(core.LinkerTests.setUpClass.__func__)
    tearDownClass = classmethod(core.LinkerTests.tearDownClass.__func__)
    assemble = classmethod(core.LinkerTests.assemble.__func__)
    link = core.LinkerTests.link
    rejected = core.LinkerTests.rejected

    def compare(self, name, objects, expected=42, options=()):
        for linker, suffix in ((LINKER, 'fas'), ('ld', 'gnu')):
            out = self.link(name + suffix, *objects, options=options, linker=linker)
            run(out, expected=expected)
            self.assertEqual(run('readelf', '-aW', out).stderr, b'')

    def test_comdat_first_group_wins_as_a_unit(self):
        def candidate(name, value):
            return self.assemble(name, '.section .text.answer,"axG",@progbits,signature,comdat\n'
                '.globl answer\nanswer: mov local_data(%rip),%eax\nret\n'
                '.section .rodata.answer,"aG",@progbits,signature,comdat\n'
                f'local_data: .long {value}')
        a, b = candidate('comdat-a', 42), candidate('comdat-b', 7)
        self.compare('comdat-first', [self.simple, a, b])
        self.compare('comdat-reverse', [self.simple, b, a], expected=7)
        out = self.link('comdat-shape', self.simple, a, b)
        syms = core.elf_symbols(out.read_bytes())
        self.assertEqual([s['name'] for s in syms].count('local_data'), 1)
        self.assertTrue(all(s['flags'] & 0x200 == 0 for s in core.elf_sections(out.read_bytes())))

    def test_archive_comdat_and_gc_group_retention(self):
        first = self.assemble('group-first', '.section .text.answer,"axG",@progbits,signature,comdat\n'
            '.globl answer\nanswer: mov $42,%eax\nret\n'
            '.section .rodata.companion,"aG",@progbits,signature,comdat\n.globl companion\ncompanion: .long 19')
        second = self.assemble('group-second', '.section .text.answer,"axG",@progbits,signature,comdat\n'
            '.globl answer\nanswer: mov $7,%eax\nret\n.text\n.globl extra\nextra: ret')
        archive = self.root / 'groups.a'
        run('ar', 'rcs', archive, first, second)
        self.compare('archive-comdat', [self.simple, archive], options=('-u', 'extra', '--gc-sections'))
        out = self.link('group-companion', self.simple, first, second, options=('--gc-sections',))
        self.assertIn('companion', [s['name'] for s in core.elf_symbols(out.read_bytes())])
        # Non-COMDAT groups with identical signatures are not duplicates.
        regular = self.assemble('regular-group', '.section .data.extra,"awG",@progbits,signature\n.quad 1')
        self.link('regular-groups', self.simple, self.answer, regular, regular)

    def test_gc_drops_dead_cycles_and_errors_only_on_live_references(self):
        obj = self.assemble('gc-graph', '.section .text.answer,"ax",@progbits\n.globl answer\n'
            'answer: mov live_data(%rip),%eax\nret\n'
            '.section .data.live,"aw",@progbits\nlive_data: .long 42\n'
            '.section .text.dead,"ax",@progbits\n.globl dead\ndead: call missing\ncall cycle\nret\n'
            '.section .text.cycle,"ax",@progbits\ncycle: call dead\nret\n'
            '.section .rodata.retained,"aR",@progbits\n.globl retained\nretained: .quad 123\n')
        self.rejected(self.simple, 'undefined symbol missing', obj)
        self.compare('gc-graph', [self.simple, obj], options=('--gc-sections',))
        out = self.link('gc-symbols', self.simple, obj, options=('--gc-sections',))
        names = [s['name'] for s in core.elf_symbols(out.read_bytes())]
        self.assertIn('retained', names)
        self.assertIn('live_data', names)
        self.assertNotIn('dead', names)
        self.assertNotIn('cycle', names)
        self.rejected(self.simple, 'undefined symbol missing', obj, options=('--gc-sections', '-udead'))
        self.rejected(self.simple, 'undefined symbol missing', obj, options=('--gc-sections', '--no-gc-sections'))

    def test_undefined_roots_extract_and_keep_archive_sections(self):
        member = self.assemble('rooted', '.section .text.rooted,"ax",@progbits\n.globl rooted\nrooted: ret\n'
            '.section .data.dead,"aw",@progbits\n.globl unused_data\nunused_data: .quad 9')
        archive = self.root / 'roots.a'
        run('ar', 'rcs', archive, member)
        for index, option in enumerate((('-u', 'rooted'), ('-urooted',), ('--undefined=rooted',), ('--undefined', 'rooted'))):
            out = self.link('rooted-' + str(index), self.simple, self.answer, archive,
                            options=('--gc-sections', *option))
            run(out, expected=42)
            names = [s['name'] for s in core.elf_symbols(out.read_bytes())]
            self.assertIn('rooted', names)
            self.assertNotIn('unused_data', names)
        self.compare('rooted-oracle', [self.simple, self.answer, archive], options=('--gc-sections', '-u', 'rooted'))

    def test_gc_follows_canonical_definitions_and_weak_references(self):
        weak = self.assemble('gc-weak', '.section .text.answer,"ax",@progbits\n.weak answer\nanswer: call absent\nret')
        strong = self.assemble('gc-strong', '.section .text.answer,"ax",@progbits\n.globl answer\nanswer: mov $42,%eax\nret\n'
            '.section .data.ptr,"awR",@progbits\n.weak maybe\n.quad maybe\n'
            '.section .text.maybe,"ax",@progbits\nmaybe: ret')
        self.compare('gc-canonical', [self.simple, weak, strong], options=('--gc-sections',))
        out = self.link('gc-weak-symbols', self.simple, weak, strong, options=('--gc-sections',))
        self.assertIn('maybe', [s['name'] for s in core.elf_symbols(out.read_bytes())])

    def test_seeded_section_graphs(self):
        rng = random.Random(0xFA504)
        for trial in range(12):
            count = rng.randrange(4, 16)
            source = '.text\n.globl _start\n_start: call f0\nmov %eax,%edi\nmov $60,%eax\nsyscall\n'
            total = 0
            for index in range(count):
                constant = rng.randrange(1, 7)
                total += constant
                source += f'.section .text.f{index},"ax",@progbits\nf{index}: '
                source += (f'call f{index+1}\nadd ${constant},%eax\nret\n' if index + 1 < count
                           else f'mov ${constant},%eax\nret\n')
            source += '.section .text.unused,"ax",@progbits\nunused: call undefined_dead\nret\n'
            obj = self.assemble('section-graph-' + str(trial), source)
            self.compare('section-graph-' + str(trial), [obj], expected=total, options=('--gc-sections',))

    def test_malformed_groups(self):
        obj = self.assemble('group-malformed-base', '.section .text.answer,"axG",@progbits,signature,comdat\n'
                            '.globl answer\nanswer: ret\n.section .data.answer,"awG",@progbits,signature,comdat\n.quad answer')
        original = obj.read_bytes()
        sections = core.elf_sections(original)
        group = next(s for s in sections if s['kind'] == 17)
        member = struct.unpack_from('<I', original, group['offset'] + 4)[0]
        target = sections[member]
        cases = [('flags', group['offset'], 'I', 2),
                 ('member-zero', group['offset'] + 4, 'I', 0),
                 ('member-bound', group['offset'] + 4, 'I', len(sections)),
                 ('repeat', group['offset'] + 8, 'I', member),
                 ('signature', group['header'] + 44, 'I', 999999),
                 ('symbol-table', group['header'] + 40, 'I', 0),
                 ('entry-size', group['header'] + 56, 'Q', 8),
                 ('missing-flag', target['header'] + 8, 'Q', target['flags'] & ~0x200)]
        for name, offset, fmt, value in cases:
            with self.subTest(name=name):
                data = bytearray(original)
                struct.pack_into('<' + fmt, data, offset, value)
                path = self.root / ('bad-group-' + name + '.o')
                path.write_bytes(data)
                self.rejected(path, 'fas-ld:', self.simple)

    def test_merge_identical_sections_and_interior_pointers(self):
        objects = []
        for index in range(2):
            objects.append(self.assemble('merge-' + str(index),
                f'.section .rodata.str1.1,"aMS",@progbits,1\n.globl text{index}\ntext{index}: .asciz "hello"\n'
                f'.section .rodata.cst8,"aM",@progbits,8\n.balign 8\n.globl number{index}\nnumber{index}: .quad 42\n'
                f'.data\n.globl pointers{index}\npointers{index}: .quad text{index}+2\n.quad text{index}+6\n.quad number{index}'))
        start = self.assemble('merge-start', '.text\n.globl _start\n_start: '
            'mov pointers0(%rip),%rax\ncmpb $108,(%rax)\njne bad\n'
            'mov pointers0+8(%rip),%rax\nlea text0+6(%rip),%rdx\ncmp %rax,%rdx\njne bad\n'
            'mov pointers1+16(%rip),%rax\nmov (%rax),%edi\njmp done\n'
            'bad: mov $99,%edi\ndone: mov $60,%eax\nsyscall')
        self.compare('merge-execution', [start, *objects])
        out = self.link('merge-shape', start, *objects)
        symbols = {s['name']: s for s in core.elf_symbols(out.read_bytes())}
        self.assertEqual(symbols['text0']['value'], symbols['text1']['value'])
        self.assertEqual(symbols['number0']['value'], symbols['number1']['value'])
        strings = [s for s in core.elf_sections(out.read_bytes()) if s['name'] == '.rodata.str1.1']
        self.assertEqual(len(strings), 1)
        self.assertEqual(strings[0]['entsize'], 1)

    def test_merge_relocations_and_writable_contents_remain_distinct(self):
        first = self.assemble('merge-reloc-first', '.section .rodata.ptr,"aM",@progbits,8\n'
            '.globl ptr0\nptr0: .quad answer\n.section .data.merge,"awM",@progbits,8\n.globl mutable0\nmutable0: .quad 4')
        second = self.assemble('merge-reloc-second', '.section .rodata.ptr,"aM",@progbits,8\n'
            '.globl ptr1\nptr1: .quad _start\n.section .data.merge,"awM",@progbits,8\n.globl mutable1\nmutable1: .quad 4')
        out = self.link('merge-reloc', self.simple, self.answer, first, second)
        run(out, expected=42)
        symbols = {s['name']: s for s in core.elf_symbols(out.read_bytes())}
        self.assertNotEqual(symbols['ptr0']['value'], symbols['ptr1']['value'])
        self.assertNotEqual(symbols['mutable0']['value'], symbols['mutable1']['value'])

    def test_merge_metadata_validation(self):
        base = self.assemble('merge-invalid-base', '.section .rodata.str2,"aMS",@progbits,2\n.short 65,0')
        original = base.read_bytes()
        section = next(s for s in core.elf_sections(original) if s['name'] == '.rodata.str2')
        for name, offset, fmt, value in (
            ('zero-entry', section['header'] + 56, 'Q', 0),
            ('partial-entry', section['header'] + 56, 'Q', 3),
            ('strings-without-merge', section['header'] + 8, 'Q', 0x22),
            ('unterminated', section['offset'] + 2, 'H', 1)):
            data = bytearray(original)
            struct.pack_into('<' + fmt, data, offset, value)
            path = self.root / ('bad-merge-' + name + '.o')
            path.write_bytes(data)
            self.rejected(path, 'fas-ld:', self.simple, self.answer)


if __name__ == '__main__':
    unittest.main(verbosity=2)
