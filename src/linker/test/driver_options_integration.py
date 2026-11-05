#!/usr/bin/env python3
"""Driver dependency policies, state stack, library selection and emulation."""
import unittest

import shared_integration as fixtures

core = fixtures.core
run = core.run


class DriverOptionsTests(unittest.TestCase):
    setUpClass = fixtures.SharedTests.__dict__['setUpClass']
    tearDownClass = fixtures.SharedTests.__dict__['tearDownClass']
    assemble = fixtures.SharedTests.__dict__['assemble']
    setUp = fixtures.SharedTests.setUp
    shared = fixtures.SharedTests.shared
    options = fixtures.SharedTests.options
    link = core.LinkerTests.link
    rejected = core.LinkerTests.rejected

    def test_as_needed_weak_and_position_sensitive_dependencies(self):
        needed = self.shared('needed', 'int answer(void){return 42;}')
        unused = self.shared('unused', 'int optional(void){return 99;}')
        start = self.assemble('as-needed-start', r'''
.text
.globl _start
.weak optional
_start:
 mov optional@GOTPCREL(%rip),%rax
 test %rax,%rax
 jne 1f
 call answer
 mov %eax,%edi
 jmp 2f
1: mov $99,%edi
2: mov $60,%eax
 syscall
''')
        for tool in (core.LINKER, 'ld'):
            out = self.link('needed', start, needed, unused, linker=tool,
                            options=(*self.options(), '--as-needed'))
            run(out, expected=42)
            report = run('readelf', '-dW', out).stdout
            self.assertIn(needed.name.encode(), report)
            self.assertNotIn(unused.name.encode(), report)
            # Positional inputs need an explicit invocation to interleave flags.
            out = self.root/'needed-state'
            run(tool, *self.options(), '-o', out, start, '--as-needed', needed,
                '--push-state', '--no-as-needed', unused, '--pop-state')
            run(out, expected=99)
            out = self.root/'needed-too-early'
            run(tool, *self.options(), '--as-needed', '-o', out, needed, start, expected=1)
            # A group rescans dependencies after the regular object's references.
            run(tool, *self.options(), '--as-needed', '-o', out,
                '--start-group', needed, start, '--end-group')
            run(out, expected=42)

    def test_static_dynamic_library_search_and_state_restore(self):
        library = self.shared('choice', 'int answer(void){return 35;}')
        obj = self.assemble('choice-static', '.text\n.globl answer\nanswer: mov $42,%eax; ret')
        archive = self.root/'libchoice.a'
        run('ar', 'rcs', archive, obj)
        for tool in (core.LINKER, 'ld'):
            for flags, expected in ((('-Bstatic',),42), (('-Bdynamic',),35),
                                    (('-Bstatic','--push-state','-Bdynamic','--pop-state'),42)):
                out = self.root/'search-mode'
                run(tool, *self.options(), '-L', self.root, '-o', out,
                    self.simple, *flags, '-lchoice')
                run(out, expected=expected)
                report = run('readelf', '-dW', out).stdout
                self.assertEqual(library.name.encode() in report, expected == 35)
            out = self.root/'static-mode'
            run(tool, '-static', '-m', 'elf_x86_64', '--eh-frame-hdr',
                '-L', self.root, '-o', out, self.simple, '-lchoice')
            run(out, expected=42)
            self.assertNotIn(b'INTERP', run('readelf', '-lW', out).stdout)

    def test_bad_state_emulation_and_unsupported_flags_preserve_output(self):
        for flags, diagnostic in ((('--pop-state',), 'without --push-state'),
                                  (('--push-state',), 'missing --pop-state'),
                                  (('-m','elf_i386'), 'unsupported emulation'),
                                  (('--push-state',)*65, 'nesting exceeds'),
                                  (('--plugin','missing.so'), 'unsupported option')):
            with self.subTest(flags=flags):
                self.rejected(self.simple, diagnostic, self.answer, options=flags)

    def test_shared_driver_interpreter_is_ignored(self):
        for tool in (core.LINKER, 'ld'):
            out = self.link('shared-with-interpreter', self.answer, linker=tool,
                            options=('-shared', '--dynamic-linker', '/unused/driver/loader'))
            self.assertNotIn(b'INTERP', run('readelf', '-lW', out).stdout)
            self.assertNotIn(b'/unused/driver/loader', out.read_bytes())

    def test_explicit_multiple_definitions_keep_first_and_preserve_partial_semantics(self):
        first = self.assemble('muldefs-first', '.text\n.globl answer\nanswer: mov $42,%eax; ret')
        second = self.assemble('muldefs-second', '.text\n.globl answer\nanswer: mov $19,%eax; ret')
        weak = self.assemble('muldefs-weak', '.text\n.weak answer\nanswer: mov $7,%eax; ret')
        data_start = self.assemble('muldefs-data-start', '.text\n.globl _start\n_start: mov duplicate_data(%rip),%edi; mov $60,%eax; syscall')
        data_first = self.assemble('muldefs-data-first', '.data\n.globl duplicate_data\n.type duplicate_data,@object\n.size duplicate_data,4\nduplicate_data: .long 42')
        data_second = self.assemble('muldefs-data-second', '.data\n.globl duplicate_data\n.type duplicate_data,@object\n.size duplicate_data,4\nduplicate_data: .long 19')
        for flag in (('--allow-multiple-definition',), ('-z','muldefs')):
            for tool in (core.LINKER, 'ld'):
                for a,b,expected in ((first,second,42), (second,first,19),
                                     (weak,first,42), (first,weak,42)):
                    for gc in ((), ('--gc-sections',)):
                        out = self.link('muldefs', self.simple, a, b, linker=tool,
                                        options=(*flag,*gc))
                        run(out, expected=expected)
                    partial = self.link('muldefs.o', a, b, linker=tool, options=('-r',*flag))
                    for consumer in (core.LINKER, 'ld'):
                        out = self.link('muldefs-partial', self.simple, partial, linker=consumer)
                        run(out, expected=expected)
                out = self.link('muldefs-data', data_start, data_first, data_second,
                                linker=tool, options=flag)
                run(out, expected=42)
            self.rejected(self.simple, 'duplicate symbol answer', first, second)


if __name__ == '__main__':
    unittest.main(verbosity=2)
