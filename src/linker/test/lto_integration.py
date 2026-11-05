#!/usr/bin/env python3
"""Explicit LTO rejection with real compiler objects and preserved output."""
import shutil
import unittest

import integration as core

run = core.run


class LtoTests(unittest.TestCase):
    setUpClass = classmethod(core.LinkerTests.setUpClass.__func__)
    tearDownClass = classmethod(core.LinkerTests.tearDownClass.__func__)
    assemble = classmethod(core.LinkerTests.assemble.__func__)
    link = core.LinkerTests.link
    rejected = core.LinkerTests.rejected

    def test_real_gcc_slim_fat_and_clang_full_thin_objects(self):
        source = self.root/'lto.c'
        source.write_text('int answer(void){return 42;}')
        obj = self.root/'lto.o'
        variants = [('gcc', ('-flto', '-fno-fat-lto-objects')),
                    ('gcc', ('-flto', '-ffat-lto-objects'))]
        for compiler in ('clang', 'clang-22'):
            if shutil.which(compiler):
                variants += [(compiler, ('-flto=full',)), (compiler, ('-flto=thin',))]
        for compiler, flags in variants:
            with self.subTest(compiler=compiler, flags=flags):
                run(compiler, '-c', '-O2', *flags, source, '-o', obj)
                self.rejected(obj, 'LTO', self.simple)
                self.rejected(obj, 'LTO', options=('-r',))
        run('gcc', '-c', '-O2', source, '-o', obj)
        run(self.link('native-control', self.simple, obj), expected=42)

    def test_bitcode_wrapper_and_lazy_archive_diagnostics(self):
        wrapped = self.root/'wrapped-bitcode.o'
        wrapped.write_bytes(b'\xde\xc0\x17\x0b' + b'\0'*60)
        self.rejected(wrapped, 'LLVM bitcode LTO')
        source = self.root/'fat-lto.c'
        source.write_text('int lto_only(void){return 42;}')
        obj = source.with_suffix('.o')
        run('gcc', '-c', '-O2', '-flto', '-ffat-lto-objects', source, '-o', obj)
        archive = self.root/'libfat-lto.a'
        run('ar', 'rcs', archive, obj)
        # Unselected payloads retain the normal lazy archive behavior.
        out = self.link('unused-lto', self.simple, self.answer, archive)
        run(out, expected=42)
        self.rejected(archive, 'LTO', self.simple, self.answer,
                      options=('--whole-archive',))
        start = self.assemble('lto-start', '.text\n.globl _start\n_start: call lto_only; mov %eax,%edi; mov $60,%eax; syscall')
        self.rejected(start, 'LTO', archive)


if __name__ == '__main__':
    unittest.main(verbosity=2)
