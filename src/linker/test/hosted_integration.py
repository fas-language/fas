#!/usr/bin/env python3
"""Pinned system CRT/libc: real startup, ordered initialization and shutdown."""
import os
from pathlib import Path
import shutil
import subprocess
import unittest

import integration as core

run = core.run
LINKER = core.LINKER
INTERPRETER = '/lib64/ld-linux-x86-64.so.2'
LIBC = Path('/lib/x86_64-linux-gnu/libc.so.6')


class HostedTests(unittest.TestCase):
    setUpClass = classmethod(core.LinkerTests.setUpClass.__func__)
    tearDownClass = classmethod(core.LinkerTests.tearDownClass.__func__)
    assemble = classmethod(core.LinkerTests.assemble.__func__)
    link = core.LinkerTests.link
    rejected = core.LinkerTests.rejected

    def setUp(self):
        if not LIBC.is_file() or not Path(INTERPRETER).is_file():
            self.skipTest('pinned glibc x86-64 sysroot unavailable; hosted gate unverified')

    def crt(self, name):
        result = run('gcc', '-print-file-name='+name).stdout.decode().strip()
        if not Path(result).is_file():
            self.skipTest('system CRT unavailable: '+name)
        return result

    def compile(self, name, source, compiler='gcc', flags=()):
        path = self.root / (name+'.c')
        path.write_text(source)
        obj = path.with_suffix('.o')
        run(compiler, '-c', '-O2', '-g', '-fPIC', '-ffunction-sections',
            '-fdata-sections', *flags, path, '-o', obj)
        return obj

    def hosted(self, name, *objects, pie=True, gc=False, linker=LINKER, extra=()):
        options = ('--dynamic-linker', INTERPRETER) + (('-pie',) if pie else ())
        if gc:
            options += ('--gc-sections',)
        return self.link(name, self.crt('Scrt1.o' if pie else 'crt1.o'),
                         self.crt('crti.o'), self.crt('crtbeginS.o'), *objects,
                         LIBC, self.crt('libc_nonshared.a'),
                         self.crt('crtendS.o'), self.crt('crtn.o'),
                         options=options+extra, linker=linker)

    def test_crt_main_arguments_environment_stdio_and_shutdown(self):
        source = r'''
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
static void goodbye(void) { puts("atexit"); }
__attribute__((constructor)) static void initialize(void) { puts("constructor"); }
__attribute__((destructor)) static void finalize(void) { puts("destructor"); }
int main(int argc, char **argv) {
  if (argc != 3 || strcmp(argv[1], "one") || strcmp(argv[2], "two")) return 1;
  if (!getenv("PATH")) return 2;
  if (atexit(goodbye)) return 3;
  puts("main");
  return 42;
}
'''
        for compiler in ('gcc', 'clang'):
            if not shutil.which(compiler):
                continue
            obj = self.compile('hosted-main-'+compiler, source, compiler)
            for pie in (False, True):
                for gc in (False, True):
                    for linker in (LINKER, 'ld'):
                        with self.subTest(compiler=compiler, pie=pie, gc=gc, linker=linker):
                            out = self.hosted('hosted-main', obj, pie=pie, gc=gc, linker=linker)
                            self.assertEqual(run(out, 'one', 'two', expected=42).stdout,
                                             b'constructor\nmain\natexit\ndestructor\n')
                            self.assertEqual(run('readelf', '-aW', out).stderr, b'')

    def test_nonpie_gcc_crt_and_narrow_absent_weak_references(self):
        obj = self.compile('nonpie-crt', r'''
#include <stdio.h>
static int initialized;
__attribute__((constructor)) static void init(void){initialized=42;}
int main(void){puts("nonpie-crt");return initialized;}
''', flags=('-fno-pie',))
        for tool in (LINKER, 'ld'):
            out = self.link('nonpie-crt', self.crt('crt1.o'), self.crt('crti.o'),
                            self.crt('crtbegin.o'), obj,
                            '/usr/lib/x86_64-linux-gnu/libc.so', self.crt('crtend.o'),
                            self.crt('crtn.o'), options=('--dynamic-linker', INTERPRETER),
                            linker=tool)
            self.assertEqual(run(out, expected=42).stdout, b'nonpie-crt\n')

    def test_numeric_priorities_stable_ties_preinit_and_reverse_fini(self):
        early = self.compile('priority-early', r'''
#include <unistd.h>
static void pre(void){write(1,"P",1);}
__attribute__((section(".preinit_array"),used)) static void (*const p)(void)=pre;
__attribute__((constructor(101))) static void a(void){write(1,"A",1);}
__attribute__((destructor(101))) static void z(void){write(1,"Z",1);}
__attribute__((constructor(103))) static void tie(void){write(1,"1",1);}
''')
        late = self.compile('priority-late', r'''
#include <unistd.h>
__attribute__((constructor(102))) static void b(void){write(1,"B",1);}
__attribute__((constructor(103))) static void tie(void){write(1,"2",1);}
__attribute__((constructor)) static void c(void){write(1,"C",1);}
__attribute__((destructor(102))) static void y(void){write(1,"Y",1);}
__attribute__((destructor)) static void x(void){write(1,"X",1);}
int main(void){write(1,"M",1);return 42;}
''')
        for objects, expected in (((late,early),b'PAB21CMXYZ'),
                                  ((early,late),b'PAB12CMXYZ')):
            for linker in (LINKER, 'ld'):
                out = self.hosted('priorities', *objects, gc=True, linker=linker)
                self.assertEqual(run(out, expected=42).stdout, expected)
                sections = core.elf_sections(out.read_bytes())
                for kind in (14,15,16):
                    self.assertEqual(sum(s['kind'] == kind for s in sections), 1)

    def test_init_fini_fragment_alignment_executes_nop_padding(self):
        prologue = self.assemble('init-prologue', '''
.section .init,"ax",@progbits
.globl _init
_init: sub $8,%rsp
.section .fini,"ax",@progbits
.globl _fini
_fini: sub $8,%rsp''')
        middle = self.assemble('init-middle', '''
.section .init,"ax",@progbits
.p2align 4
nop
.section .fini,"ax",@progbits
.p2align 4
nop''')
        end = self.assemble('init-end', '''
.section .init,"ax",@progbits
add $8,%rsp; ret
.section .fini,"ax",@progbits
add $8,%rsp; ret''')
        obj = self.compile('init-fragments-main', 'int main(void){return 42;}')
        out = self.link('init-fragments', self.crt('Scrt1.o'), prologue, middle, obj,
                        LIBC, end, options=('-pie','--gc-sections'))
        run(out, expected=42)


if __name__ == '__main__':
    unittest.main(verbosity=2)
