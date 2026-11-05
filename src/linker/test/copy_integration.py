#!/usr/bin/env python3
"""Executable data copies, independent loaders/consumers and bounded failures."""
import os
from pathlib import Path
import shutil
import struct
import subprocess
import sys
import unittest

import hosted_integration as fixtures

core = fixtures.core
run = core.run
DYNAMIC_REENTRY = str(Path(sys.argv.pop(1)).resolve())


def copies(path):
    data = path.read_bytes()
    sections = core.elf_sections(data)
    symbols = core.elf_symbols(data, kind=11)
    result = []
    for section in sections:
        if section['kind'] == 4:
            for offset in range(section['offset'], section['offset']+section['size'], 24):
                address, info, addend = struct.unpack_from('<QQq', data, offset)
                if info & 0xffffffff == 5:
                    result.append((symbols[info >> 32], address, addend))
    return result


class CopyTests(unittest.TestCase):
    setUpClass = classmethod(core.LinkerTests.setUpClass.__func__)
    tearDownClass = classmethod(core.LinkerTests.tearDownClass.__func__)
    assemble = classmethod(core.LinkerTests.assemble.__func__)
    link = core.LinkerTests.link
    rejected = core.LinkerTests.rejected
    compile = fixtures.HostedTests.compile
    crt = fixtures.HostedTests.crt
    hosted = fixtures.HostedTests.hosted
    setUp = fixtures.HostedTests.setUp

    @staticmethod
    def tools():
        return [core.LINKER, 'ld'] + ([shutil.which('ld.lld-22') or shutil.which('ld.lld')]
                                     if shutil.which('ld.lld-22') or shutil.which('ld.lld') else [])

    def library(self, name, source, *, tool='ld', flags=()):
        obj = self.compile(name, source)
        out = self.root/(name+'.so')
        run(tool, '-shared', '-soname', out.name, *flags, '-o', out, obj)
        return out

    def execute(self, path, expected=42, preload=None):
        env = {k: v for k, v in os.environ.items() if not k.startswith('LD_')}
        env['LD_LIBRARY_PATH'] = str(self.root)
        if preload:
            env['LD_PRELOAD'] = str(preload)
        result = subprocess.run([str(path)], cwd='/', env=env, capture_output=True, timeout=10)
        self.assertEqual(result.returncode, expected, result.stderr)
        return result

    def test_gcc_clang_data_alignment_pointer_identity_and_read_only_storage(self):
        source = r'''
int copy_value=11;
unsigned char copy_bytes[37]={1,2,3,[36]=77};
__attribute__((aligned(8192))) long copy_aligned=19;
int copy_bss;
const long copy_constants[3]={17,23,41};
int *const copy_pointer=&copy_value;
int *copy_address(void){return &copy_value;}
int copy_read(void){return copy_value;}
int *copy_bss_address(void){return &copy_bss;}
const long *copy_constant_address(void){return copy_constants;}
int **copy_pointer_address(void){return (int**)&copy_pointer;}
'''
        consumer = r'''
extern int copy_value,copy_bss;
extern unsigned char copy_bytes[37];
extern long copy_aligned;
extern const long copy_constants[3];
extern int *const copy_pointer;
extern int *copy_address(void),*copy_bss_address(void),copy_read(void);
extern const long *copy_constant_address(void);
extern int **copy_pointer_address(void);
int *copy_saved=&copy_value;
unsigned char *copy_interior=&copy_bytes[36];
int main(void){
 if(copy_value!=11 || copy_bss || copy_aligned!=19) return 1;
 if(copy_bytes[0]!=1 || copy_bytes[1]!=2 || copy_bytes[36]!=77) return 2;
 if(copy_constants[0]!=17 || copy_constants[2]!=41) return 3;
 if((unsigned long)&copy_aligned%8192) return 4;
 if(copy_address()!=&copy_value || copy_bss_address()!=&copy_bss) return 5;
 if(copy_constant_address()!=copy_constants || copy_pointer_address()!=&copy_pointer) return 6;
 if(copy_saved!=&copy_value || copy_interior!=&copy_bytes[36] || copy_pointer!=&copy_value) return 7;
 copy_value=42; copy_bss=7;
 if(copy_read()!=42 || *copy_bss_address()!=7 || *copy_pointer!=42) return 8;
 return 42;
}
'''
        compilers = ['gcc'] + (['clang'] if shutil.which('clang') else [])
        for producer in self.tools():
            library = self.library('copy-provider', source, tool=producer)
            for compiler in compilers:
                for pie in (False, True):
                    obj = self.compile('copy-consumer', consumer, compiler,
                                       flags=('-fPIE',) if pie else ('-fno-pic','-fno-pie'))
                    for tool in self.tools():
                        with self.subTest(producer=producer, compiler=compiler, pie=pie, tool=tool):
                            out = self.hosted('copy-program', obj, library, pie=pie,
                                              linker=tool, gc=True)
                            self.execute(out)
                            records = copies(out)
                            # Clang PIE uses the GOT for externally declared
                            # data, so it may need no COPY at all.
                            if compiler == 'gcc' or not pie:
                                self.assertEqual({s['name'] for s,_,_ in records},
                                                 {'copy_value','copy_bytes','copy_aligned','copy_bss',
                                                  'copy_constants','copy_pointer'})
                            self.assertTrue(all(s['value']==address and addend==0
                                                for s,address,addend in records))
                            self.assertEqual(run('readelf', '-aW', out).stderr, b'')
                            if tool == core.LINKER:
                                self.assertNotIn(b'TEXTREL', run('readelf', '-dW', out).stdout)

    def test_gc_archives_partial_links_and_deterministic_reentry(self):
        library = self.library('copy-lifetime', 'int copy_live=42; int copy_dead=19;')
        obj = self.compile('copy-lifetime-main', '''
extern int copy_live,copy_dead;
int dead(void){return copy_dead;}
int main(void){return copy_live;}
''', flags=('-fPIE',))
        archive = self.root/'copy-main.a'
        run('ar', 'rcs', archive, obj)
        for input_path in (obj, archive):
            out = self.hosted('copy-gc', input_path, library, gc=True)
            self.execute(out)
            self.assertEqual([s['name'] for s,_,_ in copies(out)], ['copy_live'])
        for tool in self.tools():
            partial = self.link('copy-partial.o', obj, linker=tool, options=('-r',))
            out = self.hosted('copy-partial-program', partial, library, gc=True)
            self.execute(out)
            self.assertEqual([s['name'] for s,_,_ in copies(out)], ['copy_live'])
        flags = ('-pie', '--dynamic-linker', fixtures.INTERPRETER, '--gc-sections')
        run(DYNAMIC_REENTRY, *flags, self.crt('Scrt1.o'), self.crt('crti.o'),
            self.crt('crtbeginS.o'), obj, library, fixtures.LIBC,
            self.crt('crtendS.o'), self.crt('crtn.o'))

    def test_versions_and_preloaded_provider(self):
        policy = self.root/'copy-versions.map'
        policy.write_text('V1 { global: copy_versioned; local: *; }; V2 { global: copy_versioned; } V1;')
        library = self.library('copy-versioned', '''
int old_data=17; int new_data=25;
__asm__(".symver old_data,copy_versioned@V1\\n.symver new_data,copy_versioned@@V2");
''', flags=('--version-script', policy))
        obj = self.compile('copy-version-main', '''
extern int old_data,copy_versioned;
__asm__(".symver old_data,copy_versioned@V1");
int main(void){return old_data+copy_versioned;}
''', flags=('-fPIE',))
        for tool in self.tools():
            # BFD 2.46 and LLD 22 coalesce these versioned executable copies
            # and return 34. Fas retains distinct storage. Compare each
            # version independently with every consumer below.
            if tool == core.LINKER:
                out = self.hosted('copy-version-program', obj, library, linker=tool)
                self.execute(out)
                self.assertEqual(len(copies(out)), 2)
                self.assertEqual(run('readelf', '-aW', out).stderr, b'')
            for old, expected in ((False,25), (True,17)):
                declaration = ('extern int value; __asm__(".symver value,copy_versioned@V1");'
                               if old else 'extern int copy_versioned;')
                reference = 'value' if old else 'copy_versioned'
                one = self.compile('copy-one-version',
                                   declaration + f'\nint main(void){{return {reference};}}\n',
                                   flags=('-fPIE',))
                out = self.hosted('copy-one-version-program', one, library, linker=tool)
                self.execute(out, expected=expected)
        library = self.library('copy-preempt', 'int copy_override=19; int *copy_address(void){return &copy_override;}')
        replacement = self.library('copy-replacement', 'int copy_override=42;')
        obj = self.compile('copy-preempt-main', '''
extern int copy_override; extern int *copy_address(void);
int main(void){return &copy_override==copy_address() ? copy_override : 1;}
''', flags=('-fPIE',))
        for tool in self.tools():
            out = self.hosted('copy-preempt-program', obj, library, linker=tool)
            self.execute(out, expected=19)
            self.execute(out, preload=replacement)

    def test_protected_alias_and_invalid_storage_are_explicit_failures(self):
        obj = self.compile('copy-reject-main', 'extern int copy_bad; int main(void){return copy_bad;}',
                           flags=('-fPIE',))
        for name, source, diagnostic in (
            ('protected', '__attribute__((visibility("protected"))) int copy_bad=42;', 'protected data'),
            ('alias', 'int copy_bad=42; extern int copy_alias __attribute__((alias("copy_bad")));', 'overlapping exported data'),
        ):
            library = self.library('copy-bad-'+name, source)
            self.rejected(self.crt('Scrt1.o'), diagnostic, self.crt('crti.o'), obj,
                          library, fixtures.LIBC, self.crt('crtn.o'), options=('-pie',))
        library = self.library('copy-bad-metadata', 'int copy_bad=42;')
        original = library.read_bytes()
        symbol = next(s for s in core.elf_symbols(original, kind=11) if s['name']=='copy_bad')
        symbol_at = symbol['offset']
        section = core.elf_sections(original)[symbol['section']]
        for offset, value, diagnostic in (
            (symbol_at+16, 0, 'zero-sized or out-of-bounds'),
            (symbol_at+16, 1<<63, 'zero-sized or out-of-bounds'),
            (symbol_at+8, section['address']-1, 'zero-sized or out-of-bounds'),
            (section['header']+48, 3, 'storage alignment'),
            (section['header']+48, 1<<63, 'storage alignment'),
        ):
            with self.subTest(offset=offset, value=value):
                changed = bytearray(original)
                struct.pack_into('<Q', changed, offset, value)
                library.write_bytes(changed)
                self.rejected(self.crt('Scrt1.o'), diagnostic, self.crt('crti.o'), obj,
                              library, fixtures.LIBC, self.crt('crtn.o'), options=('-pie',))
        library.write_bytes(original)
        # Ordinary GOT references still work for aliased storage: only creating
        # independent executable copies would lose the provider's identity.
        library = self.library('copy-pic-alias',
            'int copy_bad=42; extern int copy_alias __attribute__((alias("copy_bad")));')
        pic = self.compile('copy-pic-alias-main',
            'extern int copy_bad,copy_alias; int main(void){return &copy_bad==&copy_alias ? copy_bad : 1;}')
        out = self.hosted('copy-pic-alias-program', pic, library)
        self.execute(out)
        self.assertEqual(copies(out), [])

        policy = self.root/'copy-default.map'
        policy.write_text('CURRENT { global: copy_bad; };')
        library = self.library('copy-default', 'int copy_bad=42;',
                               flags=('--version-script', policy))
        mixed = self.compile('copy-mixed-version-main', r'''
extern int copy_bad,explicit_data;
__asm__(".symver explicit_data,copy_bad@CURRENT");
int main(void){return copy_bad+explicit_data;}
''', flags=('-fPIE',))
        self.rejected(self.crt('Scrt1.o'), 'mixed version spellings', self.crt('crti.o'),
                      mixed, library, fixtures.LIBC, self.crt('crtn.o'), options=('-pie',))

    def test_shared_output_still_requires_pic_and_dead_protected_reference_is_ignored(self):
        library = self.library('copy-policy', '__attribute__((visibility("protected"))) int copy_unused=1; int copy_live=42;')
        obj = self.compile('copy-policy-main', '''
extern int copy_unused,copy_live;
int dead(void){return copy_unused;}
int main(void){return copy_live;}
''', flags=('-fPIE',))
        self.rejected(obj, 'direct import relocation', library, options=('-shared',))
        out = self.hosted('copy-policy-gc', obj, library, gc=True)
        self.execute(out)
        self.assertEqual([s['name'] for s,_,_ in copies(out)], ['copy_live'])

    def test_read_only_copies_become_read_only_after_loader_startup(self):
        library = self.library('copy-readonly', 'const int copy_readonly=42;')
        obj = self.compile('copy-readonly-main', r'''
#include <signal.h>
#include <sys/wait.h>
#include <unistd.h>
extern const int copy_readonly;
int main(void){
 if(copy_readonly!=42) return 1;
 int pid=fork(); if(pid<0) return 2;
 if(pid==0){*(volatile int*)&copy_readonly=7; _exit(3);}
 int status; if(waitpid(pid,&status,0)!=pid) return 4;
 return WIFSIGNALED(status) && WTERMSIG(status)==SIGSEGV ? 42 : 5;
}
''', flags=('-fPIE',))
        for tool in self.tools():
            out = self.hosted('copy-readonly-program', obj, library, linker=tool)
            self.execute(out)

    def test_wide_pc_relative_and_fixed_read_only_addresses(self):
        library = self.library('copy-wide', 'int copy_wide=42;')
        for absolute in (False, True):
            # Absolute pointers in read-only storage are only permitted in a
            # fixed executable; a PIE would require a text relocation.
            expression = 'copy_wide' if absolute else 'copy_wide-.'
            load = ('mov (%rax),%rax' if absolute else
                    'mov (%rax),%rdx; add %rdx,%rax')
            obj = self.assemble('copy-wide-main', f'''
.text
.globl main
.type main,@function
main: lea address(%rip),%rax; {load}; mov (%rax),%eax; ret
.section .rodata,"a",@progbits
.p2align 3
address: .quad {expression}
.section .note.GNU-stack,"",@progbits
''')
            for pie in ((False,) if absolute else (False, True)):
                for tool in self.tools():
                    out = self.hosted('copy-wide-program', obj, library,
                                      linker=tool, pie=pie)
                    self.execute(out)
                    self.assertEqual([s['name'] for s,_,_ in copies(out)], ['copy_wide'])
                    self.assertNotIn('TEXTREL', run('readelf', '-dW', out).stdout.decode())


if __name__ == '__main__':
    unittest.main(verbosity=2)
