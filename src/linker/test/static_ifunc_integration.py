#!/usr/bin/env python3
"""Static IRELATIVE startup, canonical addresses and the installed glibc CRT."""
from pathlib import Path
import shutil
import struct
import subprocess
import sys
import unittest

DYNAMIC_REENTRY = str(Path(sys.argv.pop(3)).resolve())

import integration as core
import hosted_integration as hosted

run = core.run

START = r'''
.text
.globl _start
_start:
 lea __rela_iplt_start(%rip),%rbx
 lea __rela_iplt_end(%rip),%rbp
1: cmp %rbp,%rbx
 je 2f
 cmpl $37,8(%rbx)
 jne 3f
 call *16(%rbx)
 mov (%rbx),%rcx
 mov %rax,(%rcx)
 add $24,%rbx
 jmp 1b
2: call answer
 mov %eax,%edi
 mov $60,%eax
 syscall
3: mov $99,%edi
 mov $60,%eax
 syscall
.section .note.GNU-stack,"",@progbits
'''


class StaticIfuncTests(unittest.TestCase):
    setUpClass = classmethod(core.LinkerTests.setUpClass.__func__)
    tearDownClass = classmethod(core.LinkerTests.tearDownClass.__func__)
    assemble = classmethod(core.LinkerTests.assemble.__func__)
    link = core.LinkerTests.link
    rejected = core.LinkerTests.rejected
    crt = hosted.HostedTests.crt

    @staticmethod
    def tools():
        return [core.LINKER, 'ld'] + (['ld.lld-22'] if shutil.which('ld.lld-22') else [])

    def compile(self, name, source, *, compiler='gcc', flags=()):
        path = self.root/(name+'.c')
        path.write_text(source)
        obj = path.with_suffix('.o')
        run(compiler, '-c', '-O2', '-g', '-ffunction-sections', '-fdata-sections',
            *flags, path, '-o', obj)
        return obj

    def static(self, name, obj, *, linker=core.LINKER, gc=False):
        return self.link(name, self.crt('crt1.o'), self.crt('crti.o'),
                         self.crt('crtbeginT.o'), obj, '--start-group',
                         self.crt('libgcc.a'), self.crt('libgcc_eh.a'), self.crt('libc.a'),
                         '--end-group', self.crt('crtend.o'), self.crt('crtn.o'),
                         linker=linker, options=('-static', '--eh-frame-hdr',
                         *(('--gc-sections',) if gc else ())))

    def test_freestanding_startup_canonical_addresses_models_and_partial(self):
        source = r'''
static int calls;
static int implementation(void){return 21;}
static int (*resolve(void))(void){++calls;return implementation;}
int selected(void) __attribute__((ifunc("resolve")));
static int local(void) __attribute__((ifunc("resolve")));
static int (*volatile global_pointer)(void)=selected;
static int (*volatile local_pointer)(void)=local;
int answer(void){
 if(calls!=2 || global_pointer!=selected || local_pointer!=local)return 1;
 return global_pointer()+local()+selected()-local_pointer();
}
'''
        start = self.assemble('static-irel-start', START)
        for compiler in ('gcc', 'clang'):
            if not shutil.which(compiler):
                continue
            for flags in (('-fPIC',), ('-fno-pie',)):
                obj = self.compile('static-ifunc', source, compiler=compiler, flags=flags)
                for partial in (False, True):
                    objects = (start, obj)
                    if partial:
                        objects = (self.link('static-ifunc-partial.o', *objects, options=('-r',)),)
                    for gc in (False, True):
                        for tool in self.tools():
                            with self.subTest(compiler=compiler, flags=flags, partial=partial, gc=gc, tool=tool):
                                out = self.link('static-ifunc-program', *objects, linker=tool,
                                                options=(*(('--gc-sections',) if gc else ()),))
                                run(out, expected=42)
                                self.assertEqual(run('readelf', '-aW', out).stderr, b'')
                                data = out.read_bytes()
                                self.assertEqual(struct.unpack_from('<H', data, 16)[0], 2)
                                self.assertFalse(any(p['kind'] in (2, 3) for p in core.elf_segments(data)))
                                if tool == core.LINKER:
                                    rela = next(s for s in core.elf_sections(data) if s['name']=='.rela.iplt')
                                    self.assertEqual((rela['kind'], rela['size'], rela['entsize']), (4, 48, 24))
                                    for at in range(rela['offset'], rela['offset']+rela['size'], 24):
                                        target, info, resolver = struct.unpack_from('<QQQ', data, at)
                                        self.assertEqual(info, 37)
                                        self.assertTrue(any(p['flags'] & 2 and p['address'] <= target < p['address']+p['memory_size'] for p in core.elf_segments(data)))
                                        self.assertTrue(any(p['flags'] & 1 and p['address'] <= resolver < p['address']+p['memory_size'] for p in core.elf_segments(data)))

    def test_installed_static_glibc_threads_tls_ifunc_lifecycle_and_memory(self):
        source = r'''
#include <errno.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
static __thread long initialized __attribute__((aligned(8192)))=7;
static __thread long zero;
static int resolved;
static int implementation(void){return 42;}
static int (*resolve(void))(void){++resolved;return implementation;}
int selected(void) __attribute__((ifunc("resolve")));
static int (*volatile pointer)(void)=selected;
static void goodbye(void){puts("atexit");}
__attribute__((constructor(101))) static void init(void){puts("constructor");}
__attribute__((destructor)) static void fini(void){puts("destructor");}
static void *worker(void *unused){
 if(initialized!=7 || zero || (unsigned long)&initialized%8192)return (void*)1;
 initialized=99;zero=13;errno=ERANGE;return 0;
}
int main(int argc,char **argv){
 if(argc!=3 || strcmp(argv[1],"one") || strcmp(argv[2],"two") || !getenv("PATH"))return 1;
 if(resolved!=1 || pointer!=selected || selected()!=42)return 2;
 char *a=malloc(512),*b=malloc(512);if(!a || !b)return 3;
 memset(a,0,512);strcpy(a,"static glibc");memcpy(b,a,512);
 memmove(b+1,b,strlen(b)+1);if(strcmp(b+1,"static glibc"))return 4;
 free(a);free(b);
 pthread_t t;void *result;errno=0;
 if(pthread_create(&t,0,worker,0) || pthread_join(t,&result) || result)return 5;
 if(initialized!=7 || zero || errno)return 6;
 if(atexit(goodbye))return 7;puts("main");return 42;
}
'''
        for compiler in ('gcc', 'clang'):
            if not shutil.which(compiler):
                continue
            obj = self.compile('static-glibc', source, compiler=compiler,
                               flags=('-fno-pie', '-fno-builtin'))
            for gc in (False, True):
                for tool in self.tools():
                    with self.subTest(compiler=compiler, gc=gc, tool=tool):
                        out = self.static('static-glibc-program', obj, linker=tool, gc=gc)
                        self.assertEqual(run(out, 'one', 'two', expected=42).stdout,
                                         b'constructor\nmain\natexit\ndestructor\n')
                        self.assertEqual(run('readelf', '-aW', out).stderr, b'')

    def test_empty_irel_boundaries_and_bad_entry(self):
        start = self.assemble('static-empty-irel', START)
        out = self.link('static-empty-program', start, self.answer)
        run(out, expected=42)
        symbols = {s['name']:s for s in core.elf_symbols(out.read_bytes())}
        self.assertEqual(symbols['__rela_iplt_start']['value'], 0)
        self.assertEqual(symbols['__rela_iplt_end']['value'], 0)
        entry = self.assemble('static-ifunc-entry', '.text\n.globl _start\n.type _start,@gnu_indirect_function\n_start: ret')
        self.rejected(entry, 'cannot be an IFUNC')

    def test_unreferenced_ifunc_resolver_is_not_executed(self):
        start = self.assemble('static-unused-start', START)
        obj = self.compile('static-unused-ifunc', '''
static int calls;
static int implementation(void){return 91;}
static void *resolve(void){++calls;return implementation;}
int unused(void) __attribute__((ifunc("resolve")));
int answer(void){return calls ? 1 : 42;}
''', flags=('-fPIC',))
        debug = self.assemble('static-unused-debug',
                              '.section .debug_ifunc,"",@progbits\n.quad unused')
        for metadata in ((), (debug,)):
            for gc in (False, True):
                for tool in self.tools():
                    with self.subTest(metadata=bool(metadata), gc=gc, tool=tool):
                        out = self.link('static-unused-program', start, obj, *metadata, linker=tool,
                                        options=('--gc-sections',) if gc else ())
                        run(out, expected=42)

    def test_repeat_failure_and_concurrent_invocations(self):
        start = self.assemble('static-reentry-start', START)
        obj = self.compile('static-reentry', '''
static int implementation(void){return 42;}
static void *resolve(void){return implementation;}
int selected(void) __attribute__((ifunc("resolve")));
int answer(void){return selected();}
''', flags=('-fPIC',))
        result = run(DYNAMIC_REENTRY, '--gc-sections', start, obj)
        self.assertIn(b'domain reentry', result.stdout)

    def test_real_static_gcc_and_clang_driver(self):
        directory=self.root/'static-driver'
        directory.mkdir()
        (directory/'ld').symlink_to(core.LINKER)
        source=directory/'main.c'
        source.write_text('#include <stdio.h>\n#include <string.h>\nint main(int argc,char **argv){puts("static-driver");return argc==2 && !strcmp(argv[1],"checked") ? 42:1;}')
        for compiler in ('gcc','clang'):
            if not shutil.which(compiler):
                continue
            output=directory/('program-'+compiler)
            flags=('-fno-use-linker-plugin',) if compiler=='gcc' else ()
            run(compiler,'-static','-O2','-fno-builtin',*flags,'-B'+str(directory)+'/',
                source,'-o',output)
            self.assertEqual(run(output,'checked',expected=42).stdout,b'static-driver\n')


if __name__ == '__main__':
    unittest.main(verbosity=2)
