#!/usr/bin/env python3
"""CRT-owned static PIE: independent startup, ASLR, TLS, IFUNC and diagnostics."""
import os
from pathlib import Path
import shutil
import struct
import subprocess
import sys
import unittest

import hosted_integration as hosted

core = hosted.core
run = core.run
REENTRY = str(Path(sys.argv.pop(1)).resolve())


class StaticPieTests(unittest.TestCase):
    setUpClass = hosted.HostedTests.__dict__['setUpClass']
    tearDownClass = hosted.HostedTests.__dict__['tearDownClass']
    assemble = hosted.HostedTests.__dict__['assemble']
    link = core.LinkerTests.link
    rejected = core.LinkerTests.rejected
    compile = hosted.HostedTests.compile
    crt = hosted.HostedTests.crt

    def tools(self):
        lld = shutil.which('ld.lld-22') or shutil.which('ld.lld')
        return [core.LINKER, 'ld'] + ([lld] if lld else [])

    def arguments(self, *objects, musl=None, gc=True):
        target = Path(musl)/'lib' if musl else None
        crt = lambda name: target/name if target and name in ('rcrt1.o', 'crti.o', 'crtn.o') else self.crt(name)
        return (crt('rcrt1.o'), crt('crti.o'), crt('crtbeginS.o'), *objects,
                '--start-group', target/'libc.a' if target else self.crt('libc.a'),
                self.crt('libgcc.a'), self.crt('libgcc_eh.a'), '--end-group',
                crt('crtendS.o'), crt('crtn.o'))

    def static_pie(self, name, *objects, linker=core.LINKER, gc=True, musl=None, extra=()):
        options = ('-static', '-pie', '--no-dynamic-linker', '-z', 'text', *extra)
        if gc:
            options += ('--gc-sections',)
        out = self.link(name, *self.arguments(*objects, musl=musl), options=options, linker=linker)
        self.inspect(out)
        return out

    def inspect(self, path):
        data = path.read_bytes()
        self.assertEqual(struct.unpack_from('<H', data, 16)[0], 3)
        segments = core.elf_segments(data)
        self.assertNotIn(3, [s['kind'] for s in segments], 'PT_INTERP in static PIE')
        self.assertIn(2, [s['kind'] for s in segments])
        sections = core.elf_sections(data)
        self.assertNotIn('.interp', [s['name'] for s in sections])
        dynamic = next(s for s in sections if s['kind'] == 6)
        tags = dict(struct.unpack_from('<qQ', data, at)
                    for at in range(dynamic['offset'], dynamic['offset']+dynamic['size'], 16))
        self.assertNotIn(1, tags, 'DT_NEEDED in static PIE')
        self.assertNotIn(22, tags, 'DT_TEXTREL in static PIE')
        self.assertEqual(run('readelf', '-aW', path).stderr, b'')

    def test_glibc_startup_aslr_relative_pointers_and_lifecycle(self):
        source = r'''
#include <stdio.h>
#include <stdlib.h>
static int value=42; static int *pointer=&value;
static void goodbye(void){puts("atexit");}
__attribute__((constructor(101))) static void init(void){puts("init");}
__attribute__((destructor)) static void fini(void){puts("fini");}
int main(void){if(atexit(goodbye))return 1;printf("%p\n",(void*)pointer);return *pointer;}
'''
        for compiler in ('gcc', 'clang'):
            if not shutil.which(compiler):
                continue
            obj = self.compile('static-pie-main-'+compiler, source, compiler, flags=('-fPIE',))
            for tool in self.tools():
                for gc in (False, True):
                    with self.subTest(compiler=compiler, tool=tool, gc=gc):
                        out = self.static_pie('static-pie-main', obj, linker=tool, gc=gc)
                        addresses = set()
                        for _ in range(4):
                            lines = run(out, expected=42).stdout.splitlines()
                            self.assertEqual((lines[0], lines[2:]), (b'init', [b'atexit', b'fini']))
                            addresses.add(int(lines[1], 16))
                        if Path('/proc/sys/kernel/randomize_va_space').read_text().strip() != '0':
                            self.assertGreater(len(addresses), 1)

    def test_glibc_threaded_aligned_tls_and_ifunc_relocation_order(self):
        source = r'''
#include <pthread.h>
#include <stdio.h>
#include <string.h>
__thread long own __attribute__((aligned(256)))=42;
__thread long zero[17];
static int data=42; static int *pointer=&data; static int resolved;
static int implementation(void){return *pointer;}
static void *choose(void){++resolved;return *pointer==42 ? implementation : 0;}
int selected(void) __attribute__((ifunc("choose")));
static int (*function_pointer)(void)=selected;
static void *worker(void *unused){
  if(own!=42 || zero[16] || (unsigned long)&own%256)return (void*)1;
  own=7;zero[16]=9;return 0;
}
int main(void){
  char buffer[64];void *result;pthread_t threads[3];
  memset(buffer,0,sizeof buffer);memcpy(buffer,"static pie",11);
  if(strcmp(buffer,"static pie") || selected()!=42 || function_pointer()!=42 ||
     function_pointer!=selected || resolved<1)return 1;
  for(int n=0;n<3;++n)if(pthread_create(threads+n,0,worker,0))return 2;
  for(int n=0;n<3;++n)if(pthread_join(threads[n],&result)||result)return 3;
  puts("tls+ifunc");return own==42 && !zero[16] ? 42 : 4;
}
'''
        for model in ('local-exec', 'initial-exec'):
            for indirect in (False, True):
                flags = ('-fPIE', '-ftls-model='+model, '-fno-builtin')
                if indirect:
                    flags += ('-fno-plt',)
                obj = self.compile(f'static-pie-tls-{model}-{indirect}', source, flags=flags)
                for tool in self.tools():
                    with self.subTest(model=model, indirect=indirect, tool=tool):
                        out = self.static_pie('static-pie-tls', obj, linker=tool)
                        self.assertEqual(run(out, expected=42).stdout, b'tls+ifunc\n')
                        relocs = run('readelf', '-rW', out).stdout
                        self.assertIn(b'R_X86_64_RELATIVE', relocs)
                        self.assertIn(b'R_X86_64_IRELATIVE', relocs)
                        self.assertNotIn(b'GLOB_DAT', relocs)

    def test_bootstrap_relaxation_keeps_call_return_addresses_and_registers(self):
        obj = self.assemble('static-pie-bootstrap', '''
.text
.globl _start
_start:
mov answer@GOTPCREL(%rip),%r13
lea answer(%rip),%rcx
cmp %rcx,%r13
jne bad
lea after_call(%rip),%rdi
call *answer@GOTPCREL(%rip)
after_call:
mov %eax,%edi
mov $60,%eax
syscall
bad: mov $1,%edi;mov $60,%eax;syscall
.type answer,@function
answer: cmp %rdi,(%rsp);jne bad;mov $42,%eax;ret
''')
        for tool in self.tools():
            out = self.link('static-pie-bootstrap', obj, linker=tool,
                            options=('-pie', '--no-dynamic-linker'))
            run(out, expected=42)
        result = run(core.LINKER, '-static-pie', '--stats', '-o', self.root/'bootstrap-count', obj)
        import json
        self.assertEqual(json.loads(result.stderr)['counts']['gotpcrelx_relaxations'], 2)

    def test_bootstrap_rewrite_preserves_other_relocations_in_instruction_prefixes(self):
        # An apparent REX prefix is also the last byte of a separate relocated
        # word. It is data, not a safely rewritable instruction. Keep the marked
        # GOT load intact even though the input bytes resemble a MOV pattern.
        obj = self.assemble('static-pie-prefix-relocation', '''
.text
.globl _start
_start: mov $60,%eax; mov $42,%edi; syscall
.globl prefix_word
prefix_word: .long 0x48000000
.byte 0x8b,0x05
displacement: .long 0
.reloc prefix_word,R_X86_64_32,prefix_value
.reloc displacement,R_X86_64_REX_GOTPCRELX,answer-4
.type answer,@function
answer: ret
''')
        definition = self.assemble('static-pie-prefix-value', '.globl prefix_value\n.set prefix_value,42')
        out = self.link('static-pie-prefix-output', obj, definition, options=('-static-pie',))
        run(out, expected=42)
        data = out.read_bytes()
        symbols = {s['name']: s for s in core.elf_symbols(data)}
        prefix = symbols['prefix_word']
        section = core.elf_sections(data)[prefix['section']]
        offset = section['offset'] + prefix['value'] - section['address']
        self.assertEqual(data[offset:offset+6], b'\x2a\0\0\0\x8b\x05')

    def test_absent_weak_calls_and_explicit_absolute_references(self):
        obj = self.compile('static-pie-weak',
                           'extern void optional(void) __attribute__((weak));'
                           'int main(void){if(optional)optional();return optional ? 1 : 42;}',
                           flags=('-fPIE',))
        for tool in self.tools():
            run(self.static_pie('static-pie-weak', obj, linker=tool), expected=42)
        invalid = self.assemble('static-pie-absolute',
                                '.text\n.globl _start\n'
                                '_start: lea absolute(%rip),%rax;ret')
        absolute = self.assemble('static-pie-absolute-definition', '.globl absolute\n.set absolute,0')
        self.rejected(invalid, 'absolute or undefined', absolute, options=('-static-pie',))

    def test_reentry_and_native_gcc_static_pie_driver(self):
        obj = self.compile('static-pie-driver', 'int main(void){return 42;}', flags=('-fPIE',))
        arguments = self.arguments(obj)
        result = run(REENTRY, '-static-pie', '--gc-sections', *arguments)
        self.assertIn(b'domain reentry', result.stdout)
        directory = self.root/'static-pie-driver-tools'
        directory.mkdir()
        (directory/'ld').symlink_to(core.LINKER)
        out = self.root/'static-pie-gcc'
        run('gcc', '-static-pie', '-fno-use-linker-plugin', '-B'+str(directory)+'/',
            '-Wl,--gc-sections', '-o', out, obj)
        self.inspect(out)
        run(out, expected=42)

    def test_unsupported_inputs_tls_and_incompatible_options_preserve_output(self):
        shared = self.link('static-pie-input.so', self.answer, options=('-shared',))
        self.rejected(self.simple, 'static PIE cannot use shared-library', shared,
                      options=('-static-pie',))
        obj = self.compile('static-pie-gd', '__thread int own;int *address(void){return &own;}')
        self.rejected(obj, 'static PIE supports only', options=('-static-pie', '-e', 'address'))
        for options, message in ((('-r', '-static-pie'), '-r cannot'),
                                 (('-shared', '-static-pie'), '-shared cannot')):
            self.rejected(self.simple, message, self.answer, options=options)

    def test_output_mode_order_and_archive_only_search(self):
        for flags, pie, interpreter in (
            (('-static-pie',), True, False),
            (('--no-dynamic-linker', '-pie'), True, False),
            (('-static-pie', '-static'), False, False),
            (('-static-pie', '-no-pie'), False, False),
            (('-static-pie', '--dynamic-linker', hosted.INTERPRETER), True, True),
            (('-pie', '--dynamic-linker', hosted.INTERPRETER, '--no-dynamic-linker'), True, False),
        ):
            out = self.link('static-pie-mode-order', self.simple, self.answer, options=flags)
            self.assertEqual(struct.unpack_from('<H', out.read_bytes(), 16)[0], 3 if pie else 2)
            self.assertEqual(any(s['kind'] == 3 for s in core.elf_segments(out.read_bytes())), interpreter)
            run(out, expected=42)
        directory = self.root/'static-pie-library-search'
        directory.mkdir()
        wrong = self.assemble('static-pie-wrong-answer',
                              '.globl answer\n.text\nanswer: mov $43,%eax;ret')
        run('ld', '-shared', '-soname', 'libanswer.so', '-o', directory/'libanswer.so', wrong)
        run('ar', 'rcs', directory/'libanswer.a', self.answer)
        for policy in ((), ('-Bdynamic',)):
            out = self.link('static-pie-library-choice', self.simple, '-lanswer',
                            options=('-static-pie', *policy, '-L', directory))
            self.inspect(out)
            run(out, expected=42)

    @unittest.skipUnless(os.environ.get('FAS_MUSL_SYSROOT'),
                         'set FAS_MUSL_SYSROOT; static PIE musl gate unverified')
    def test_musl_crt_self_relocation_threads_and_lifecycle(self):
        target = Path(os.environ['FAS_MUSL_SYSROOT']).resolve()
        source = self.root/'static-pie-musl.c'
        source.write_text(r'''
#include <stdio.h>
#include <pthread.h>
__thread int own=42; __thread int zero[17];
static int data=42;static int *pointer=&data;
__attribute__((constructor)) static void init(void){puts("init");}
__attribute__((destructor)) static void fini(void){puts("fini");}
static void *worker(void *unused){if(own!=42 || zero[16])return (void*)1;own=7;return 0;}
int main(void){pthread_t t;void *result;
 if(pthread_create(&t,0,worker,0)||pthread_join(t,&result)||result)return 1;
 puts("musl static pie");return own==42 && !zero[16] ? *pointer : 2;}
''')
        for model in ('local-exec', 'initial-exec'):
            obj = source.with_suffix('.o')
            run(target/'bin/musl-gcc', '-c', '-O2', '-fPIE', '-ftls-model='+model,
                source, '-o', obj)
            for tool in self.tools():
                with self.subTest(model=model, tool=tool):
                    out = self.static_pie('static-pie-musl', obj, musl=target, linker=tool)
                    self.assertEqual(run(out, expected=42).stdout,
                                     b'init\nmusl static pie\nfini\n')


if __name__ == '__main__':
    unittest.main(verbosity=2)
