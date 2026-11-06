#!/usr/bin/env python3
"""Real threaded TLS: templates, local/initial/global/local-dynamic access."""
from pathlib import Path
import random
import shutil
import struct
import unittest

import hosted_integration as hosted

core = hosted.core
run = core.run
LINKER = core.LINKER


class TlsTests(unittest.TestCase):
    setUpClass = hosted.HostedTests.__dict__['setUpClass']
    tearDownClass = hosted.HostedTests.__dict__['tearDownClass']
    assemble = hosted.HostedTests.__dict__['assemble']
    setUp = hosted.HostedTests.setUp
    link = core.LinkerTests.link
    rejected = core.LinkerTests.rejected
    compile = hosted.HostedTests.compile
    crt = hosted.HostedTests.crt
    hosted = hosted.HostedTests.hosted

    def test_static_tls_with_explicit_thread_pointer_startup(self):
        # A static linker supplies PT_TLS and offsets. Runtime startup owns
        # allocating/copying the template and installing the thread pointer.
        start = self.assemble('static-tls-start', r'''
.text
.globl _start
_start:
  lea __ehdr_start(%rip),%rsi
  mov 32(%rsi),%rax
  add %rsi,%rax
  movzwl 56(%rsi),%ecx
1: cmpl $7,(%rax)
  je 2f
  add $56,%rax
  loop 1b
  ud2
2: mov 40(%rax),%r8
  mov 48(%rax),%r9
  lea -1(%r9,%r8),%r8
  neg %r9
  and %r9,%r8
  lea tls_buffer(%rip),%rdi
  lea (%rdi,%r8),%r12
  mov %r12,(%r12)
  mov 16(%rax),%rsi
  mov 32(%rax),%rcx
  rep movsb
  mov $158,%eax
  mov $0x1002,%edi
  mov %r12,%rsi
  syscall
  test %rax,%rax
  jne 3f
  call main
  mov %eax,%edi
  mov $60,%eax
  syscall
3: ud2
.bss
.p2align 12
tls_buffer: .zero 8192
''')
        source = r'''
__thread long own __attribute__((aligned(256)))=7;
__thread long zero[19];
int main(void){
  if(own!=7 || zero[0] || zero[18] || ((unsigned long)&own % 256))return 1;
  own=40; zero[18]=2; return own+zero[18];
}
'''
        for compiler in ('gcc', 'clang'):
            if not shutil.which(compiler):
                continue
            for model in ('local-exec', 'initial-exec'):
                obj = self.compile('static-tls', source, compiler,
                                   flags=('-ftls-model='+model,))
                for tool in (LINKER, 'ld'):
                    with self.subTest(compiler=compiler, model=model, tool=tool):
                        out = self.link('static-tls', start, obj, linker=tool,
                                        options=('--gc-sections',))
                        run(out, expected=42)
                        self.assertEqual(run('readelf', '-aW', out).stderr, b'')

    def shared(self, name, source):
        path = self.root / (name+'.c')
        path.write_text(source)
        output = self.root / ('lib'+name+'.so')
        run('gcc', '-fPIC', '-O2', '-shared', '-nostdlib',
            '-Wl,-soname,'+output.name, path, '-o', output)
        return output

    def test_models_initialized_zero_alignment_and_thread_isolation(self):
        source = r'''
#include <pthread.h>
#include <stdint.h>
__thread long own __attribute__((aligned(256)))=42;
__thread long zero[17] __attribute__((aligned(64)));
static void *worker(void *arg){
  if (own!=42 || zero[0] || zero[16] || ((uintptr_t)&own % 256)
      || ((uintptr_t)zero % 64)) return (void*)1;
  own=(long)arg; zero[0]=7; zero[16]=9;
  return own==(long)arg && zero[0]==7 && zero[16]==9 ? 0 : (void*)2;
}
int main(void){
  pthread_t t[4]; void *r;
  for(long n=0;n<4;n++) if(pthread_create(t+n,0,worker,(void*)(n+1)))return 1;
  for(int n=0;n<4;n++) if(pthread_join(t[n],&r)||r)return 2;
  return own==42 && !zero[0] && !zero[16] ? 42 : 3;
}
'''
        for compiler in ('gcc','clang'):
            if not shutil.which(compiler):
                continue
            for model in ('local-exec','initial-exec','global-dynamic','local-dynamic'):
                obj = self.compile('tls-model-'+compiler+'-'+model, source, compiler,
                                   flags=('-ftls-model='+model,))
                for pie in (False,True):
                    for linker in (LINKER,'ld'):
                        with self.subTest(compiler=compiler, model=model, pie=pie, linker=linker):
                            out = self.hosted('tls-model', obj, hosted.INTERPRETER,
                                              pie=pie, gc=True, linker=linker)
                            run(out, expected=42)
                            self.assertEqual(run('readelf','-aW',out).stderr,b'')
                            if linker == LINKER:
                                data = out.read_bytes()
                                segments = [s for s in core.elf_segments(data) if s['kind']==7]
                                self.assertEqual(len(segments),1)
                                segment = segments[0]
                                self.assertEqual(segment['alignment'],256)
                                self.assertLess(segment['file_size'],segment['memory_size'])
                                definitions = {s['name']:s for s in core.elf_symbols(data)}
                                self.assertEqual(definitions['own']['info'] & 15,6)
                                self.assertLess(definitions['own']['value'],segment['memory_size'])

    def test_imported_tls_and_executable_preemption(self):
        library = self.shared('tls-import', '''
__thread long imported=35;
__thread long overridden=1;
long read_override(void){return overridden;}
long *address(void){return &imported;}
''')
        source = r'''
#include <pthread.h>
extern __thread long imported;
__thread long overridden=7;
extern long read_override(void);
extern long *address(void);
static void *worker(void *arg){
  if (imported!=35 || overridden!=7 || read_override()!=7 || address()!=&imported)
    return (void*)1;
  imported=99; overridden=101;
  return read_override()==101 && *address()==99 ? 0 : (void*)2;
}
int main(void){
  pthread_t t; void *r;
  if(pthread_create(&t,0,worker,0)||pthread_join(t,&r)||r)return 1;
  return imported+read_override()==42 && *address()==35 ? 42 : 2;
}
'''
        for model in ('initial-exec','global-dynamic'):
            obj = self.compile('tls-import-'+model, source, flags=('-ftls-model='+model,))
            for linker in (LINKER,'ld'):
                out = self.hosted('tls-import', obj, library, hosted.INTERPRETER,
                                  gc=True, linker=linker, extra=('-rpath','$ORIGIN'))
                run(out, expected=42)
                self.assertEqual(run('readelf','-aW',out).stderr,b'')
                if linker == LINKER:
                    exported = {s['name']:s for s in core.elf_symbols(out.read_bytes(),kind=11)}
                    self.assertEqual(exported['overridden']['info'] & 15,6)
                    self.assertNotEqual(exported['overridden']['section'],0)

    def test_tbss_only_and_seeded_mixed_alignment_layouts(self):
        rng = random.Random(0x715)
        for seed in range(12):
            parts = []
            for n in range(4):
                alignment = 1 << rng.randrange(3,10)
                value = 0 if seed == 0 else rng.randrange(4)
                text = f'__thread long item{n} __attribute__((aligned({alignment})))={value};\n'
                text += f'long check{n}(void){{return item{n}=={value} && ((unsigned long)&item{n})%{alignment}==0;}}\n'
                parts.append(self.compile(f'tls-layout-{seed}-{n}', text,
                                         flags=('-ftls-model=local-exec',)))
            main = self.compile('tls-layout-main', '\n'.join(f'long check{n}(void);' for n in range(4))+
                                '\nint main(void){return '+ ' && '.join(f'check{n}()' for n in range(4))+'?42:1;}')
            rng.shuffle(parts)
            for linker in (LINKER,'ld'):
                out = self.hosted('tls-layout', main, *parts, gc=True, linker=linker)
                run(out, expected=42)

    def test_invalid_tls_use_fails_without_replacing_output(self):
        obj = self.compile('tls-required', '__thread int own=42; int main(void){return own;}',
                           flags=('-ftls-model=global-dynamic',))
        start = self.assemble('tls-start', '.text\n.globl _start\n_start: call main; mov %eax,%edi; mov $60,%eax; syscall')
        self.rejected(start, 'TLS relocation requires dynamic output', obj)
        library = self.shared('tls-foreign','__thread int foreign=42;')
        direct = self.assemble('tls-direct', '.text\n.globl _start\n_start: lea foreign(%rip),%rax; ret')
        self.rejected(direct,'non-TLS relocation',library,options=('-pie',))
        local = self.assemble('tls-local-import', '.text\n.globl _start\n_start: mov %fs:foreign@TPOFF,%eax; ret')
        self.rejected(local,'local TLS relocation',library,options=('-pie',))

    def test_malformed_tls_symbols_flags_and_array_metadata(self):
        obj = self.compile('tls-malformed', '__thread long item=42; int main(void){return item;}',
                           flags=('-ftls-model=local-exec',))
        original = obj.read_bytes()
        sections = core.elf_sections(original)
        tls = next(s for s in sections if s['flags'] & 0x400)
        symbol = next(s for s in core.elf_symbols(original) if s['name']=='item')
        for name,offset,encoding,value,diagnostic in (
            ('tls-noalloc',tls['header']+8,'Q',0x401,'invalid TLS flags'),
            ('tls-readonly',tls['header']+8,'Q',0x402,'invalid TLS flags'),
            ('tls-type',symbol['offset']+4,'B',0x11,'TLS type'),
            ('tls-absolute',symbol['offset']+6,'H',0xfff1,'absolute TLS')):
            data = bytearray(original);struct.pack_into('<'+encoding,data,offset,value)
            path = self.root/(name+'.o');path.write_bytes(data)
            self.rejected(path,diagnostic,options=('-pie',))
        arrays = self.compile('array-malformed', '__attribute__((constructor)) static void f(void){}',
                              flags=('-O0',))
        original = arrays.read_bytes()
        array = next(s for s in core.elf_sections(original) if s['kind']==14)
        for n,(relative,encoding,value) in enumerate(((8,'Q',2),(32,'Q',7),(56,'Q',4),(48,'Q',16))):
            data = bytearray(original);struct.pack_into('<'+encoding,data,array['header']+relative,value)
            path = self.root/f'array-malformed-{n}.o';path.write_bytes(data)
            self.rejected(path,'initialization array metadata',options=('-pie',))


if __name__ == '__main__':
    unittest.main(verbosity=2)
