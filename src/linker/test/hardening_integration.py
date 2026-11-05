#!/usr/bin/env python3
"""Loader-enforced RELRO, binding policy, and imported GNU IFUNC behavior."""
from pathlib import Path
import os
import shutil
import struct
import subprocess
import unittest

import integration as core
import hosted_integration as hosted
import shared_integration as shared

run = core.run
LINKER = core.LINKER


class HardeningTests(unittest.TestCase):
    setUpClass = classmethod(core.LinkerTests.setUpClass.__func__)
    tearDownClass = classmethod(core.LinkerTests.tearDownClass.__func__)
    assemble = classmethod(core.LinkerTests.assemble.__func__)
    link = core.LinkerTests.link
    rejected = core.LinkerTests.rejected
    setUp = hosted.HostedTests.setUp
    crt = hosted.HostedTests.crt
    compile = hosted.HostedTests.compile
    hosted = hosted.HostedTests.hosted
    shared = shared.SharedTests.shared

    def test_relro_permissions_mutable_data_tls_and_write_faults(self):
        self.check_relro(256, (LINKER, 'ld'))
        # GNU ld on this host spans an unmapped LOAD gap in GNU_RELRO when
        # TLS alignment exceeds a page; even its GCC-driver output fails in
        # the loader. Keep this boundary test for our contiguous RW layout.
        self.check_relro(8192, (LINKER,))

    def check_relro(self, tls_alignment, linkers):
        source = r'''
#include <stdint.h>
#include <stdio.h>
#include <signal.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <unistd.h>
int datum=7;
int *const immutable_pointer=&datum;
__thread int tls=35;
__thread char aligned_tls[32] __attribute__((aligned(TLS_ALIGNMENT)));
char large_data[32] __attribute__((aligned(8192)))={1};
extern char _DYNAMIC[];
extern void *get_got(void);
static int writable(void *pointer) {
  FILE *f=fopen("/proc/self/maps","r"); if(!f) return -1;
  char line[1024],mode[8]; unsigned long a,b; int result=-1;
  while(fgets(line,sizeof line,f)) {
    if(sscanf(line,"%lx-%lx %7s",&a,&b,mode)==3 &&
       (uintptr_t)pointer>=a && (uintptr_t)pointer<b) {result=mode[1]=='w'; break;}
  }
  fclose(f); return result;
}
static int can_write(void *pointer) {
  pid_t child=fork(); if(child<0) return -1;
  if(child==0) {
    struct rlimit limit={0,0}; setrlimit(RLIMIT_CORE,&limit);
    signal(SIGSEGV,SIG_DFL);
    __asm__ volatile("orb $0,(%0)"::"r"(pointer):"memory");
    _exit(0);
  }
  int status; if(waitpid(child,&status,0)!=child) return -1;
  return WIFEXITED(status) && WEXITSTATUS(status)==0;
}
__attribute__((constructor)) static void init(void){datum=42;}
int main(int argc,char **argv) {
  int expected=argc==2 && argv[1][0]=='w';
  if(immutable_pointer!=&datum || datum!=42 || tls!=35) return 1;
  tls++; aligned_tls[31]=7; large_data[31]=9; datum++;
  if(tls!=36 || datum!=43 || aligned_tls[31]!=7 || large_data[31]!=9) return 2;
  void *targets[]={get_got(),_DYNAMIC,(void*)&immutable_pointer};
  for(int i=0;i<3;i++)
    if(writable(targets[i])!=expected || can_write(targets[i])!=expected) return 3+i;
  if(writable(&datum)!=1 || writable(&tls)!=1 || writable(large_data)!=1) return 6;
  return 42;
}
'''
        got = self.assemble('get-got', '''.text
.globl get_got
get_got: lea puts@GOTPCREL(%rip),%rax; ret
.section .note.GNU-stack,"",@progbits
''')
        for compiler in ('gcc', 'clang'):
            if not shutil.which(compiler):
                continue
            obj = self.compile('relro-'+compiler, source, compiler=compiler,
                               flags=('-DTLS_ALIGNMENT='+str(tls_alignment),))
            for pie in (False, True):
                for enabled in (False, True):
                    for linker in linkers:
                        with self.subTest(compiler=compiler, pie=pie, enabled=enabled, linker=linker, tls_alignment=tls_alignment):
                            extra = ('-z', 'now', '-z', 'relro' if enabled else 'norelro')
                            out = self.hosted('relro-program', obj, got, hosted.INTERPRETER, pie=pie,
                                              gc=True, linker=linker, extra=extra)
                            run(out, *(() if enabled else ('w',)), expected=42)
                            if linker != LINKER:
                                continue
                            data = out.read_bytes()
                            segments = core.elf_segments(data)
                            relro = [s for s in segments if s['kind']==0x6474e552]
                            self.assertEqual(len(relro), int(enabled))
                            if enabled:
                                segment = relro[0]
                                self.assertEqual(segment['address'] % 4096, 0)
                                self.assertEqual(segment['memory_size'] % 4096, 0)
                                sections = core.elf_sections(data)
                                for section in sections:
                                    if section['name'] in ('.dynamic','.got','.init_array','.fini_array') or section['name'].startswith('.data.rel.ro'):
                                        self.assertGreaterEqual(section['address'],segment['address'])
                                        self.assertLessEqual(section['address']+section['size'],segment['address']+segment['memory_size'])
                            self.assertEqual(run('readelf','-aW',out).stderr,b'')

    def test_binding_policy_last_wins_and_atomic_errors(self):
        obj = self.assemble('policy', '.text\n.globl _start\n_start: mov $60,%eax; mov $42,%edi; syscall')
        for policy, count in (((),1),(('-z','norelro','-z','relro'),1),
                              (('-z','relro','-z','norelro'),0)):
            out = self.link('policy-program',obj,options=('-pie',*policy))
            self.assertEqual(sum(s['kind']==0x6474e552 for s in core.elf_segments(out.read_bytes())),count)
            run(out,expected=42)
        run(LINKER,'-z',expected=1)
        for flags,message in ((('-z','lazy'),'lazy binding'),
                              (('-z','unknown'),'unsupported -z')):
            self.rejected(obj,message,options=('-pie',*flags))

    def test_ifunc_calls_function_addresses_and_preload(self):
        library = self.shared('ifunc', '''static int implementation(void){return 42;}
static void *resolve_answer(void){return implementation;}
int answer(void) __attribute__((ifunc("resolve_answer")));
''')
        obj = self.compile('ifunc-consumer', '''extern int answer(void);
int (*pointer)(void)=answer;
int main(void){if(pointer!=answer) return 1; return pointer()==answer()?answer():2;}
''')
        preload = self.shared('ifunc-preload', 'int answer(void){return 43;}')
        for pie in (False, True):
            for linker in (LINKER,'ld'):
                with self.subTest(pie=pie,linker=linker):
                    out = self.hosted('ifunc-program',obj,library,pie=pie,linker=linker,
                                      extra=('-rpath','$ORIGIN'))
                    run(out,expected=42)
                    result = subprocess.run([str(out)],capture_output=True,timeout=5,
                                            env=dict(os.environ,LD_PRELOAD=str(preload)))
                    self.assertEqual(result.returncode,43,result.stderr)
                    self.assertEqual(run('readelf','-aW',out).stderr,b'')


if __name__ == '__main__':
    unittest.main(argv=[__file__],verbosity=2)
