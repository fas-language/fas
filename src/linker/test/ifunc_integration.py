#!/usr/bin/env python3
"""Defined IFUNCs: local resolver relocations, preemption and address identity."""
import struct
import shutil
import unittest

import hosted_integration as hosted

core = hosted.core
run = core.run
LINKER = core.LINKER
LLD = shutil.which('ld.lld') or shutil.which('ld.lld-22')


class IfuncTests(unittest.TestCase):
    setUpClass = hosted.HostedTests.__dict__['setUpClass']
    tearDownClass = hosted.HostedTests.__dict__['tearDownClass']
    assemble = hosted.HostedTests.__dict__['assemble']
    setUp = hosted.HostedTests.setUp
    link = core.LinkerTests.link
    rejected = core.LinkerTests.rejected
    compile = hosted.HostedTests.compile
    crt = hosted.HostedTests.crt
    hosted = hosted.HostedTests.hosted

    def produce(self, name, *objects, linker=LINKER, extra=()):
        return self.link('lib'+name+'.so', *objects, linker=linker,
                         options=('-shared','-soname','lib'+name+'.so','--gc-sections',*extra))

    def test_executable_local_hidden_and_global_ifunc(self):
        source = r'''
#ifndef CHECK_IDENTITY
#define CHECK_IDENTITY 1
#endif
static int implementation(void){return 42;}
static int (*volatile selected)(void)=implementation;
static void *resolve(void){return selected;}
static int local(void) __attribute__((ifunc("resolve")));
__attribute__((visibility("hidden"))) int hidden(void) __attribute__((ifunc("resolve")));
int answer(void) __attribute__((ifunc("resolve")));
static int (*volatile pointers[])(void)={local,hidden,answer};
int main(void){
 if(local()!=42 || hidden()!=42 || answer()!=42) return 1;
 if(CHECK_IDENTITY && (pointers[0]!=local || pointers[1]!=hidden || pointers[2]!=answer)) return 2;
 return pointers[0]()+pointers[1]()+pointers[2]()==126 ? 42 : 3;
}
'''
        for compiler in ('gcc','clang'):
            if not shutil.which(compiler):
                continue
            obj = self.compile('ifunc-own',source,compiler=compiler)
            # This host's GNU ld emits PLT addresses for PC32 references but
            # resolves R_X86_64_64 pointers to implementations in PIE output.
            # Keep its call/stored-pointer oracle; check full identity with
            # fas-ld (and LLD when available).
            gnu_obj = self.compile('ifunc-gnu',source,compiler=compiler,
                                   flags=('-DCHECK_IDENTITY=0',))
            for pie in (False,True):
                for linker in (LINKER,'ld',*((LLD,) if LLD else ())):
                    with self.subTest(compiler=compiler,pie=pie,linker=linker):
                        out = self.hosted('ifunc-own',gnu_obj if linker=='ld' and pie else obj,
                                          pie=pie,gc=True,linker=linker)
                        run(out,expected=42)
                        self.assertEqual(run('readelf','-aW',out).stderr,b'')
                        if linker == LINKER:
                            data=out.read_bytes()
                            rela=next(s for s in core.elf_sections(data) if s['name']=='.rela.dyn')
                            kinds=[struct.unpack_from('<Q',data,at+8)[0] & 0xffffffff
                                   for at in range(rela['offset'],rela['offset']+rela['size'],24)]
                            self.assertIn(37,kinds)
                            self.assertFalse(any(k!=37 for k in kinds[kinds.index(37):]))

    def test_shared_preemption_protected_hidden_and_dlsym(self):
        obj = self.compile('ifunc-exports',r'''
static int implementation(void){return 35;}
static void *resolve(void){return implementation;}
int answer(void) __attribute__((ifunc("resolve")));
__attribute__((visibility("protected"))) int protected_answer(void) __attribute__((ifunc("resolve")));
__attribute__((visibility("hidden"))) int hidden_answer(void) __attribute__((ifunc("resolve")));
static int (*volatile pointer)(void)=answer;
int check(void){
 if(pointer!=answer || protected_answer()!=35 || hidden_answer()!=35) return 1;
 return answer()+7;
}
int (*address(void))(void){return answer;}
''')
        main = self.compile('ifunc-shared-main',r'''
#include <dlfcn.h>
int answer(void); int protected_answer(void); int check(void);
int (*address(void))(void);
int main(void){
 if(answer()!=35 || check()!=42 || address()!=answer) return 1;
 if(dlsym(RTLD_DEFAULT,"answer")!=(void*)answer || protected_answer()!=35) return 2;
 return 42;
}
''')
        override = self.compile('ifunc-override-main',r'''
int answer(void){return 42;}
int protected_answer(void){return 99;}
int check(void); int (*address(void))(void);
int main(void){return check()==49 && address()==answer ? 42 : 1;}
''')
        for producer in (LINKER,'ld'):
            library=self.produce('ifuncexports',obj,linker=producer)
            symbols={s['name']:s for s in core.elf_symbols(library.read_bytes(),kind=11)}
            self.assertEqual(symbols['answer']['info'] & 15,10)
            self.assertEqual(symbols['protected_answer']['info'] & 15,10)
            self.assertNotIn('hidden_answer',symbols)
            for consumer in (LINKER,'ld'):
                for source in (main,override):
                    out=self.hosted('ifunc-consumer',source,library,linker=consumer,
                                    extra=('-rpath','$ORIGIN'))
                    run(out,expected=42)

    def test_local_resolvers_after_dependency_relocations_and_name_collisions(self):
        dep=self.produce('ifuncdep',self.compile('ifunc-dep','int target(void){return 42;} int choice=1;'))
        parts=[]; gnu_parts=[]
        for n in range(2):
            source=r'''
#ifndef CHECK_IDENTITY
#define CHECK_IDENTITY 1
#endif
extern int choice,target(void);
static int (*volatile selected)(void)=target;
static void *resolve(void){return choice==1 ? selected : (void*)0;}
static int same(void) __attribute__((ifunc("resolve")));
static int (*volatile pointer)(void)=same;
int CHECK(void){return (!CHECK_IDENTITY || pointer==same) && same()==42 ? 42 : 1;}
'''.replace('int CHECK(', 'int check'+str(n)+'(')
            parts.append(self.compile('ifunc-local-'+str(n),source))
            gnu_parts.append(self.compile('ifunc-gnu-local-'+str(n),source,
                                          flags=('-DCHECK_IDENTITY=0',)))
        main=self.compile('ifunc-dependent-main','int check0(void),check1(void); int main(void){return check0()==42 && check1()==42 ? 42 : 1;}')
        for pie in (False,True):
            for linker in (LINKER,'ld'):
                selected=gnu_parts if linker=='ld' and pie else parts
                out=self.hosted('ifunc-dependent',main,*selected,dep,pie=pie,linker=linker,
                                gc=True,extra=('-rpath','$ORIGIN'))
                run(out,expected=42)

    def test_dlopen_resolvers_and_versioned_exports(self):
        obj=self.compile('ifunc-versioned',r'''
static int implementation(void){return 42;}
static void *resolve(void){return implementation;}
int answer(void) __attribute__((ifunc("resolve")));
''')
        script=self.root/'ifunc-versions.map'
        script.write_text('FAS_1 { global: answer; local: *; };')
        main=self.compile('ifunc-dlopen-main',r'''
#define _GNU_SOURCE
#include <dlfcn.h>
int main(int argc,char **argv){
 for(int i=0;i<3;i++) {
  void *h=dlopen(argv[1],RTLD_NOW); if(!h) return 1;
  int (*f)(void)=dlsym(h,"answer"),(*v)(void)=dlvsym(h,"answer","FAS_1");
  if(!f || f!=v || f()!=42) return 2;
  if(dlclose(h)) return 3;
 }
 return 42;
}
''')
        for producer in (LINKER,'ld'):
            library=self.produce('ifuncversioned',obj,linker=producer,
                                 extra=('--version-script='+str(script),))
            for consumer in (LINKER,'ld'):
                out=self.hosted('ifunc-dlopen-main',main,linker=consumer)
                run(out,library,expected=42)

    def test_invalid_ifunc_uses_preserve_output(self):
        obj=self.assemble('ifunc-static', '.text\n.globl _start\n_start: call answer; ret\n.type answer,@gnu_indirect_function\nanswer: lea impl(%rip),%rax; ret\nimpl: ret\n')
        static = self.link('ifunc-static-inspected',obj)
        self.assertTrue(any(s['name']=='.rela.iplt' for s in core.elf_sections(static.read_bytes())))
        entry=self.assemble('ifunc-entry','.text\n.globl _start\n.type _start,@gnu_indirect_function\n_start: ret\n')
        self.rejected(entry,'cannot be an IFUNC',options=('-pie',))
        invalid=self.assemble('ifunc-data','.data\n.globl answer\n.type answer,@gnu_indirect_function\nanswer: .quad 0\n')
        self.rejected(invalid,'IFUNC resolver must be inside executable code',options=('-shared',))


if __name__ == '__main__':
    unittest.main(verbosity=2)
