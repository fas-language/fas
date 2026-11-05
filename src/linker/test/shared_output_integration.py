#!/usr/bin/env python3
"""Independent consumers and loader semantics for fas-ld produced DSOs."""
import os
from pathlib import Path
import shutil
import struct
import subprocess
import unittest

import integration as core
import hosted_integration as hosted

run = core.run
LINKER = core.LINKER


class SharedOutputTests(unittest.TestCase):
    setUpClass = classmethod(core.LinkerTests.setUpClass.__func__)
    tearDownClass = classmethod(core.LinkerTests.tearDownClass.__func__)
    assemble = classmethod(core.LinkerTests.assemble.__func__)
    link = core.LinkerTests.link
    rejected = core.LinkerTests.rejected
    setUp = hosted.HostedTests.setUp
    crt = hosted.HostedTests.crt
    compile = hosted.HostedTests.compile
    hosted = hosted.HostedTests.hosted

    def produce(self, name, *objects, linker=LINKER, extra=()):
        return self.link('lib'+name+'.so',*objects,linker=linker,
                         options=('-shared','-soname','lib'+name+'.so',*extra))

    def consumer(self, name, source, library, *, linker=LINKER, extra=()):
        obj = self.compile(name,source)
        return self.hosted(name,obj,library,linker=linker,
                           extra=('-rpath','$ORIGIN',*extra))

    def test_exports_gc_soname_and_independent_consumers(self):
        source = '''int datum=35; static int local=7;
int *pointer=&datum; int *offset=&datum+1;
__attribute__((visibility("hidden"))) int hidden=11;
__attribute__((visibility("protected"))) int protected_value=13;
int helper(void){return 7;}
int answer(void){return helper()+*pointer+(offset==&datum+1?0:99);}
int unused_export(void){return local+hidden+protected_value;}
'''
        for compiler in ('gcc','clang'):
            if not shutil.which(compiler):
                continue
            obj = self.compile('producer-'+compiler,source,compiler=compiler)
            library = self.produce('producer',obj,extra=('--gc-sections',))
            data = library.read_bytes()
            segments = core.elf_segments(data)
            self.assertFalse(any(s['kind']==3 for s in segments))
            self.assertEqual(int.from_bytes(data[24:32],'little'),0)
            symbols = {s['name']:s for s in core.elf_symbols(data,kind=11)}
            self.assertTrue({'answer','helper','datum','pointer','offset','unused_export','protected_value'} <= symbols.keys())
            self.assertNotIn('hidden',symbols)
            self.assertNotIn('local',symbols)
            self.assertEqual(symbols['protected_value']['other'] & 3,3)
            self.assertEqual(library.stat().st_mode & 0o777,0o644)
            self.assertEqual(run('readelf','-aW',library).stderr,b'')
            for linker in (LINKER,'ld'):
                out = self.consumer('consumer','extern int answer(void); int main(void){return answer();}',library,linker=linker)
                run(out,expected=42)

    def test_preemption_weak_protected_and_function_pointer_identity(self):
        obj = self.compile('preemption', '''int datum=7;
__attribute__((weak)) int weak_value=9;
__attribute__((visibility("protected"))) int protected_value=11;
int helper(void){return 5;}
int (*function)(void)=helper; int *pointer=&datum;
int answer(void){if(function!=helper || pointer!=&datum) return 1;
return helper()+datum+weak_value+protected_value;}
''')
        for producer in (LINKER,'ld'):
            library = self.produce('preemption',obj,linker=producer,extra=('--gc-sections',))
            for consumer in (LINKER,'ld'):
                source = '''int datum=13,weak_value=11,protected_value=99;
int helper(void){return 7;}
extern int answer(void); int main(void){return answer();}'''
                out = self.consumer('override',source,library,linker=consumer)
                run(out,expected=42)

    def test_unresolved_imports_dependency_and_dlopen_lifecycle(self):
        obj = self.compile('plugin', '''extern int callback(void);
extern void finished(void);
static int state;
__attribute__((constructor)) static void init(void){state=7;}
__attribute__((destructor)) static void fini(void){finished();}
int answer(void){return callback()+state;}
''')
        library = self.produce('plugin',obj,extra=('--gc-sections',))
        source = r'''
#include <dlfcn.h>
static int closed;
int callback(void){return 35;}
void finished(void){closed++;}
int main(int argc,char **argv){
 for(int i=0;i<3;i++) {
  void *h=dlopen(argv[1],RTLD_NOW|RTLD_LOCAL); if(!h) return 1;
  int (*answer)(void)=dlsym(h,"answer"); if(!answer || answer()!=42) return 2;
  if(dlclose(h) || closed!=i+1) return 3;
 }
 return 42;
}
'''
        for linker in (LINKER,'ld'):
            obj = self.compile('dlopen-main',source)
            out = self.hosted('dlopen-main',obj,linker=linker,extra=('-E',))
            run(out,library,expected=42)

    def test_shared_tls_general_local_dynamic_and_initial_exec(self):
        source = '''__thread int value=7;
static __thread int local=35;
static __thread char zeros[48] __attribute__((aligned(256)));
int answer(void){int result=value+local+zeros[47]; value++; local++; zeros[47]++; return result;}
'''
        for model in ('global-dynamic','local-dynamic','initial-exec'):
            for compiler in ('gcc','clang'):
                if not shutil.which(compiler):
                    continue
                obj = self.compile('tls-producer',source,compiler=compiler,
                                   flags=('-ftls-model='+model,))
                library = self.produce('tlsproducer',obj,hosted.INTERPRETER,
                                       extra=('--gc-sections',))
                main = r'''
#include <pthread.h>
#include <stdint.h>
extern int answer(void);
static void *worker(void *p){return (void*)(uintptr_t)(answer()==42 && answer()==45);}
int main(void){
 pthread_t thread; void *result;
 if(answer()!=42 || answer()!=45) return 1;
 if(pthread_create(&thread,0,worker,0) || pthread_join(thread,&result) || result!=(void*)1) return 2;
 if(answer()!=48) return 3;
 return 42;
}
'''
                for linker in (LINKER,'ld'):
                    with self.subTest(model=model,compiler=compiler,linker=linker):
                        out = self.consumer('tlsconsumer',main,library,linker=linker)
                        run(out,expected=42)
                self.assertEqual(run('readelf','-aW',library).stderr,b'')

    def test_dlopen_tls_with_threads_created_before_module(self):
        main = r'''
#include <pthread.h>
#include <dlfcn.h>
static pthread_barrier_t barrier;
static int (*answer)(void);
static int okay;
static void *worker(void *unused){
 pthread_barrier_wait(&barrier);
 okay=answer()==42 && answer()==45;
 return 0;
}
int main(int argc,char **argv){
 for(int i=0;i<3;i++) {
  pthread_t thread;
  if(pthread_barrier_init(&barrier,0,2) || pthread_create(&thread,0,worker,0)) return 1;
  void *module=dlopen(argv[1],RTLD_NOW|RTLD_LOCAL); if(!module) return 2;
  answer=dlsym(module,"answer"); if(!answer || answer()!=42) return 3;
  pthread_barrier_wait(&barrier);
  if(pthread_join(thread,0) || !okay || answer()!=45) return 4;
  pthread_barrier_destroy(&barrier);
  if(dlclose(module)) return 5;
 }
 return 42;
}
'''
        consumer = self.compile('late-tls-main',main)
        for model in ('global-dynamic','local-dynamic'):
            obj = self.compile('late-tls', '''__thread int value=7;
static __thread int local=35;
static __thread unsigned char buffer[65536];
int answer(void){int result=value+local+buffer[65535]; value++; local++; buffer[65535]++; return result;}
''',flags=('-ftls-model='+model,))
            library = self.produce('latetls',obj,hosted.INTERPRETER)
            for linker in (LINKER,'ld'):
                with self.subTest(model=model,linker=linker):
                    out = self.hosted('late-tls-main',consumer,linker=linker)
                    run(out,library,expected=42)

    def test_invalid_shared_modes_preserve_output(self):
        obj = self.assemble('bad-shared','.text\n.globl answer\nanswer: ret')
        for options,message in ((('-shared','-pie'),'cannot be combined'),
                                (('-soname','x'),'requires -shared'),
                                (('-shared','-soname',''),'empty SONAME')):
            self.rejected(obj,message,options=options)
        missing = self.compile('missing-shared','extern int missing(void); int answer(void){return missing();}')
        for policy in (('--no-undefined',),('-z','defs')):
            self.rejected(missing,'undefined symbol missing',options=('-shared',*policy))
        preinit = self.assemble('shared-preinit','.section .preinit_array,"aw",@preinit_array\n.quad 0')
        self.rejected(preinit,'preinit arrays',options=('-shared',))
        local_exec = self.compile('shared-local-exec','__thread int x; int answer(void){return x;}',
                                  flags=('-ftls-model=local-exec',))
        self.rejected(local_exec,'local-exec TLS',options=('-shared',))

    def test_nonallocatable_plt_relocations_without_runtime_stubs(self):
        # Reduced source fixtures for both shared-link AFL findings. Metadata
        # references alone do not require a runtime PLT entry.
        for defined in (False,True):
            obj = self.assemble('metadata-plt', '''.text
.globl answer
answer: ret
.section .debug_info,"",@progbits
.long 0
.reloc 0,R_X86_64_PLT32,datum-4
''' + ('.data\n.globl datum\ndatum: .quad 35\n' if defined else '.globl datum\n'))
            self.produce('metadata-plt',obj)
        obj = self.compile('retargeted-relocations','''__thread int local_tls=7;
int datum=35; int answer(void){return datum+local_tls;}
''')
        data = bytearray(obj.read_bytes())
        sections = core.elf_sections(data)
        text_relocs = next(s for s in sections if s['name']=='.rela.text.answer')
        debug_strings = next(s for s in sections if s['name']=='.debug_str')
        headers = struct.unpack_from('<Q',data,40)[0]
        struct.pack_into('<I',data,headers+text_relocs['index']*64+44,debug_strings['index'])
        # Preserve enough destination space for every retargeted field.
        self.assertGreaterEqual(debug_strings['size'],64)
        obj.write_bytes(data)
        self.produce('retargeted-relocations',obj)

    def test_version_definitions_imports_hiding_and_inheritance(self):
        obj = self.compile('versioned-producer', '''extern int puts(const char*);
int datum=35; int answer(void){return 7;}
int private_function(void){return 99;}
__attribute__((constructor)) static void init(void){puts("versioned-init");}
''')
        script = self.root/'versions.map'
        script.write_text('FAS_1 { global: answer; local: private*; };\n'
                          'FAS_2 { global: datum; local: *; } FAS_1;\n')
        for producer in (LINKER,'ld'):
            library = self.produce('versioned',obj,hosted.LIBC,linker=producer,
                                   extra=('--gc-sections','--version-script='+str(script)))
            self.assertEqual(run('readelf','-aW',library).stderr,b'')
            report = run('readelf','--dyn-syms','--version-info','-W',library).stdout
            self.assertIn(b'answer@@FAS_1',report)
            self.assertIn(b'datum@@FAS_2',report)
            self.assertNotIn(b'private_function',report)
            for consumer in (LINKER,'ld'):
                out = self.consumer('versioned-main', '''extern int answer(void),datum;
int main(void){return answer()+datum;}
''',library,linker=consumer)
                self.assertEqual(run(out,expected=42).stdout,b'versioned-init\n')
                requirements = run('readelf','-V',out).stdout
                self.assertIn(b'FAS_1',requirements)
                self.assertIn(b'FAS_2',requirements)

    def test_version_script_anonymous_policy_errors_and_alias_protection(self):
        obj = self.compile('anonymous','int hidden(void){return 35;} int answer(void){return hidden()+7;}')
        script = self.root/'anonymous.map'
        script.write_text('{ global: answer; local: *; };')
        library = self.produce('anonymous',obj,extra=('--version-script',str(script),'--gc-sections'))
        symbols = {s['name'] for s in core.elf_symbols(library.read_bytes(),kind=11)}
        self.assertIn('answer',symbols)
        self.assertNotIn('hidden',symbols)
        out = self.consumer('anonymous-main','extern int answer(void); int main(void){return answer();}',library)
        run(out,expected=42)
        original = script.read_bytes()
        run(LINKER,'-shared','--version-script',script,'-o',script,obj,expected=1)
        self.assertEqual(script.read_bytes(),original)
        for text in ('A { global: answer; } MISSING;', 'A {}; A {};',
                     '{ global: a; }; B {};', 'A { global: a?; };',
                     'A { extern "C++" { x; }; };', 'A { global: a;',
                     '/* unterminated', 'A { global: answer; }; B { global: answer; };'):
            script.write_text(text)
            self.rejected(obj,'version',options=('-shared','--version-script',str(script)))


if __name__ == '__main__':
    unittest.main(argv=[__file__],verbosity=2)
