#!/usr/bin/env python3
"""GNU2 TLS descriptors: loader semantics and independent ELF inspection."""
import struct
import unittest

import hosted_integration as hosted

core = hosted.core
run = core.run
LINKER = core.LINKER


class TlsdescTests(unittest.TestCase):
    setUpClass = hosted.HostedTests.__dict__['setUpClass']
    tearDownClass = hosted.HostedTests.__dict__['tearDownClass']
    assemble = hosted.HostedTests.__dict__['assemble']
    setUp = hosted.HostedTests.setUp
    link = core.LinkerTests.link
    rejected = core.LinkerTests.rejected
    compile = hosted.HostedTests.compile
    crt = hosted.HostedTests.crt
    hosted = hosted.HostedTests.hosted

    def descriptor_object(self, name, source):
        return self.compile(name, source, flags=('-mtls-dialect=gnu2',))

    def produce(self, name, *objects, linker=LINKER):
        return self.link('lib'+name+'.so', *objects, linker=linker,
                         options=('-shared', '-soname', 'lib'+name+'.so', '--gc-sections'))

    def test_own_executable_descriptors_and_mixed_models(self):
        source = r'''
#include <pthread.h>
__thread int visible=7;
static __thread int local=35;
static __thread char zeros[48] __attribute__((aligned(256)));
int mixed(void);
static int check(void){
 int result=visible+local+zeros[47];
 if(((unsigned long)zeros)%256 || mixed()!=visible) return 1;
 visible++; local++; zeros[47]++; return result;
}
static void *worker(void *arg){return check()==42 && check()==45 ? 0 : (void*)1;}
int main(void){
 pthread_t t; void *result;
 if(check()!=42 || check()!=45) return 1;
 if(pthread_create(&t,0,worker,0) || pthread_join(t,&result) || result) return 2;
 return check()==48 ? 42 : 3;
}
'''
        obj = self.descriptor_object('desc-own', source)
        self.assertIn(b'R_X86_64_GOTPC32_TLSDESC', run('readelf','-rW',obj).stdout)
        mixed = self.compile('desc-mixed', 'extern __thread int visible; int mixed(void){return visible;}')
        for pie in (False, True):
            for linker in (LINKER, 'ld'):
                with self.subTest(pie=pie, linker=linker):
                    out = self.hosted('desc-own', obj, mixed, hosted.INTERPRETER,
                                      pie=pie, gc=True, linker=linker)
                    run(out, expected=42)
                    self.assertEqual(run('readelf', '-aW', out).stderr, b'')

    def test_shared_preemption_local_descriptors_and_imports(self):
        dependency = self.produce('descdep', self.descriptor_object('desc-dep', '__thread int imported=13;'))
        obj = self.descriptor_object('desc-producer', r'''
extern __thread int imported;
__thread int overridden=1;
static __thread int local=11;
__attribute__((visibility("hidden"))) __thread int hidden=5;
__attribute__((visibility("protected"))) __thread int protected_value=6;
int answer(void){return overridden+imported+local+hidden+protected_value;}
int *address(void){return &overridden;}
''')
        consumer = self.compile('desc-consumer', r'''
#include <pthread.h>
__thread int overridden=7,protected_value=99;
extern __thread int imported;
int answer(void); int *address(void);
static void *worker(void *arg){
 if(answer()!=42 || address()!=&overridden) return (void*)1;
 overridden=9; imported=15; return answer()==46 ? 0 : (void*)2;
}
int main(void){
 pthread_t t; void *result;
 if(answer()!=42 || address()!=&overridden) return 1;
 if(pthread_create(&t,0,worker,0) || pthread_join(t,&result) || result) return 2;
 return answer()==42 ? 42 : 3;
}
''')
        for producer in (LINKER, 'ld'):
            library = self.produce('descproducer', obj, dependency, linker=producer)
            self.assertEqual(run('readelf', '-aW', library).stderr, b'')
            if producer == LINKER:
                data = library.read_bytes()
                sections = core.elf_sections(data)
                table = next(s for s in sections if s['kind']==11)
                symbols = core.elf_symbols(data, kind=11)
                self.assertGreater(table['info'], 1)
                self.assertTrue(all(s['info'] >> 4 == 0 for s in symbols[:table['info']]))
                self.assertTrue(all(s['info'] >> 4 != 0 for s in symbols[table['info']:]))
                relocs = next(s for s in sections if s['name']=='.rela.dyn')
                descriptors = [struct.unpack_from('<QQq', data, at)
                               for at in range(relocs['offset'],relocs['offset']+relocs['size'],24)
                               if struct.unpack_from('<Q',data,at+8)[0] & 0xffffffff == 36]
                self.assertGreaterEqual(len(descriptors), 4)
                self.assertTrue(all(info >> 32 != 0 for _,info,_ in descriptors))
            for consumer_linker in (LINKER, 'ld'):
                out = self.hosted('desc-consumer', consumer, library, dependency, hosted.INTERPRETER,
                                  linker=consumer_linker, gc=True, extra=('-rpath','$ORIGIN'))
                run(out, expected=42)

    def test_late_descriptors_with_existing_threads_and_large_tls(self):
        obj = self.descriptor_object('desc-late', r'''
__thread int visible=7;
static __thread int local=35;
static __thread unsigned char buffer[65536];
int answer(void){int r=visible+local+buffer[65535]; visible++; local++; buffer[65535]++; return r;}
''')
        main = self.compile('desc-late-main', r'''
#include <pthread.h>
#include <dlfcn.h>
static pthread_barrier_t barrier;
static int (*answer)(void);
static int okay;
static void *worker(void *arg){
 pthread_barrier_wait(&barrier); okay=answer()==42 && answer()==45; return 0;
}
int main(int argc,char **argv){
 for(int i=0;i<3;i++) {
  pthread_t t;
  if(pthread_barrier_init(&barrier,0,2) || pthread_create(&t,0,worker,0)) return 1;
  void *h=dlopen(argv[1],RTLD_NOW|RTLD_LOCAL); if(!h) return 2;
  answer=dlsym(h,"answer"); if(!answer || answer()!=42) return 3;
  pthread_barrier_wait(&barrier);
  if(pthread_join(t,0) || !okay || answer()!=45) return 4;
  if(pthread_barrier_destroy(&barrier) || dlclose(h)) return 5;
 }
 return 42;
}
''')
        for producer in (LINKER, 'ld'):
            library = self.produce('desclate', obj, linker=producer)
            for consumer in (LINKER, 'ld'):
                out = self.hosted('desc-late-main', main, linker=consumer)
                run(out, library, expected=42)

    def test_weak_undefined_and_bad_descriptor_references(self):
        obj = self.descriptor_object('desc-weak', r'''
extern __thread int absent __attribute__((weak));
int answer(void){return &absent==0 ? 42 : absent;}
''')
        main = self.compile('desc-weak-main', 'int answer(void); int main(void){return answer();}')
        for producer in (LINKER, 'ld'):
            library = self.produce('descweak', obj, linker=producer)
            for consumer in (LINKER, 'ld'):
                out = self.hosted('desc-weak-main', main, library, linker=consumer,
                                  extra=('-rpath','$ORIGIN'))
                run(out, expected=42)
        invalid = self.assemble('desc-invalid', '.text\n.globl answer\nanswer: .long 0\n.reloc 0,R_X86_64_GOTPC32_TLSDESC,datum-4\n.data\n.globl datum\ndatum: .long 42')
        self.rejected(invalid, 'TLS relocation references non-TLS', options=('-shared',))
        self.rejected(obj, 'TLS relocation requires dynamic output')
        missing = self.assemble('desc-missing', '.text\n.globl _start\n_start: .long 0\n.reloc 0,R_X86_64_GOTPC32_TLSDESC,missing-4\n.type missing,@tls_object\n')
        self.rejected(missing, 'undefined TLS symbol missing', options=('-pie',))


if __name__ == '__main__':
    unittest.main(verbosity=2)
