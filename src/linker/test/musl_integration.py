#!/usr/bin/env python3
"""Opt-in isolated musl sysroot acceptance; never installs into the host sysroot."""
import os
from pathlib import Path
import unittest

import hosted_integration as hosted

core = hosted.core
run = core.run
SYSROOT = os.environ.get('FAS_MUSL_SYSROOT')


@unittest.skipUnless(SYSROOT, 'set FAS_MUSL_SYSROOT; musl gate unverified without a sysroot')
class MuslTests(unittest.TestCase):
    setUpClass = hosted.HostedTests.__dict__['setUpClass']
    tearDownClass = hosted.HostedTests.__dict__['tearDownClass']
    assemble = hosted.HostedTests.__dict__['assemble']
    link = core.LinkerTests.link
    crt = hosted.HostedTests.crt

    def setUp(self):
        self.sysroot = Path(SYSROOT).resolve()
        self.lib = self.sysroot / 'lib'
        for name in ('libc.so', 'libc.a', 'crt1.o', 'Scrt1.o', 'crti.o', 'crtn.o'):
            self.assertTrue((self.lib / name).is_file(), 'incomplete musl sysroot: '+name)

    def compile(self, name, source, flags=()):
        path = self.root / (name+'.c')
        path.write_text(source)
        obj = path.with_suffix('.o')
        run(self.sysroot / 'bin/musl-gcc', '-c', '-O2', '-g', '-fPIC',
            '-ffunction-sections', '-fdata-sections', *flags, path, '-o', obj)
        return obj

    def hosted(self, name, *objects, mode='pie', tool=core.LINKER):
        options = ('--gc-sections',)
        if mode != 'static':
            options += ('--dynamic-linker', str(self.lib / 'ld-musl-x86_64.so.1'))
            if mode == 'pie':
                options += ('-pie',)
        return self.link(name, self.lib / ('Scrt1.o' if mode == 'pie' else 'crt1.o'),
                         self.lib / 'crti.o', self.crt('crtbeginS.o'), *objects,
                         self.lib / ('libc.a' if mode == 'static' else 'libc.so'),
                         self.crt('libgcc.a'), self.crt('crtendS.o'), self.lib / 'crtn.o',
                         options=options, linker=tool)

    def test_startup_threaded_tls_models_and_shutdown(self):
        source = r'''
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <errno.h>
__thread long own __attribute__((aligned(256)))=42;
__thread long zero[17];
__attribute__((constructor(101))) static void early(void){puts("early");}
__attribute__((constructor(102))) static void late(void){puts("late");}
__attribute__((destructor)) static void done(void){puts("done");}
static void goodbye(void){puts("atexit");}
static void *worker(void *arg){
  if(own!=42 || zero[0] || zero[16] || ((unsigned long)&own % 256))return (void*)1;
  own=7;zero[16]=9;errno=123;
  return own==7 && zero[16]==9 && errno==123 ? 0 : (void*)2;
}
int main(int argc,char **argv){
  pthread_t threads[3];void *result;
  if(argc!=2 || strcmp(argv[1],"check") || !getenv("PATH"))return 1;
  errno=17;
  for(int n=0;n<3;n++)if(pthread_create(threads+n,0,worker,0))return 2;
  for(int n=0;n<3;n++)if(pthread_join(threads[n],&result)||result)return 3;
  if(own!=42 || zero[16] || errno!=17 || atexit(goodbye))return 4;
  puts("main");return 42;
}
'''
        for model in ('local-exec', 'initial-exec', 'global-dynamic', 'local-dynamic', 'tlsdesc'):
            flags = ('-mtls-dialect=gnu2',) if model == 'tlsdesc' else ('-ftls-model='+model,)
            obj = self.compile('musl-'+model, source, flags)
            modes = ('static', 'fixed', 'pie') if model in ('local-exec', 'initial-exec') else ('fixed', 'pie')
            for mode in modes:
                for tool in (core.LINKER, 'ld'):
                    with self.subTest(model=model, mode=mode, tool=tool):
                        out = self.hosted('musl-program', obj, mode=mode, tool=tool)
                        self.assertEqual(run(out, 'check', expected=42).stdout,
                                         b'early\nlate\nmain\natexit\ndone\n')
                        self.assertEqual(run('readelf', '-aW', out).stderr, b'')

    def test_shared_library_late_tls_and_dlopen(self):
        obj = self.compile('musl-late', r'''
__thread long own=42;
long *address(void){return &own;}
''', ('-mtls-dialect=gnu2',))
        for tool in (core.LINKER, 'ld'):
            library = self.link('libmusl-late.so', obj, self.lib/'libc.so', linker=tool,
                                options=('-shared', '-soname', 'libmusl-late.so'))
            main = self.compile('musl-dlopen', r'''
#include <dlfcn.h>
#include <pthread.h>
static pthread_barrier_t ready,loaded;
static long *(*address)(void);
static void *worker(void *unused){
  pthread_barrier_wait(&ready);pthread_barrier_wait(&loaded);
  if(*address()!=42)return (void*)1;
  *address()=7;return *address()==7?0:(void*)2;
}
int main(int argc,char **argv){
  pthread_t thread;void *result;
  pthread_barrier_init(&ready,0,2);pthread_barrier_init(&loaded,0,2);
  if(pthread_create(&thread,0,worker,0))return 1;
  pthread_barrier_wait(&ready);
  void *h=dlopen(argv[1],RTLD_NOW|RTLD_LOCAL);if(!h)return 2;
  address=dlsym(h,"address");if(!address)return 3;
  pthread_barrier_wait(&loaded);
  if(pthread_join(thread,&result)||result||*address()!=42)return 4;
  return dlclose(h)?5:42;
}
''')
            out = self.hosted('musl-dlopen', main)
            run(out, library, expected=42)

    def test_real_musl_compiler_driver_static_and_dynamic(self):
        directory = self.root/'musl-driver-bin'
        directory.mkdir(exist_ok=True)
        (directory/'ld').symlink_to(core.LINKER)
        source = self.root/'musl-driver.c'
        source.write_text('#include <stdio.h>\nstatic __thread int tls=42;\n'
                          'int main(void){puts("musl driver");return tls;}\n')
        for flags in (('-static',), ('-no-pie',), ('-pie',)):
            with self.subTest(flags=flags):
                out = self.root/'musl-driver'
                run(self.sysroot/'bin/musl-gcc', '-fno-use-linker-plugin',
                    '-B'+str(directory)+'/', '-O2', '-g', *flags, source, '-o', out)
                self.assertEqual(run(out, expected=42).stdout, b'musl driver\n')
                self.assertEqual(run('readelf', '-aW', out).stderr, b'')


if __name__ == '__main__':
    unittest.main(verbosity=2)
