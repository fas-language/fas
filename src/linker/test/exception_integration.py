#!/usr/bin/env python3
"""Compiler personality/LSDA records, cleanup and cross-DSO exception propagation."""
from pathlib import Path
import random
import shutil
import struct
import subprocess
import unittest

import integration as core
import hosted_integration as hosted

run = core.run


class ExceptionTests(unittest.TestCase):
    setUpClass = classmethod(core.LinkerTests.setUpClass.__func__)
    tearDownClass = classmethod(core.LinkerTests.tearDownClass.__func__)
    assemble = classmethod(core.LinkerTests.assemble.__func__)
    link = core.LinkerTests.link
    rejected = core.LinkerTests.rejected
    setUp = hosted.HostedTests.setUp
    crt = hosted.HostedTests.crt
    hosted = hosted.HostedTests.hosted

    @staticmethod
    def compilers():
        tools = ['g++']
        clang = shutil.which('clang++-22') or shutil.which('clang++')
        if clang:
            tools.append(clang)
        return tools

    @staticmethod
    def linkers():
        return [core.LINKER, 'ld'] + (['ld.lld-22'] if shutil.which('ld.lld-22') else [])

    def compile(self, name, source, compiler='g++'):
        path = self.root/(name+'.cpp')
        path.write_text(source)
        obj = path.with_suffix('.o')
        run(compiler, '-c', '-O2', '-g', '-fPIC', '-fomit-frame-pointer',
            '-ffunction-sections', '-fdata-sections', path, '-o', obj)
        return obj

    def runtime(self):
        return self.crt('libstdc++.so'), self.crt('libgcc_s.so.1')

    def test_cross_object_cleanup_rethrow_and_partial_links(self):
        throw_source = r'''
extern int cleaned;
struct Guard { ~Guard(){ ++cleaned; } };
__attribute__((noinline)) void throw_value(){Guard guard;throw 42;}
__attribute__((noinline)) void dead_throw(){throw 91.0;}
'''
        catch_source = r'''
#include <cstdio>
int cleaned;
extern void throw_value();
static void rethrow_value(){try{throw_value();}catch(int value){if(value!=42)throw 7;throw;}}
int main(){
 try{rethrow_value();}catch(int value){if(cleaned!=1)return 1;std::puts("caught-cleaned-rethrown");return value;}
 return 2;
}
'''
        for compiler in self.compilers():
            thrower = self.compile('exception-throw', throw_source, compiler)
            catcher = self.compile('exception-catch', catch_source, compiler)
            self.assertTrue(any(s['name'].startswith('.gcc_except_table')
                                for s in core.elf_sections(thrower.read_bytes())))
            for partial in (False, True):
                objects = (thrower, catcher)
                if partial:
                    objects = (self.link('exception-partial.o', *objects, options=('-r',)),)
                for gc in (False, True):
                    for pie in (False, True):
                        for linker in self.linkers():
                            with self.subTest(compiler=compiler, partial=partial, gc=gc, pie=pie, linker=linker):
                                out = self.hosted('exception-cleanup', *objects, *self.runtime(),
                                                  pie=pie, gc=gc, linker=linker,
                                                  extra=('--eh-frame-hdr',))
                                self.assertEqual(run(out, expected=42).stdout, b'caught-cleaned-rethrown\n')
                                self.assertEqual(run('readelf', '-aW', '--debug-dump=frames', out).stderr, b'')
                                symbols = {s['name'] for s in core.elf_symbols(out.read_bytes())}
                                if linker == core.LINKER or not gc:
                                    self.assertEqual('_Z10dead_throwv' in symbols, not gc)

    def test_exceptions_cross_shared_boundaries_in_both_directions(self):
        library_source = r'''
extern "C" int cleaned;
extern "C" void host_throw();
struct Guard { ~Guard(){++cleaned;} };
extern "C" __attribute__((noinline)) void library_throw(){Guard guard;throw 42;}
extern "C" int library_catch(){try{Guard guard;host_throw();}catch(int value){return value;}return 1;}
'''
        main_source = r'''
#include <cstdio>
extern "C" {int cleaned;void library_throw();int library_catch();}
extern "C" void host_throw(){throw 42;}
int main(){
 try{library_throw();}catch(int value){if(value!=42 || cleaned!=1)return 1;}
 int value=library_catch();if(cleaned!=2)return 2;
 std::puts("cross-dso-cleanup");return value;
}
'''
        for compiler in self.compilers():
            obj = self.compile('exception-library', library_source, compiler)
            main = self.compile('exception-host', main_source, compiler)
            for producer in self.linkers():
                library = self.link('libfas_exceptions.so', obj, *self.runtime(), hosted.LIBC,
                                    linker=producer, options=('-shared', '--gc-sections', '--eh-frame-hdr',
                                    '-soname', 'libfas_exceptions.so', '--hash-style=both'))
                for consumer in self.linkers():
                    with self.subTest(compiler=compiler, producer=producer, consumer=consumer):
                        out = self.hosted('exception-boundary', main, library, *self.runtime(),
                                          gc=True, linker=consumer, extra=('--eh-frame-hdr', '-rpath', '$ORIGIN'))
                        self.assertEqual(run(out, expected=42).stdout, b'cross-dso-cleanup\n')

    def test_late_dlopen_exceptions_thread_destructors_and_reload(self):
        library_source = r'''
static void (*notify)(int);
static int initialized;
struct Global {
 Global(){initialized=7;}
 ~Global(){if(notify)notify(4);initialized=0;}
} global;
struct Marker {
 int value;
 Marker():value(42){notify(1);}
 ~Marker(){notify(3);}
};
thread_local Marker marker;
struct Guard {~Guard(){notify(5);}};
extern "C" void set_callback(void (*callback)(int)){notify=callback;}
extern "C" void enter(){
 if(initialized!=7)throw 1;
 Guard guard;throw marker.value;
}
'''
        main_source = r'''
#include <atomic>
#include <cstdio>
#include <dlfcn.h>
#include <thread>
#include <initializer_list>
static std::atomic<int> events[6];
static void callback(int event){++events[event];}
int main(int argc,char **argv){
 if(argc!=2)return 1;
 for(int cycle=0;cycle<2;++cycle){
  void *module=dlopen(argv[1],RTLD_NOW|RTLD_LOCAL);if(!module)return 2;
  auto set=(void (*)(void (*)(int)))dlsym(module,"set_callback");
  auto enter=(void (*)())dlsym(module,"enter");if(!set||!enter)return 3;
  set(callback);
  auto worker=[enter](){try{enter();}catch(int value){if(value==42)++events[2];}};
  std::thread one(worker),two(worker);one.join();two.join();
  for(int event:{1,2,3,5})if(events[event]!=(cycle+1)*2)return 4;
  if(dlclose(module))return 5;
  if(events[4]!=cycle+1)return 6;
 }
 std::puts("late-exceptions-thread-destruction-reload");return 42;
}
'''
        for compiler in self.compilers():
            obj = self.compile('late-exception-library', library_source, compiler)
            main = self.compile('late-exception-host', main_source, compiler)
            for producer in self.linkers():
                library = self.link('libfas_late_exceptions.so', self.crt('crti.o'),
                                    self.crt('crtbeginS.o'), obj, *self.runtime(),
                                    hosted.LIBC, hosted.INTERPRETER,
                                    self.crt('crtendS.o'), self.crt('crtn.o'),
                                    linker=producer, options=('-shared', '--gc-sections',
                                    '--eh-frame-hdr', '-z', 'defs', '-soname', 'libfas_late_exceptions.so'))
                for consumer in self.linkers():
                    with self.subTest(compiler=compiler, producer=producer, consumer=consumer):
                        out = self.hosted('late-exception-host', main, *self.runtime(),
                                          gc=True, linker=consumer, extra=('--eh-frame-hdr',))
                        self.assertEqual(run(out, library, expected=42).stdout,
                                         b'late-exceptions-thread-destruction-reload\n')

    def test_augmentation_bounds_encoding_and_mutations(self):
        obj = self.compile('exception-malformed', 'extern void fail(); int answer(){try{fail();}catch(int value){return value;}return 0;}')
        original = obj.read_bytes()
        section = next(s for s in core.elf_sections(original) if s['name']=='.eh_frame')
        at = section['offset']
        name = original.index(b'zPLR\0', at, at+section['size'])
        # Version 1, one-byte code/data alignment and return register operands.
        augmentation_length = name+5+3
        self.assertEqual(original[augmentation_length], 7)
        edits = [(name+1, ord('X')), (name+2, ord('P')),
                 (augmentation_length, 127), (augmentation_length, 0),
                 (augmentation_length+1, 0x7f), (augmentation_length+6, 0x7f),
                 (augmentation_length+7, 0x7f)]
        bad = self.root/'exception-malformed-edited.o'
        for offset, value in edits:
            with self.subTest(offset=offset, value=value):
                data = bytearray(original)
                data[offset] = value
                bad.write_bytes(data)
                self.rejected(self.simple, '.eh_frame', bad, *self.runtime(),
                              options=('-pie',))
        rng = random.Random(0x1D5A)
        out = self.root/'exception-mutations'
        for _ in range(256):
            data = bytearray(original)
            for _ in range(rng.randrange(1, 5)):
                data[at+rng.randrange(section['size'])] ^= rng.randrange(1, 256)
            bad.write_bytes(data)
            result = subprocess.run([core.LINKER, '-pie', '-o', str(out), str(self.simple),
                                     str(bad), *self.runtime()], capture_output=True, timeout=3)
            self.assertIn(result.returncode, (0, 1), result.stderr)
            self.assertNotIn(b'Fatal error', result.stderr)

    def test_rtti_typeinfo_identity_and_cpp_global_lifecycle_across_dso(self):
        header = self.root/'exception-types.hpp'
        header.write_text(r'''
#include <typeinfo>
struct Base {virtual ~Base();};
struct Derived:Base {int value=42;~Derived() override;};
extern "C" Base *make_object();
extern "C" const std::type_info *library_type();
extern "C" int destruction_count();
''')
        library_source = r'''
#include "exception-types.hpp"
#include <cstdio>
static int destroyed;
Base::~Base()=default;
Derived::~Derived(){++destroyed;}
struct LibraryGlobal {
 LibraryGlobal(){std::puts("library-init");}
 ~LibraryGlobal(){std::puts("library-fini");}
} library_global;
extern "C" Base *make_object(){return new Derived;}
extern "C" const std::type_info *library_type(){return &typeid(Derived);}
extern "C" int destruction_count(){return destroyed;}
'''
        main_source = r'''
#include "exception-types.hpp"
#include <cstdio>
struct HostGlobal {
 HostGlobal(){std::puts("host-init");}
 ~HostGlobal(){std::puts("host-fini");}
} host_global;
int main(){
 Base *base=make_object();Derived *derived=dynamic_cast<Derived*>(base);
 if(!derived || derived->value!=42)return 1;
 if(typeid(*base)!=typeid(Derived) || library_type()!=&typeid(Derived))return 2;
 delete base;if(destruction_count()!=1)return 3;
 std::puts("rtti-delete");return 42;
}
'''
        expected = b'library-init\nhost-init\nrtti-delete\nhost-fini\nlibrary-fini\n'
        for compiler in self.compilers():
            obj = self.compile('rtti-library', library_source, compiler)
            main = self.compile('rtti-main', main_source, compiler)
            for producer in self.linkers():
                library = self.link('libfas_rtti.so', self.crt('crti.o'), self.crt('crtbeginS.o'),
                                    obj, *self.runtime(), hosted.LIBC, self.crt('crtendS.o'), self.crt('crtn.o'),
                                    linker=producer, options=('-shared', '--gc-sections', '--eh-frame-hdr',
                                    '-z', 'defs', '-soname', 'libfas_rtti.so'))
                for consumer in self.linkers():
                    with self.subTest(compiler=compiler, producer=producer, consumer=consumer):
                        out = self.hosted('rtti-lifecycle', main, library, *self.runtime(),
                                          gc=True, linker=consumer, extra=('--eh-frame-hdr', '-rpath', '$ORIGIN'))
                        self.assertEqual(run(out, expected=42).stdout, expected)


if __name__ == '__main__':
    unittest.main(verbosity=2)
