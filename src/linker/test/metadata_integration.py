#!/usr/bin/env python3
"""Compiler drivers, independent GNU hash lookup and deterministic build IDs."""
import hashlib
from pathlib import Path
import shutil
import struct
import unittest

import integration as core
import hosted_integration as fixtures

run = core.run


def gnu_hash(name):
    value = 5381
    for c in name.encode():
        value = (value * 33 + c) & 0xffffffff
    return value


class MetadataTests(unittest.TestCase):
    setUpClass = classmethod(core.LinkerTests.setUpClass.__func__)
    tearDownClass = classmethod(core.LinkerTests.tearDownClass.__func__)
    assemble = classmethod(core.LinkerTests.assemble.__func__)
    link = core.LinkerTests.link
    rejected = core.LinkerTests.rejected
    compile = fixtures.HostedTests.compile
    crt = fixtures.HostedTests.crt
    hosted = fixtures.HostedTests.hosted
    setUp = fixtures.HostedTests.setUp

    def check_hash(self, path, style):
        data = path.read_bytes()
        sections = {s['name']: s for s in core.elf_sections(data)}
        self.assertEqual('.hash' in sections, style != 'gnu')
        self.assertEqual('.gnu.hash' in sections, style != 'sysv')
        dynamic = run('readelf', '-dW', path).stdout
        self.assertEqual(b'(GNU_HASH)' in dynamic, style != 'sysv')
        self.assertEqual(b'(HASH)' in dynamic, style != 'gnu')
        self.assertEqual(run('readelf', '-aW', path).stderr, b'')
        if style == 'sysv':
            return
        section = sections['.gnu.hash']
        self.assertEqual(section['link'], sections['.dynsym']['index'])
        buckets, first, bloom, shift = struct.unpack_from('<IIII', data, section['offset'])
        self.assertGreater(buckets, 0)
        self.assertEqual(bloom & (bloom - 1), 0)
        symbols = core.elf_symbols(data, kind=11)
        if len(symbols) > 32:
            self.assertGreater(buckets, 1)
        base = section['offset'] + 16
        bucket_at = base + bloom * 8
        chain_at = bucket_at + buckets * 4
        covered = set()
        for bucket in range(buckets):
            index, = struct.unpack_from('<I', data, bucket_at + bucket * 4)
            while index:
                self.assertTrue(first <= index < len(symbols))
                self.assertNotIn(index, covered)
                covered.add(index)
                h = gnu_hash(symbols[index]['name'])
                self.assertEqual(h % buckets, bucket)
                chain, = struct.unpack_from('<I', data, chain_at + (index - first) * 4)
                self.assertEqual(chain & ~1, h & ~1)
                word, = struct.unpack_from('<Q', data, base + ((h // 64) & (bloom - 1)) * 8)
                self.assertTrue(word & (1 << (h % 64)))
                self.assertTrue(word & (1 << ((h >> shift) % 64)))
                if chain & 1:
                    break
                index += 1
        self.assertEqual(covered, set(range(first, len(symbols))))

    def test_build_id_hashes_zeroed_image_deterministically(self):
        obj = self.compile('id-main', 'int main(void){return 42;}')
        for style in ('sha1', 'md5', 'none'):
            for mode in ('static', 'fixed', 'pie', 'shared'):
                flags = ('--build-id='+style,)
                if mode == 'static':
                    make = lambda: self.link('id-static', self.simple, self.answer, options=flags)
                elif mode == 'shared':
                    make = lambda: self.link('id.so', obj, options=('-shared', *flags))
                else:
                    make = lambda: self.hosted('id-hosted', obj, pie=mode == 'pie', extra=flags)
                with self.subTest(style=style, mode=mode):
                    out = make()
                    data = out.read_bytes()
                    self.assertEqual(make().read_bytes(), data)
                    notes = [s for s in core.elf_sections(data) if s['name'] == '.note.gnu.build-id']
                    self.assertEqual(len(notes), int(style != 'none'))
                    self.assertEqual(run('readelf', '-aW', out).stderr, b'')
                    if notes:
                        note = notes[0]
                        at = note['offset']
                        namesz, size, kind = struct.unpack_from('<III', data, at)
                        self.assertEqual((namesz, kind, data[at+12:at+16]), (4, 3, b'GNU\0'))
                        zeroed = bytearray(data)
                        zeroed[at+16:at+16+size] = bytes(size)
                        self.assertEqual(data[at+16:at+16+size], hashlib.new(style, zeroed).digest())
                        self.assertTrue(any(s['kind'] == 4 and s['offset'] <= at and
                                            s['offset'] + s['file_size'] >= at + note['size']
                                            for s in core.elf_segments(data)))
                    if mode != 'shared':
                        run(out, expected=42)
        for flag in ('--build-id=uuid', '--hash-style=unknown'):
            self.rejected(self.simple, 'unsupported', self.answer, options=(flag,))

    def test_hash_styles_versioned_exports_ifunc_and_tls(self):
        source = r'''
__thread int value=7;
static __thread int local=35;
int answer(void){return value+local;}
static int impl(void){return 42;}
static void *resolve(void){return impl;}
int indirect(void) __attribute__((ifunc("resolve")));
int old(void){return 11;} int current(void){return 42;}
__asm__(".symver old,api@V1\n.symver current,api@@V2");
''' + ''.join(f'int function_{n:03d}(void){{return {n};}}\n' for n in range(128))
        obj = self.compile('hash-library', source, flags=('-mtls-dialect=gnu2',))
        policy = self.root/'hash.versions'
        policy.write_text('V1 { global: api; }; V2 { global: api; answer; indirect; function_*; local: *; } V1;')
        main = self.compile('hash-consumer', r'''
#define _GNU_SOURCE
#include <dlfcn.h>
#include <stdio.h>
int main(int argc,char **argv){
 void *h=dlopen(argv[1],RTLD_NOW); if(!h){puts(dlerror());return 1;}
 for(int i=0;i<128;i++){
  char name[64]; snprintf(name,sizeof name,"function_%03d",i);
  int (*f)(void)=dlsym(h,name);if(!f || f()!=i)return 2;
 }
 if(dlsym(h,"not_present_anywhere"))return 3;
 int (*answer)(void)=dlsym(h,"answer"),(*indirect)(void)=dlsym(h,"indirect");
 int (*old)(void)=dlvsym(h,"api","V1"),(*current)(void)=dlsym(h,"api");
 if(!answer||answer()!=42||!indirect||indirect()!=42||!old||old()!=11||!current||current()!=42)return 4;
 return dlclose(h)?5:42;
}
''')
        for style in ('sysv', 'gnu', 'both'):
            with self.subTest(style=style):
                flags = ('--hash-style='+style,)
                lib = self.link('libhash.so', obj, options=('-shared', '--version-script', policy, *flags))
                self.check_hash(lib, style)
                # Fas validates its produced hashes independently as DSO input.
                direct = self.compile('hash-direct', 'extern int answer(void),indirect(void),api(void); int main(void){return answer()==42 && indirect()==42 && api()==42?42:1;}')
                for tool in (core.LINKER, 'ld'):
                    out = self.hosted('hash-direct', direct, lib, linker=tool,
                                      extra=('-rpath', '$ORIGIN', *flags))
                    run(out, expected=42)
                    out = self.hosted('hash-dlopen', main, linker=tool, extra=flags)
                    run(out, lib, expected=42)
                empty = self.assemble('hash-empty', '.text\nnop')
                library = self.link('libempty.so', empty, options=('-shared', *flags))
                self.check_hash(library, style)

    def test_real_compiler_drivers_default_fixed_shared_and_gc(self):
        directory = self.root/'driver-bin'
        directory.mkdir(exist_ok=True)
        driver_linker = directory/'ld'
        if not driver_linker.exists():
            driver_linker.symlink_to(core.LINKER)
        source = self.root/'driver-main.c'
        source.write_text(r'''
#include <stdio.h>
#include <pthread.h>
static __thread int tls=35;
static int initialized;
__attribute__((constructor)) static void init(void){initialized=7;}
static void *worker(void *p){return (void*)(long)(++tls);}
int main(void){pthread_t t;void *v;
 if(pthread_create(&t,0,worker,0)||pthread_join(t,&v)||v!=(void*)36||tls!=35)return 1;
 puts("driver works");return tls+initialized;}
''')
        for compiler in ('gcc', 'clang', 'clang-22'):
            if not shutil.which(compiler):
                continue
            common = [compiler, '-B'+str(directory)+'/', '-O2', '-g', '-pthread']
            if compiler == 'gcc':
                common.append('-fno-use-linker-plugin')
            for flags in ((), ('-no-pie',), ('-ffunction-sections', '-Wl,--gc-sections')):
                with self.subTest(compiler=compiler, flags=flags):
                    output = self.root/'driver-program'
                    run(*common, *flags, source, '-o', output)
                    self.assertEqual(run(output, expected=42).stdout, b'driver works\n')
                    self.check_hash(output, 'gnu')
            output = self.root/'driver-shared.so'
            run(*common, '-fPIC', '-shared', source, '-o', output)
            self.check_hash(output, 'gnu')


if __name__ == '__main__':
    unittest.main(verbosity=2)
