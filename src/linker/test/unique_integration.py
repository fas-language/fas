#!/usr/bin/env python3
"""GNU unique DSO inputs and loader identity across shared-library boundaries."""
import shutil
import struct
import unittest

import integration as core
import hosted_integration as hosted

run = core.run


class UniqueTests(unittest.TestCase):
    setUpClass = classmethod(core.LinkerTests.setUpClass.__func__)
    tearDownClass = classmethod(core.LinkerTests.tearDownClass.__func__)
    assemble = classmethod(core.LinkerTests.assemble.__func__)
    link = core.LinkerTests.link
    rejected = core.LinkerTests.rejected
    setUp = hosted.HostedTests.setUp
    compile = hosted.HostedTests.compile
    crt = hosted.HostedTests.crt
    hosted = hosted.HostedTests.hosted

    def libraries(self):
        result = []
        for suffix, value in (('a', 35), ('b', 99)):
            obj = self.assemble('unique-'+suffix, f'''
.data
.globl shared_unique
.type shared_unique,@gnu_unique_object
.size shared_unique,4
shared_unique: .long {value}
.text
.globl get_unique_{suffix}
.type get_unique_{suffix},@function
get_unique_{suffix}: mov shared_unique@GOTPCREL(%rip),%rax; ret
''')
            out = self.link('libunique_'+suffix+'.so', obj, linker='ld',
                            options=('-shared', '-soname', 'libunique_'+suffix+'.so'))
            symbol = next(s for s in core.elf_symbols(out.read_bytes(), kind=11)
                          if s['name']=='shared_unique')
            self.assertEqual(symbol['info'] >> 4, 10)
            result.append(out)
        return result

    def test_direct_imports_remain_undefined_global_and_keep_loader_identity(self):
        libraries = self.libraries()
        start = self.assemble('unique-start', r'''
.text
.globl _start
_start:
 call get_unique_a
 mov %rax,%rbx
 call get_unique_b
 cmp %rbx,%rax
 jne 1f
 mov shared_unique@GOTPCREL(%rip),%rcx
 cmp %rcx,%rax
 jne 1f
 mov (%rax),%edi
 add $7,%edi
 jmp 2f
1: mov $88,%edi
2: mov $60,%eax
 syscall
''')
        tools = [core.LINKER, 'ld'] + (['ld.lld-22'] if shutil.which('ld.lld-22') else [])
        for tool in tools:
            for reverse in (False, True):
                with self.subTest(tool=tool, reverse=reverse):
                    out = self.link('unique-consumer', start,
                                    *(libraries[::-1] if reverse else libraries), linker=tool,
                                    options=('-pie', '--dynamic-linker', hosted.INTERPRETER,
                                             '-rpath', '$ORIGIN', '--no-allow-shlib-undefined'))
                    run(out, expected=106 if reverse else 42)
                    symbol = next(s for s in core.elf_symbols(out.read_bytes(), kind=11)
                                  if s['name']=='shared_unique')
                    self.assertEqual(symbol['info'] >> 4, 1)
                    self.assertEqual(symbol['section'], 0)
        # GNU-unique definitions in ET_REL still require resolver/output support.
        obj = self.root/'unique-a.o'
        self.rejected(obj, 'binding', self.simple, self.answer)

    def test_local_dlopen_identity_and_invalid_unique_undefined_symbol(self):
        a, b = self.libraries()
        obj = self.compile('unique-local-loader', r'''
#include <dlfcn.h>
int main(int argc,char **argv){
 if(argc!=3)return 1;
 void *a=dlopen(argv[1],RTLD_NOW|RTLD_LOCAL),*b=dlopen(argv[2],RTLD_NOW|RTLD_LOCAL);
 if(!a||!b)return 2;
 int *(*one)(void)=(int *(*)(void))dlsym(a,"get_unique_a");
 int *(*two)(void)=(int *(*)(void))dlsym(b,"get_unique_b");
 if(!one||!two||one()!=two()||*one()!=35)return 3;
 dlclose(a);dlclose(b);return 42;
}
''')
        for tool in (core.LINKER, 'ld'):
            out = self.hosted('unique-local-loader', obj, linker=tool)
            run(out, a, b, expected=42)
        data = bytearray(a.read_bytes())
        symbol = next(s for s in core.elf_symbols(data, kind=11) if s['name']=='shared_unique')
        struct.pack_into('<H', data, symbol['offset']+6, 0)
        a.write_bytes(data)
        self.rejected(self.simple, 'undefined GNU unique symbol', a, self.answer,
                      options=('-pie',))


if __name__ == '__main__':
    unittest.main(verbosity=2)
