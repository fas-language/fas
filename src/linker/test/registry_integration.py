#!/usr/bin/env python3
"""Native C section registries: layout, GC, archives, partial links and DSOs."""
from pathlib import Path
import shutil
import sys
import unittest

import hosted_integration as hosted

core = hosted.core
run = core.run
REENTRY = str(Path(sys.argv.pop(1)).resolve())


class RegistryTests(unittest.TestCase):
    setUpClass = classmethod(core.LinkerTests.setUpClass.__func__)
    tearDownClass = classmethod(core.LinkerTests.tearDownClass.__func__)
    setUp = hosted.HostedTests.setUp
    assemble = classmethod(core.LinkerTests.assemble.__func__)
    compile = hosted.HostedTests.compile
    crt = hosted.HostedTests.crt
    hosted = hosted.HostedTests.hosted
    link = core.LinkerTests.link
    rejected = core.LinkerTests.rejected

    def tools(self):
        return [core.LINKER, 'ld'] + (['ld.lld-22'] if shutil.which('ld.lld-22') else [])

    def records(self, retained=False):
        result = []
        for name, value in (('a', 13), ('b', 29)):
            result.append(self.compile('registry-' + name, '''
static int callback(void){return ''' + str(value) + ''';}
int dead_''' + name + '''(void){return -1;}
typedef int (*entry)(void);
__attribute__((section("registry"),used''' + (',retain' if retained else '') + '''))
static entry record = callback;
int anchor_''' + name + '''(void){return 0;}
'''))
        return result

    def consumer(self, name='registry-consumer', main=True):
        return self.compile(name, '''
typedef int (*entry)(void);
extern entry __start_registry[], __stop_registry[];
int check(void){
 if(__stop_registry-__start_registry!=2)return 1;
 return __start_registry[0]()+__start_registry[1]();
}
''' + ('int main(void){return check();}\n' if main else ''))

    def test_multiple_objects_callbacks_and_independent_gc_oracles(self):
        first, second = self.records()
        main = self.consumer()
        filler = self.assemble('registry-filler', '.section unrelated,"aw"\n.zero 104')
        for tool in self.tools():
            for gc in (False, True):
                extra = ('-z', 'nostart-stop-gc') if gc and 'lld' in tool else ()
                for pie in (False, True):
                    out = self.hosted('registry-multi', main, first, filler, second,
                                      pie=pie, gc=gc, linker=tool, extra=extra)
                    run(out, expected=42)
                    self.assertEqual(run('readelf', '-aW', out).stderr, b'')
                    if tool == core.LINKER:
                        sections = [s for s in core.elf_sections(out.read_bytes()) if s['name'] == 'registry']
                        self.assertEqual(len(sections), 1)
                        self.assertEqual(sections[0]['size'], 16)
                        symbols = {s['name']: s for s in core.elf_symbols(out.read_bytes())}
                        for name in ('__start_registry', '__stop_registry'):
                            self.assertEqual(symbols[name]['other'] & 3, 2)
                        if gc:
                            self.assertNotIn('dead_a', symbols)
                            self.assertNotIn('dead_b', symbols)
        run(REENTRY, '-pie', '--gc-sections', '--dynamic-linker', hosted.INTERPRETER,
            self.crt('Scrt1.o'), self.crt('crti.o'), self.crt('crtbeginS.o'),
            main, first, filler, second, hosted.LIBC, self.crt('libc_nonshared.a'),
            self.crt('crtendS.o'), self.crt('crtn.o'))

    def test_alignment_nobits_and_empty_sections(self):
        small = self.assemble('registry-small', '.section aligned,"a"\n.balign 8\n'
                              '.globl small\nsmall: .quad 13\n'
                              '.section zeros,"aw",@nobits\n.globl zeroa\nzeroa: .zero 1\n'
                              '.section empty_registry,"a"')
        large = self.assemble('registry-large', '.section aligned,"a"\n.balign 32\n'
                              '.globl large\nlarge: .quad 29\n'
                              '.section zeros,"aw",@nobits\n.balign 16\n'
                              '.globl zerob\nzerob: .zero 3')
        main = self.compile('registry-alignment-main', '''
#include <stdint.h>
extern unsigned char __start_aligned[],__stop_aligned[],small[],large[];
extern unsigned char __start_zeros[],__stop_zeros[],zeroa[],zerob[];
extern unsigned char __start_empty_registry[],__stop_empty_registry[];
int main(void){
 if((uintptr_t)__start_aligned%32 || small!=__start_aligned)return 1;
 if(large-small!=32 || __stop_aligned-small!=40)return 2;
 if(*(long*)small!=13 || *(long*)large!=29)return 3;
 for(int i=8;i<32;i++)if(small[i])return 4;
 if((uintptr_t)__start_zeros%16 || zeroa!=__start_zeros)return 5;
 if(zerob-zeroa!=16 || __stop_zeros-zeroa!=19)return 6;
 for(int i=0;i<19;i++)if(zeroa[i])return 7;
 zerob[2]=42;
 if(__start_empty_registry!=__stop_empty_registry)return 8;
 return zerob[2];
}
''')
        for tool in self.tools():
            extra = ('-z', 'nostart-stop-gc') if 'lld' in tool else ()
            out = self.hosted('registry-aligned', main, small, large, gc=True, linker=tool, extra=extra)
            run(out, expected=42)
            if tool == core.LINKER:
                section = next(s for s in core.elf_sections(out.read_bytes()) if s['name'] == 'zeros')
                self.assertEqual((section['kind'], section['size'], section['align']), (8, 19, 16))

    def test_partial_links_preserve_unresolved_boundaries_for_final_consumer(self):
        main, records = self.consumer(), self.records(retained=True)
        for producer in self.tools():
            partial = self.link('registry-stage1.o', main, records[0], options=('-r',), linker=producer)
            for symbol in core.elf_symbols(partial.read_bytes()):
                if symbol['name'] in ('__start_registry', '__stop_registry'):
                    self.assertEqual(symbol['section'], 0)
            partial = self.link('registry-stage2.o', partial, records[1], options=('-r',), linker=producer)
            for consumer in self.tools():
                out = self.hosted('registry-partial-final', partial, gc=True, linker=consumer)
                run(out, expected=42)

    def test_archive_extraction_is_driven_by_real_symbols_not_boundaries(self):
        records, main = self.records(), self.consumer()
        archive = self.root / 'registry.a'
        run('ar', 'rcs', archive, *records)
        self.rejected(main, 'undefined', archive, options=('-pie',))
        loaded = self.compile('registry-load-archive', '''
extern int anchor_a(void),anchor_b(void),check(void);
int main(void){return anchor_a()+anchor_b()+check();}
''')
        check = self.consumer('registry-check-only', main=False)
        for tool in self.tools():
            extra = ('-z', 'nostart-stop-gc') if 'lld' in tool else ()
            run(self.hosted('registry-extracted', loaded, check, archive, gc=True,
                            linker=tool, extra=extra), expected=42)
            run(self.hosted('registry-whole', main, gc=True, linker=tool,
                            extra=(*extra, '--whole-archive', str(archive), '--no-whole-archive')),
                expected=42)

    def test_explicit_overrides_do_not_retain_registry_and_absent_weak_is_zero(self):
        override = self.assemble('registry-override', '.globl __start_registry,__stop_registry\n'
                                 '.set __start_registry,0x1234\n.set __stop_registry,0x5678')
        main = self.compile('registry-override-main', '''
#include <stdint.h>
extern char __start_registry[],__stop_registry[];
extern char __start_absent[] __attribute__((weak));
int main(void){return (uintptr_t)__start_registry==0x1234 &&
 (uintptr_t)__stop_registry==0x5678 && !__start_absent ? 42:1;}
''')
        out = self.hosted('registry-overridden', main, override, *self.records(), gc=True)
        run(out, expected=42)
        self.assertNotIn('registry', [s['name'] for s in core.elf_sections(out.read_bytes())])
        symbols = {s['name']: s for s in core.elf_symbols(out.read_bytes())}
        self.assertEqual(symbols['__start_registry']['other'] & 3, 0)
        dead = self.compile('registry-dead-boundary', '''
extern char __start_missing[];
void *dead_reference(void){return __start_missing;}
int main(void){return 42;}
''')
        run(self.hosted('registry-dead', dead, gc=True), expected=42)
        self.rejected(dead, 'undefined', options=('-pie',))

    def test_private_dso_boundaries_are_not_preempted_or_exported(self):
        check, records = self.consumer('registry-dso-check', main=False), self.records()
        policy = self.root / 'registry.versions'
        policy.write_text('REGISTRY_1 { global: *; };')
        library = self.link('registry.so', check, *records,
                            options=('-shared', '--gc-sections', '--version-script', str(policy)))
        exports = {s['name'] for s in core.elf_symbols(library.read_bytes(), kind=11)}
        self.assertNotIn('__start_registry', exports)
        self.assertNotIn('__stop_registry', exports)
        loader = self.compile('registry-dso-loader', '''
#include <dlfcn.h>
int __start_registry=-1,__stop_registry=-1;
int main(int argc,char **argv){
 if(argc!=2)return 1;
 void *h=dlopen(argv[1],RTLD_NOW|RTLD_LOCAL);if(!h)return 2;
 int (*check)(void)=dlsym(h,"check");if(!check)return 3;
 int result=check();if(dlclose(h))return 4;return result;
}
''')
        for tool in self.tools():
            host = self.hosted('registry-host', loader, linker=tool, extra=('-E',))
            run(host, library, expected=42)

    def test_invalid_names_tls_and_incompatible_sections_fail_explicitly(self):
        for name in ('__start_missing', '__start_', '__start_bad.name', '__start_1bad'):
            obj = self.assemble('registry-bad-ref', '.text\n.globl answer\nanswer: mov $42,%eax;ret\n'
                                '.data\n.quad ' + name)
            self.rejected(obj, 'undefined', self.simple)
        main = self.consumer()
        for text, error in (('.section registry,"a"\n.quad 1\n', 'incompatible'),
                            ('.section registry,"awT",@progbits\n.quad 1\n', 'non-TLS'),
                            ('.section registry,""\n.quad 1\n', 'allocatable')):
            invalid = self.assemble('registry-bad-input', text)
            self.rejected(main, error, *self.records(), invalid, options=('-pie', '--gc-sections'))

    def test_static_comdat_merge_records_and_explicit_undefined_roots(self):
        start = self.assemble('registry-static-start', '''
.text
.globl _start
_start:
 lea __start_registry(%rip),%rsi
 lea __stop_registry(%rip),%rax
 sub %rsi,%rax
 cmp $8,%rax
 jne 1f
 mov (%rsi),%edi
 add 4(%rsi),%edi
 jmp 2f
1: mov $1,%edi
2: mov $60,%eax
 syscall
''')
        def group(name, value):
            return self.assemble(name, '.section .text.group,"axG",@progbits,signature,comdat\n'
                                 '.globl signature\nsignature: ret\n'
                                 '.section registry,"aG",@progbits,signature,comdat\n'
                                 f'.long {value}\n')
        first, loser = group('registry-group-first', 13), group('registry-group-loser', 99)
        tail = self.assemble('registry-group-tail', '.section registry,"a"\n.long 29')
        for tool in self.tools():
            flags = ('--gc-sections',) + (('-z', 'nostart-stop-gc') if 'lld' in tool else ())
            run(self.link('registry-static', start, first, loser, tail, options=flags,
                          linker=tool), expected=42)
        # Equal mergeable records must still have separate addresses in a registry.
        a = self.assemble('registry-merge-a', '.section registry,"aM",@progbits,4\n.long 21')
        b = self.assemble('registry-merge-b', '.section registry,"aM",@progbits,4\n.long 21')
        run(self.link('registry-merge', start, a, b, options=('--gc-sections',)), expected=42)
        for boundary in ('__start_registry', '__stop_registry'):
            out = self.link('registry-root', self.simple, self.answer, a, b,
                            options=('--gc-sections', '-u', boundary))
            run(out, expected=42)
            section = next(s for s in core.elf_sections(out.read_bytes()) if s['name'] == 'registry')
            self.assertEqual(section['size'], 8)


if __name__ == '__main__':
    unittest.main(verbosity=2)
