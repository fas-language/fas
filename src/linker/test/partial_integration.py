#!/usr/bin/env python3
"""ET_REL combination, independent final consumers and preserved link semantics."""
from pathlib import Path
import shutil
import sys
import unittest

REENTRY = str(Path(sys.argv.pop(3)).resolve())
import hosted_integration as fixtures

core = fixtures.core
run = core.run


class PartialTests(unittest.TestCase):
    setUpClass = classmethod(core.LinkerTests.setUpClass.__func__)
    tearDownClass = classmethod(core.LinkerTests.tearDownClass.__func__)
    assemble = classmethod(core.LinkerTests.assemble.__func__)
    compile = fixtures.HostedTests.compile
    crt = fixtures.HostedTests.crt
    hosted = fixtures.HostedTests.hosted
    link = core.LinkerTests.link
    rejected = core.LinkerTests.rejected

    def partial(self, name, *objects, tool=core.LINKER, flags=()):
        out = self.root/(name+'.o')
        run(tool, '-r', *flags, '-o', out, *objects)
        data = out.read_bytes()
        self.assertEqual(int.from_bytes(data[16:18], 'little'), 1)
        self.assertEqual(int.from_bytes(data[24:40], 'little'), 0)
        self.assertEqual(run('readelf', '-aW', out).stderr, b'')
        return out

    def tools(self):
        return [core.LINKER, 'ld'] + (['ld.lld-22'] if shutil.which('ld.lld-22') else [])

    def test_unresolved_local_absolute_common_and_multistage_links(self):
        one = self.assemble('partial-one', r'''
.text
.globl answer
answer: call helper
 add local(%rip),%eax
 add common(%rip),%eax
 ret
.data
local: .long 7
.comm common,8,64
.weak optional
.quad optional
.hidden future_hidden
.quad future_hidden
.globl end
.quad end
''')
        two = self.assemble('partial-two', r'''
.text
.globl helper
helper: mov $35,%eax;ret
.data
local: .quad 99
.comm common,32,16
.hidden future_hidden
.globl future_hidden
future_hidden: .quad 0
''')
        for producer in self.tools():
            with self.subTest(producer=producer):
                first = self.partial('partial-first', one, tool=producer)
                symbols = core.elf_symbols(first.read_bytes())
                common = next(s for s in symbols if s['name'] == 'common')
                self.assertEqual((common['section'], common['value'], common['size']), (0xfff2,64,8))
                self.assertEqual(next(s for s in symbols if s['name'] == 'end')['section'], 0)
                merged = self.partial('partial-merged', first, two, tool=producer)
                common = next(s for s in core.elf_symbols(merged.read_bytes()) if s['name'] == 'common')
                self.assertEqual((common['section'], common['value'], common['size']), (0xfff2,64,32))
                self.assertEqual(sum(s['name'] == 'local' for s in core.elf_symbols(merged.read_bytes())), 2)
                for consumer in self.tools():
                    out = self.link('partial-final', self.simple, merged, linker=consumer)
                    run(out, expected=42)
        # Common remains overridable by a later regular definition.
        merged = self.partial('partial-common', one, two)
        override = self.assemble('partial-override', '.globl common\n.data\ncommon: .long 1')
        for consumer in self.tools():
            run(self.link('partial-overridden', self.simple, merged, override, linker=consumer), expected=43)

    def test_comdat_groups_relocations_and_final_gc(self):
        def candidate(name, value):
            return self.assemble(name, '.section .text.answer,"axG",@progbits,sig,comdat\n'
                '.globl answer\nanswer: mov local(%rip),%eax;ret\n'
                '.section .rodata.answer,"aG",@progbits,sig,comdat\n'
                f'local: .long {value}\n')
        a, b = candidate('partial-group-a',42), candidate('partial-group-b',7)
        for producer in self.tools():
            first = self.partial('partial-group-one', a, tool=producer)
            for inputs, expected in (((first,b),42), ((b,first),7)):
                merged = self.partial('partial-group-both', *inputs, tool=producer)
                sections = core.elf_sections(merged.read_bytes())
                self.assertEqual(sum(s['kind'] == 17 for s in sections),1)
                for consumer in self.tools():
                    run(self.link('partial-group-final', self.simple, merged, linker=consumer,
                                  options=('--gc-sections',)), expected=expected)

    def test_archives_roots_thin_members_and_final_archive_extraction(self):
        library = self.root/'partial-lib.a'
        run('ar','rcsT',library,self.answer)
        # There is no implicit _start extraction root in relocatable mode.
        startlib = self.root/'partial-start.a'
        run('ar','rcs',startlib,self.simple,self.answer)
        unselected = self.partial('partial-unselected', startlib)
        self.assertFalse(any(s['name'] == '_start' for s in core.elf_symbols(unselected.read_bytes())))
        for producer in self.tools():
            merged = self.partial('partial-archive', self.simple,library,tool=producer)
            run(self.link('partial-archive-exe',merged),expected=42)
            rooted = self.partial('partial-root',startlib,tool=producer,flags=('-u','_start'))
            run(self.link('partial-root-exe',rooted),expected=42)
        combined = self.partial('partial-answer',self.answer)
        library = self.root/'partial-combined-lib.a'
        run('ar','rcs',library,combined)
        run(self.link('partial-extracted-later',self.simple,library),expected=42)

    def test_empty_and_unselected_archives_emit_reusable_empty_objects(self):
        empty, unselected, thin = (self.root/name for name in
                                  ('partial-empty.a', 'partial-unselected.a', 'partial-unselected-thin.a'))
        run('ar', 'rcs', empty)
        run('ar', 'rcs', unselected, self.simple, self.answer)
        run('ar', 'rcsT', thin, self.simple, self.answer)
        for archive in (empty, unselected, thin):
            for producer in self.tools():
                with self.subTest(archive=archive.name, producer=producer):
                    # Empty archives carry no target hint for LLD. The initial
                    # Fas target is always x86-64; use explicit emulation for all.
                    merged = self.partial('partial-empty-output', archive, tool=producer,
                                          flags=('-m', 'elf_x86_64'))
                    self.assertFalse(any(s['name'] for s in core.elf_symbols(merged.read_bytes())))
                    again = self.partial('partial-empty-again', merged)
                    for consumer in self.tools():
                        run(self.link('partial-empty-final', again, self.simple, self.answer,
                                      linker=consumer), expected=42)
        self.assertIn(b'domain reentry', run(REENTRY, '-r', empty).stdout)
        out = self.root/'partial-no-input'
        for flags in ((), ('-u', 'hook'), ('--start-group', '--end-group')):
            out.write_bytes(b'previous output')
            p = run(core.LINKER, '-r', '-o', out, *flags, expected=1)
            self.assertIn(b'no input files', p.stderr)
            self.assertEqual(out.read_bytes(), b'previous output')
        original = empty.read_bytes()
        run(core.LINKER, '-r', '-o', empty, empty, expected=1)
        self.assertEqual(empty.read_bytes(), original)

    def test_undefined_roots_survive_partial_stages_and_select_later_archives(self):
        empty = self.root/'partial-roots-empty.a'
        run('ar', 'rcs', empty)
        filler = self.assemble('partial-root-filler', '.data\nfiller: .long 7')
        provider = self.assemble('partial-later-provider',
            '.text\n.globl hook,zed\nhook: ret\nzed: ret')
        library = self.root/'partial-later-provider.a'
        run('ar', 'rcs', library, provider)
        # GNU retains undeclared -u roots in ET_REL. LLD 22 drops those roots;
        # this test deliberately qualifies Fas against GNU's preservation policy.
        for producer in (core.LINKER, 'ld'):
            for initial in (empty, filler):
                with self.subTest(producer=producer, initial=initial.name):
                    merged = self.partial('partial-forced-roots', initial, tool=producer,
                                          flags=('-u', 'zed', '-u', 'hook', '-u', 'zed'))
                    symbols = core.elf_symbols(merged.read_bytes())
                    for name in ('hook', 'zed'):
                        matches = [s for s in symbols if s['name'] == name]
                        self.assertEqual(len(matches), 1)
                        self.assertEqual((matches[0]['section'], matches[0]['info'] >> 4), (0, 1))
                    second = self.partial('partial-forced-roots-again', merged)
                    for consumer in self.tools():
                        out = self.link('partial-forced-roots-final', self.simple, self.answer,
                                        second, library, linker=consumer)
                        run(out, expected=42)
                        names = {s['name']: s for s in core.elf_symbols(out.read_bytes())}
                        self.assertNotEqual(names['hook']['section'], 0)
                        self.assertNotEqual(names['zed']['section'], 0)
        first = self.partial('partial-root-order-one', filler, flags=('-u','hook','-u','zed'))
        second = self.partial('partial-root-order-two', filler, flags=('-u','zed','-u','hook'))
        self.assertEqual(first.read_bytes(), second.read_bytes())
        weak = self.assemble('partial-root-weak', '.weak hook\n.data\n.quad hook')
        upgraded = self.partial('partial-root-upgraded', weak, flags=('-u', 'hook'))
        hook = next(s for s in core.elf_symbols(upgraded.read_bytes()) if s['name'] == 'hook')
        self.assertEqual(hook['info'] >> 4, 1)
        defined = self.partial('partial-root-defined', self.answer, flags=('-u', 'answer'))
        self.assertEqual(sum(s['name'] == 'answer' for s in core.elf_symbols(defined.read_bytes())), 1)

    def test_debug_initialization_tls_addrsig_and_independent_hosted_consumers(self):
        for compiler in ('gcc','clang'):
            if not shutil.which(compiler):
                continue
            one = self.compile('partial-hosted-one', r'''
#include <stdio.h>
extern int initialized,other(void);
__thread int tls=7;
__attribute__((constructor(101))) static void init(void){initialized=35;}
int answer(void){return other()+tls;}
''', compiler=compiler, flags=('-gz=zlib',))
            two = self.compile('partial-hosted-two', r'''
#include <stdio.h>
int initialized; int other(void){return initialized;}
extern int answer(void);
__attribute__((destructor)) static void done(void){puts("done");}
int main(void){puts("partial works");return answer();}
''', compiler=compiler)
            for producer in self.tools():
                merged = self.partial('partial-hosted',one,two,tool=producer)
                if compiler == 'clang' and producer == core.LINKER:
                    self.assertTrue(any(s['kind']==0x6fff4c03 for s in core.elf_sections(merged.read_bytes())))
                for consumer in self.tools():
                    out = self.hosted('partial-hosted-exe',merged,fixtures.INTERPRETER,linker=consumer,gc=True)
                    self.assertEqual(run(out,expected=42).stdout,b'partial works\ndone\n')
                    self.assertIn(b'partial-hosted-one.c',run('readelf','--debug-dump=decodedline',out).stdout)

    def test_versions_ifunc_and_shared_final_output(self):
        obj = self.compile('partial-versions', r'''
static int impl(void){return 42;}
static void *resolve(void){return impl;}
int indirect(void) __attribute__((ifunc("resolve")));
int old(void){return 7;} int current(void){return 42;}
__asm__(".symver old,api@V1,remove\n.symver current,api@@V2,remove");
''')
        merged = self.partial('partial-versions-merged',obj)
        policy = self.root/'partial.versions'
        policy.write_text('V1 {}; V2 {} V1;')
        main = self.compile('partial-version-main','extern int indirect(void),api(void);int main(void){return indirect()==42 && api()==42?42:1;}')
        for producer in self.tools():
            lib = self.link('libpartial.so',merged,linker=producer,
                            options=('-shared','--version-script',policy))
            for consumer in self.tools():
                run(self.hosted('partial-version-exe',main,lib,linker=consumer,
                                extra=('-rpath','$ORIGIN')),expected=42)

    def test_mixed_addrsig_and_absent_tables_preserve_address_identity(self):
        if not shutil.which('clang'):
            self.skipTest('Clang unavailable')
        gcc = self.compile('partial-identity-gcc', '''
int gcc_a(void){return 7;} int gcc_b(void){return 7;}
int (*volatile a)(void)=gcc_a,(*volatile b)(void)=gcc_b;
int answer(void){return a!=b?42:1;}
''')
        clang = self.compile('partial-identity-clang', 'int filler(void){return 7;}',compiler='clang')
        merged = self.partial('partial-identity',gcc,clang)
        data = merged.read_bytes()
        symbols = core.elf_symbols(data)
        section = next(s for s in core.elf_sections(data) if s['kind']==0x6fff4c03)
        marked = set()
        value = shift = 0
        for byte in data[section['offset']:section['offset']+section['size']]:
            value |= (byte & 127) << shift
            shift += 7
            if not byte & 128:
                marked.add(symbols[value]['name'])
                value = shift = 0
        self.assertTrue({'gcc_a','gcc_b','a','b'} <= marked)
        if shutil.which('ld.lld-22'):
            out = self.link('partial-identity-final',self.simple,merged,
                            linker='ld.lld-22',options=('--icf=safe',))
            run(out,expected=42)

    def test_atomic_failures_permissions_reproducibility_and_api_reentry(self):
        merged = self.partial('partial-repeat',self.simple,self.answer)
        expected = merged.read_bytes()
        self.assertEqual(self.partial('partial-repeat',self.simple,self.answer).read_bytes(),expected)
        self.assertEqual(merged.stat().st_mode & 0o777,0o644)
        self.assertIn(b'domain reentry',run(REENTRY,'-r',self.simple,self.answer).stdout)
        for flags in (('-r','-pie'),('-r','--gc-sections'),('-r','--build-id'),('-r','-Map','map.txt')):
            self.rejected(self.simple,'-r cannot be combined',self.answer,options=flags)
        self.rejected(self.simple,'duplicate symbol answer',self.answer,self.answer,options=('-r',))
        original = self.answer.read_bytes()
        run(core.LINKER,'-r','-o',self.answer,self.simple,self.answer,expected=1)
        self.assertEqual(self.answer.read_bytes(),original)


if __name__ == '__main__':
    unittest.main(verbosity=2)
