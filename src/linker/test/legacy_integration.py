#!/usr/bin/env python3
"""Legacy lifecycle lists are diagnosed on final links and preserved by -r."""
import shutil
import unittest

import hosted_integration as hosted

core = hosted.core
run = core.run


class LegacyTests(unittest.TestCase):
    setUpClass = classmethod(core.LinkerTests.setUpClass.__func__)
    tearDownClass = classmethod(core.LinkerTests.tearDownClass.__func__)
    setUp = hosted.HostedTests.setUp
    assemble = classmethod(core.LinkerTests.assemble.__func__)
    compile = hosted.HostedTests.compile
    crt = hosted.HostedTests.crt
    hosted = hosted.HostedTests.hosted
    rejected = core.LinkerTests.rejected
    link = core.LinkerTests.link

    def test_real_compiler_lists_fail_before_gc_and_modern_arrays_still_run(self):
        clang = shutil.which('clang-22') or shutil.which('clang')
        if not clang:
            self.skipTest('Clang unavailable; legacy compiler fixture unverified')
        source = '''#include <stdio.h>
static volatile int state;
__attribute__((constructor(101))) static void init(void){state=42;puts("init");}
__attribute__((destructor(101))) static void fini(void){puts("fini");}
int main(void){puts("main");return state==42?42:1;}
'''
        legacy = self.compile('legacy-real', source, compiler=clang,
                              flags=('-fno-use-init-array',))
        modern = self.compile('modern-real', source, compiler=clang)
        for gc in (False, True):
            flags = ('--gc-sections',) if gc else ()
            for mode in ((), ('-pie',), ('-shared',)):
                self.rejected(legacy, 'legacy lifecycle section', options=(*mode, *flags))
            for pie in (False, True):
                for tool, obj in ((core.LINKER, modern), ('ld', legacy)):
                    out = self.hosted('legacy-control', obj, pie=pie, gc=gc, linker=tool)
                    self.assertEqual(run(out, expected=42).stdout, b'init\nmain\nfini\n')

    def test_legacy_partial_output_remains_usable_by_independent_final_linker(self):
        clang = shutil.which('clang-22') or shutil.which('clang')
        if not clang:
            self.skipTest('Clang unavailable; legacy compiler fixture unverified')
        obj = self.compile('legacy-partial', '''#include <stdio.h>
__attribute__((constructor)) static void init(void){puts("init");}
__attribute__((destructor)) static void fini(void){puts("fini");}
int main(void){puts("main");return 42;}
''', compiler=clang, flags=('-fno-use-init-array',))
        partial = self.link('legacy-partial-combined.o', obj, options=('-r',))
        partial = self.link('legacy-partial-twice.o', partial, options=('-r',))
        self.assertEqual(run('readelf', '-aW', partial).stderr, b'')
        out = self.hosted('legacy-independent', partial, gc=True, linker='ld')
        self.assertEqual(run(out, expected=42).stdout, b'init\nmain\nfini\n')
        self.rejected(partial, 'legacy lifecycle section', options=('-pie',))

    def test_destructor_only_lists_and_section_name_boundaries(self):
        for name in ('.ctors', '.ctors.65434', '.dtors', '.dtors.65535'):
            obj = self.assemble('legacy-reserved', '.section ' + name + ',"aw"\n.quad 0')
            self.rejected(obj, 'legacy lifecycle section', self.simple, self.answer)
        ordinary = self.assemble('legacy-ordinary',
            '.section .ctors_extra,"aw"\n.quad 0\n.section .dtors,"aw"')
        run(self.link('legacy-unrelated', self.simple, self.answer, ordinary), expected=42)


if __name__ == '__main__':
    unittest.main(verbosity=2)
