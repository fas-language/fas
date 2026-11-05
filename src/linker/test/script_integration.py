#!/usr/bin/env python3
"""Bounded implicit sysroot scripts, archive semantics and alias protection."""
import os
from pathlib import Path
import random
import subprocess
import unittest

import hosted_integration as hosted

core = hosted.core
run = core.run
LINKER = core.LINKER


class ScriptTests(unittest.TestCase):
    setUpClass = hosted.HostedTests.__dict__['setUpClass']
    tearDownClass = hosted.HostedTests.__dict__['tearDownClass']
    assemble = hosted.HostedTests.__dict__['assemble']
    setUp = hosted.HostedTests.setUp
    link = core.LinkerTests.link
    rejected = core.LinkerTests.rejected
    compile = hosted.HostedTests.compile
    crt = hosted.HostedTests.crt

    def script(self, name, text):
        path = self.root / name
        path.write_text(text)
        return path

    def test_system_libc_script_startup_tls_and_shutdown(self):
        libc = Path('/usr/lib/x86_64-linux-gnu/libc.so')
        if not libc.is_file():
            self.skipTest('pinned libc linker script unavailable')
        obj = self.compile('script-hosted', r'''
#include <stdio.h>
#include <stdlib.h>
__thread int n=42;
static void finish(void){puts("shutdown");}
int main(void){if(atexit(finish))return 1;printf("%d\n",n);return n;}
''')
        for linker in (LINKER,'ld'):
            out = self.link('script-hosted',self.crt('Scrt1.o'),self.crt('crti.o'),
                            self.crt('crtbeginS.o'),obj, '-lc',self.crt('crtendS.o'),
                            self.crt('crtn.o'),linker=linker,
                            options=('-pie','--dynamic-linker',hosted.INTERPRETER,
                                     '--gc-sections','-L',libc.parent))
            self.assertEqual(run(out,expected=42).stdout,b'42\nshutdown\n')
            self.assertEqual(run('readelf','-aW',out).stderr,b'')

    def test_nested_relative_quoted_inputs_library_search_and_groups(self):
        directory = self.root / 'script inputs'
        directory.mkdir()
        last = self.assemble('script-last','.text\n.globl last\nlast: mov $42,%eax; ret')
        middle = self.assemble('script-middle','.text\n.globl answer\nanswer: jmp last')
        one = directory/'libone.a'; two = directory/'libtwo.a'
        run('ar','rcs',one,last);run('ar','rcs',two,middle)
        inner = self.script('script inputs/inner.ld', '/* embedded filenames */\n'
                            'GROUP ( libone.a "libtwo.a" )\n')
        outer = self.script('outer.ld', 'OUTPUT_FORMAT("elf64-x86-64");\n'
                            'INPUT ( "script inputs/inner.ld" )')
        for linker in (LINKER,'ld'):
            run(self.link('script-nested',self.simple,outer,linker=linker),expected=42)
        search = self.script('search.ld','GROUP(-lone -ltwo)')
        for linker in (LINKER,'ld'):
            run(self.link('script-search',self.simple,search,
                          options=('-L',directory),linker=linker),expected=42)
        sequence = self.script('sequence.ld','INPUT("script inputs/libone.a" "script inputs/libtwo.a")')
        self.rejected(self.simple,'undefined symbol last',sequence)

    def test_as_needed_weak_order_and_group_reconsideration(self):
        source = self.root/'script-library.c'
        source.write_text('int answer(void){return 42;}')
        library = self.root/'libscript-needed.so'
        run('gcc','-nostdlib','-shared','-fPIC',source,'-o',library,
            '-Wl,-soname,'+library.name)
        script = self.script('asneeded.ld','INPUT(AS_NEEDED(libscript-needed.so))')
        no_call = self.assemble('script-no-call','.text\n.globl _start\n_start: mov $42,%edi;mov $60,%eax;syscall')
        weak = self.assemble('script-weak','.weak answer\n.data\n.quad answer')
        options = ('-pie','--dynamic-linker',hosted.INTERPRETER,'-rpath','$ORIGIN')
        for linker in (LINKER,'ld'):
            for objects, needed in (((no_call,script),False),((no_call,weak,script),False),
                                     ((self.simple,script),True)):
                out = self.link('script-asneeded',*objects,options=options,linker=linker)
                run(out,expected=42)
                self.assertEqual(b'NEEDED' in run('readelf','-dW',out).stdout,needed)
        caller = self.assemble('script-caller','.text\n.globl call_answer\ncall_answer: jmp answer')
        start = self.assemble('script-group-start','.text\n.globl _start\n_start: call call_answer;mov %eax,%edi;mov $60,%eax;syscall')
        archive = self.root/'libscript-caller.a';run('ar','rcs',archive,caller)
        group = self.script('reconsider.ld','GROUP(AS_NEEDED(libscript-needed.so) libscript-caller.a)')
        for linker in (LINKER,'ld'):
            run(self.link('script-reconsider',start,group,options=options,linker=linker),expected=42)

    def test_nested_inputs_cannot_be_overwritten_by_executable_or_map(self):
        script = self.script('alias-input.ld', 'INPUT(answer.o)')
        original = self.answer.read_bytes()
        alias = self.root/'script-alias'
        os.link(self.answer,alias)
        for flags in (('-o',alias),('-o',self.root/'alias-exe','-Map',alias)):
            result = run(LINKER,*flags,self.simple,script,expected=1)
            self.assertIn(b'overwrite input',result.stderr)
            self.assertEqual(self.answer.read_bytes(),original)
        self.assertEqual(list(self.root.glob('.fas-ld-*.tmp')),[])

    def test_malformed_unsupported_cycles_and_seeded_mutations(self):
        for index,(text,message) in enumerate((
            ('INPUT(answer.o','missing closing'),('INPUT("answer.o)','unterminated'),
            ('/* unclosed','unterminated comment'),('SECTIONS {}','unsupported command'),
            ('OUTPUT_FORMAT(elf32-i386)','only OUTPUT_FORMAT'),
            ('INPUT(missing.o)','cannot find script input'),
            ('INPUT(=missing-sysroot-answer.o)','cannot find script input'),
            ('INPUT(AS_NEEDED('+('AS_NEEDED('*17)+'answer.o'+(')'*19)+')','nesting'))):
            self.rejected(self.simple,message,self.script(f'bad-script-{index}.ld',text))
        cycle = self.script('cycle.ld','INPUT(cycle.ld)')
        self.rejected(self.simple,'cyclic linker script',cycle)
        nested = 'answer.o'
        for n in range(18):
            nested = self.script(f'depth-{n}.ld',f'INPUT({Path(nested).name})').name
        self.rejected(self.simple,'nesting exceeds',self.root/nested)
        valid = b'OUTPUT_FORMAT(elf64-x86-64)\nGROUP(answer.o AS_NEEDED(answer.o))\n'
        rng = random.Random(0x5C71)
        for n in range(128):
            data = bytearray(valid)
            for _ in range(rng.randrange(1,5)):
                data[rng.randrange(len(data))] = rng.randrange(256)
            path = self.root/'mutation.ld';path.write_bytes(data)
            result = subprocess.run([LINKER,'-o',str(self.root/'mutation-output'),str(self.simple),str(path)],
                                    capture_output=True,timeout=5)
            self.assertIn(result.returncode,(0,1),result.stderr)
            self.assertNotIn(b'Fatal error',result.stderr)


if __name__ == '__main__':
    unittest.main(verbosity=2)
