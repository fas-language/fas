#!/usr/bin/env python3
"""Real GCC/Clang DWARF contributions, independent readers and debugger checks."""
from pathlib import Path
import shutil
import struct
import subprocess
import unittest

import integration as core

run = core.run
LINKER = core.LINKER


class DebugTests(unittest.TestCase):
    setUpClass = classmethod(core.LinkerTests.setUpClass.__func__)
    tearDownClass = classmethod(core.LinkerTests.tearDownClass.__func__)
    assemble = classmethod(core.LinkerTests.assemble.__func__)
    link = core.LinkerTests.link
    rejected = core.LinkerTests.rejected

    def test_valgrind_multi_object_executable_and_shared_library(self):
        if not shutil.which('valgrind'):
            self.skipTest('Valgrind unavailable; debug-reader acceptance remains unverified')
        tools = self.root / 'debug-gcc-tools'
        tools.mkdir(exist_ok=True)
        (tools / 'ld').symlink_to(LINKER)
        sources = [self.root / ('mapping-' + name + '.c') for name in ('first', 'second', 'main')]
        sources[0].write_text('int first_data=19; int first_bss[17];\n'
            'const int first_rodata=1;\n'
            'int first(void) { return first_data+first_bss[16]+first_rodata; }\n')
        sources[1].write_text('__attribute__((aligned(64))) int second_data=20;\n'
            '__attribute__((aligned(64))) int second_bss[17];\n'
            'const int second_rodata=2;\n'
            'extern int first(void);\n'
            'int answer(void) { return first()+second_data+second_bss[16]+second_rodata; }\n')
        sources[2].write_text('#include <stdio.h>\nextern int answer(void);\n'
            'int main(void) { int value=answer(); printf("%d\\n",value); return value!=42; }\n')
        for function_sections in (False, True):
            with self.subTest(function_sections=function_sections):
                objects = []
                for source in sources:
                    obj = source.with_suffix('.o')
                    run('gcc', '-c', '-O0', '-g', '-fPIC',
                        *(['-ffunction-sections'] if function_sections else []), source, '-o', obj)
                    objects.append(obj)
                library = self.root / 'libmapping.so'
                out = self.root / 'debug-mapping'
                run('gcc', '-B'+str(tools)+'/', '-fno-use-linker-plugin', '-shared',
                    *objects[:2], '-o', library)
                run('gcc', '-B'+str(tools)+'/', '-fno-use-linker-plugin', objects[2],
                    library, '-Wl,-rpath,'+str(self.root), '-o', out)
                self.assertEqual(run(out).stdout, b'42\n')
                for image in (out, library):
                    sections = core.elf_sections(image.read_bytes())
                    for name in ('.text', '.data', '.bss', '.rodata'):
                        self.assertEqual(len([s for s in sections if s['name'] == name]), 1)
                    self.assertEqual(run('readelf', '-aW', image).stderr, b'')
                sections = core.elf_sections(library.read_bytes())
                symbols = {s['name']: s for s in core.elf_symbols(library.read_bytes())}
                for name in ('second_data', 'second_bss'):
                    self.assertEqual(symbols[name]['value'] % 64, 0)
                text = next(s for s in sections if s['name'] == '.text')
                for name in ('first', 'answer'):
                    self.assertEqual(symbols[name]['section'], text['index'])
                    self.assertIn(('mapping-'+('first' if name == 'first' else 'second')+'.c').encode(),
                        run('addr2line', '-e', library, hex(symbols[name]['value'])).stdout)
                if shutil.which('valgrind'):
                    report = self.root / 'mapping-valgrind.log'
                    p = subprocess.run(['valgrind', '--verbose', '--error-exitcode=99',
                        '--leak-check=full', '--read-var-info=yes', '--log-file='+str(report),
                        str(out)], capture_output=True, timeout=30)
                    log = report.read_text()
                    self.assertEqual(p.returncode, 0, log)
                    self.assertEqual(p.stdout, b'42\n')
                    self.assertIn('libmapping.so', log)
                    self.assertIn('ERROR SUMMARY: 0 errors', log)
                    self.assertNotIn('Serious problem when reading debug info', log)
                    self.assertNotIn("Can't make sense", log)

    def build(self, compiler, version, gc=False):
        first = self.root / 'debug-first.c'
        first.write_text('int bias = 40;\nint helper(int value) {\n  return value + bias;\n}\n')
        second = self.root / 'debug-second.c'
        second.write_text('extern int helper(int);\nint answer(void) {\n  int input = 2;\n  return helper(input);\n}\nint dead(void) { return 9; }\n')
        objects = []
        for source in (first, second):
            obj = source.with_suffix('.o')
            run(compiler, '-c', '-O0', '-g', '-gdwarf-' + str(version), '-ffunction-sections', '-fdata-sections',
                '-fPIC', '-ffreestanding', '-fno-stack-protector', '-fno-asynchronous-unwind-tables',
                '-fno-unwind-tables', '-fcf-protection=none', '-o', obj, source)
            objects.append(obj)
        return self.link('debug-program', self.simple, *objects, options=('--gc-sections',) if gc else ())

    def test_multi_object_dwarf_and_source_addresses(self):
        compilers = ['gcc']
        clang = shutil.which('clang-22') or shutil.which('clang')
        if clang:
            compilers.append(clang)
        for compiler in compilers:
            for version in (4, 5):
                for gc in (False, True):
                    with self.subTest(compiler=compiler, version=version, gc=gc):
                        out = self.build(compiler, version, gc)
                        run(out, expected=42)
                        sections = core.elf_sections(out.read_bytes())
                        for name in ('.debug_info', '.debug_abbrev', '.debug_line', '.debug_str'):
                            self.assertEqual(len([s for s in sections if s['name'] == name]), 1)
                        report = run('readelf', '--debug-dump=info,decodedline,frames', out)
                        self.assertEqual(report.stderr, b'')
                        self.assertIn(b'debug-first.c', report.stdout)
                        self.assertIn(b'debug-second.c', report.stdout)
                        syms = {s['name']: s for s in core.elf_symbols(out.read_bytes())}
                        for name, file in (('helper', 'debug-first.c'), ('answer', 'debug-second.c')):
                            line = run('addr2line', '-e', out, '-f', hex(syms[name]['value'])).stdout
                            self.assertIn(name.encode(), line)
                            self.assertIn(file.encode(), line)
                        self.assertEqual('dead' in syms, not gc)

    def test_gdb_breakpoints_locals_and_backtrace(self):
        if not shutil.which('gdb'):
            self.skipTest('gdb unavailable; debugger acceptance gate remains unverified')
        out = self.build('gcc', 5, gc=True)
        p = subprocess.run(['gdb', '-nx', '-batch', '-ex', 'set debuginfod enabled off',
            '-ex', 'set disable-randomization off', '-ex', 'break debug-first.c:3', '-ex', 'run',
            '-ex', 'print value', '-ex', 'backtrace', '-ex', 'continue', str(out)], capture_output=True, timeout=20)
        text = p.stdout + p.stderr
        if b'ptrace: Operation not permitted' in text:
            self.skipTest('ptrace unavailable; runtime debugger gate remains unverified')
        self.assertEqual(p.returncode, 0, text.decode(errors='replace'))
        self.assertIn(b'Breakpoint 1, helper (value=2)', text)
        self.assertIn(b'$1 = 2', text)
        self.assertIn(b'answer () at', text)
        self.assertNotIn(b'Dwarf Error', text)

    def test_addrsig_is_validated_and_consumed(self):
        clang = shutil.which('clang-22') or shutil.which('clang')
        if not clang:
            self.skipTest('Clang unavailable')
        source = self.root / 'addrsig.c'
        source.write_text('int answer(void) { return 42; }\nvoid *pointer = answer;\n')
        obj = source.with_suffix('.o')
        run(clang, '-c', '-O0', '-fPIC', '-fno-asynchronous-unwind-tables', '-fno-unwind-tables',
            '-o', obj, source)
        out = self.link('addrsig-out', self.simple, obj)
        run(out, expected=42)
        self.assertNotIn('.llvm_addrsig', [s['name'] for s in core.elf_sections(out.read_bytes())])
        original = obj.read_bytes()
        section = next(s for s in core.elf_sections(original) if s['kind'] == 0x6fff4c03)
        self.assertGreater(section['size'], 0)
        for label, offset, fmt, value in (
            ('bad-index', section['offset'], 'B', 127),
            ('unterminated', section['offset'] + section['size'] - 1, 'B', 128),
            ('bad-link', section['header'] + 40, 'I', 1)):
            data = bytearray(original)
            struct.pack_into('<' + fmt, data, offset, value)
            path = self.root / ('addrsig-' + label + '.o')
            path.write_bytes(data)
            self.rejected(self.simple, 'address-significance', path)
        # GNU/LLD -r set sh_link=0 when symbol indices have become stale.
        data = bytearray(original)
        struct.pack_into('<I', data, section['header'] + 40, 0)
        data[section['offset']:section['offset']+section['size']] = b'\xff'*section['size']
        path = self.root/'addrsig-invalidated.o'
        path.write_bytes(data)
        run(self.link('addrsig-invalidated',self.simple,path),expected=42)


if __name__ == '__main__':
    unittest.main(verbosity=2)
