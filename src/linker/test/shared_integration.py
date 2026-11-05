#!/usr/bin/env python3
"""Independent DSO producers, runtime imports/versions, and malformed metadata."""
import os
from pathlib import Path
import random
import struct
import subprocess
import unittest

import integration as core

run = core.run
LINKER = core.LINKER
INTERPRETER = '/lib64/ld-linux-x86-64.so.2'


class SharedTests(unittest.TestCase):
    setUpClass = classmethod(core.LinkerTests.setUpClass.__func__)
    tearDownClass = classmethod(core.LinkerTests.tearDownClass.__func__)
    assemble = classmethod(core.LinkerTests.assemble.__func__)
    link = core.LinkerTests.link
    rejected = core.LinkerTests.rejected

    def setUp(self):
        if not Path(INTERPRETER).is_file():
            self.skipTest('glibc x86-64 interpreter unavailable; loader gate remains unverified')

    def shared(self, name, source, *, style='both', versions=None):
        path = self.root / (name+'.c')
        path.write_text(source)
        output = self.root / ('lib'+name+'.so')
        flags = []
        if versions:
            script = self.root / (name+'.map')
            script.write_text(versions)
            flags = ['-Wl,--version-script='+str(script)]
        run('gcc', '-fPIC', '-O2', '-nostdlib', '-shared', '-Wl,--hash-style='+style,
            '-Wl,-soname,'+output.name, *flags, path, '-o', output)
        return output

    def options(self, pie=True):
        return ('--dynamic-linker', INTERPRETER, '-rpath', '$ORIGIN') + (('-pie',) if pie else ())

    def test_functions_data_pointers_weak_and_preload(self):
        library = self.shared('imports', 'int datum=7; int answer(void){return 35;}')
        source = self.root / 'imports.c'
        source.write_text('extern int datum, answer(void);\n'
                          'extern int optional(void) __attribute__((weak));\n'
                          'int *pointer=&datum; int (*function)(void)=answer;\n'
                          'int entry(void){if(pointer!=&datum || function!=answer) return 99;\n'
                          'if(optional) return optional(); return function()+*pointer;}\n')
        obj = source.with_suffix('.o')
        run('gcc', '-c', '-O1', '-g', '-fPIC', source, '-o', obj)
        start = self.assemble('import-start', '.text\n.globl _start\n_start: call entry\nmov %eax,%edi\nmov $60,%eax\nsyscall')
        preload = self.shared('optional', 'int optional(void){return 43;}')
        for pie in (False, True):
            for linker in (LINKER, 'ld'):
                with self.subTest(pie=pie, linker=linker):
                    out = self.link('import-program', start, obj, library, options=self.options(pie), linker=linker)
                    run(out, expected=42)
                    result = subprocess.run([str(out)], env=dict(os.environ, LD_PRELOAD=str(preload)), capture_output=True, timeout=5)
                    self.assertEqual(result.returncode, 43, result.stderr)
                    self.assertEqual(run('readelf', '-aW', out).stderr, b'')
                    if linker == LINKER:
                        data = out.read_bytes()
                        imports = {s['name']: s for s in core.elf_symbols(data, kind=11)}
                        self.assertEqual(imports['optional']['info'] >> 4, 2)
                        self.assertEqual(imports['answer']['info'] & 15, 2)
                        self.assertEqual(imports['datum']['info'] & 15, 1)
                        self.assertTrue(all(imports[name]['section'] == 0 for name in ('optional', 'answer', 'datum')))

    def test_default_and_explicit_symbol_versions_and_loader_rejection(self):
        library = self.shared('versions', '''int old(void){return 7;} int current(void){return 35;}
__asm__(".symver old,answer@V1");
__asm__(".symver current,answer@@V2");''', versions='V1 {}; V2 {} V1;')
        start = self.assemble('version-start', '''.symver old_answer,answer@V1
.text
.globl _start
_start: call old_answer
 mov %eax,%ebx
 call answer
 add %ebx,%eax
 mov %eax,%edi
 mov $60,%eax
 syscall''')
        for linker in (LINKER, 'ld'):
            out = self.link('version-program', start, library, options=self.options(), linker=linker)
            run(out, expected=42)
            report = run('readelf', '-VW', out)
            self.assertEqual(report.stderr, b'')
            self.assertIn(b'V1', report.stdout)
            self.assertIn(b'V2', report.stdout)
        out = self.link('required-versions', start, library, options=self.options())
        self.shared('versions', 'int answer(void){return 42;}', versions='V2 { global: answer; };')
        result = subprocess.run([str(out)], capture_output=True, timeout=5)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn(b"version `V1' not found", result.stderr)

    def test_executable_exports_callbacks_interposition_and_archive_roots(self):
        library = self.shared('callback', 'extern int callback(void);\n'
                              'int override(void){return 1;}\n'
                              'int drive(void){return callback()+override();}\n')
        start = self.assemble('callback-start', '.text\n.globl _start\n'
                              '_start: call drive; mov %eax,%edi; mov $60,%eax; syscall')
        source = self.root / 'callback.c'
        source.write_text('int callback(void){return 7;}\n'
                          'int override(void){return 35;}\n'
                          '__attribute__((visibility("hidden"))) int hidden(void){return 99;}\n'
                          'int unused(void){return 88;}\n')
        obj = source.with_suffix('.o')
        run('gcc', '-c', '-O2', '-fPIC', '-ffunction-sections', source, '-o', obj)
        archive = self.root / 'libcallback.a'
        run('ar', 'rcs', archive, obj)
        for pie in (False, True):
            for linker in (LINKER, 'ld'):
                for archive_input in (False, True):
                    with self.subTest(pie=pie, linker=linker, archive=archive_input):
                        out = self.link('callback-exe', start, library,
                                        archive if archive_input else obj,
                                        options=self.options(pie)+('--gc-sections',), linker=linker)
                        run(out, expected=42)
                        exported = {s['name']:s for s in core.elf_symbols(out.read_bytes(), kind=11)
                                    if s['section'] != 0}
                        self.assertTrue({'callback','override'} <= exported.keys())
                        self.assertNotIn('hidden', exported)
                        self.assertNotIn('unused', exported)
                        self.assertEqual(run('readelf', '-aW', out).stderr, b'')
        for flags, present in ((('--export-dynamic',), True),
                               (('-E', '--no-export-dynamic'), False),
                               (('--no-export-dynamic','-E'), True)):
            out = self.link('exports-exe', start, obj, library,
                            options=self.options()+('--gc-sections',)+flags)
            exported = {s['name'] for s in core.elf_symbols(out.read_bytes(), kind=11)
                        if s['section'] != 0}
            self.assertEqual('unused' in exported, present)
            self.assertNotIn('hidden', exported)
            run(out, expected=42)

    def test_library_hash_styles_and_archive_selection(self):
        for style in ('sysv', 'gnu', 'both'):
            with self.subTest(style=style):
                library = self.shared('hash-'+style, 'int answer(void){return 41;}', style=style)
                run(self.link('hash-program', self.simple, library, options=self.options()), expected=41)
        library = self.shared('selection', 'int answer(void){return 41;}')
        archive = self.root / 'libselection.a'
        run('ar', 'rcs', archive, self.answer)
        for linker in (LINKER, 'ld'):
            for inputs, expected in (((library, archive), 41), ((archive, library), 42)):
                with self.subTest(linker=linker, inputs=inputs):
                    run(self.link('selection-program', self.simple, *inputs,
                                  options=self.options(), linker=linker), expected=expected)
        run(self.link('selection-search', self.simple,
                      options=(*self.options(), '-L', self.root, '-lselection')), expected=41)
        out = self.link('selection-repeat', self.simple, library, library, options=self.options())
        self.assertEqual(run('readelf', '-dW', out).stdout.count(b'(NEEDED)'), 1)

    def test_pinned_libc_import_versions(self):
        libc = Path('/lib/x86_64-linux-gnu/libc.so.6')
        if not libc.is_file():
            self.skipTest('declared glibc path unavailable')
        start = self.assemble('libc-imports', '''.text
.globl _start
_start: and $-16,%rsp
 mov $1,%edi
 lea message(%rip),%rsi
 mov $7,%edx
 call write
 mov $42,%edi
 call _exit
.section .rodata
message: .ascii "fas-ld\\n"''')
        out = self.link('libc-import-program', start, libc, options=self.options())
        self.assertEqual(run(out, expected=42).stdout, b'fas-ld\n')
        report = run('readelf', '-aW', out)
        self.assertEqual(report.stderr, b'')
        self.assertIn(b'GLIBC_2.2.5', report.stdout)
        self.assertIn(b'libc.so.6', report.stdout)

    def test_hidden_weak_gc_and_unsupported_live_imports(self):
        library = self.shared('unsupported', 'int datum=42; int answer(void){return 42;} __thread int tls=9;')
        hidden = self.assemble('hidden-weak', '.weak answer\n.hidden answer\n.text\n.globl _start\n_start: mov answer@GOTPCREL(%rip),%rax\ntest %rax,%rax\njnz 1f\nmov $42,%edi\njmp 2f\n1: mov $99,%edi\n2: mov $60,%eax\nsyscall')
        run(self.link('hidden-weak-program', hidden, library, options=self.options()), expected=42)
        for name, source, diagnostic in (
            ('function-direct', '.long answer - .', 'direct import'),
            ('hidden-strong', '.hidden answer\ncall answer', 'non-default visibility'),
            ('tls-import', 'mov tls@GOTPCREL(%rip),%rax', 'non-TLS relocation')):
            obj = self.assemble(name, '.text\n.globl _start\n_start: '+source+'\nret')
            self.rejected(obj, diagnostic, library, options=self.options())
        dead = self.assemble('dead-import', '.section .text.dead,"ax"\ncall nonexistent\n.text\n.globl _start\n_start: mov $60,%eax\nmov $42,%edi\nsyscall')
        run(self.link('dead-import-program', dead, library,
                      options=(*self.options(), '--gc-sections')), expected=42)
        self.rejected(self.simple, 'requires -pie', library)

    def test_shared_metadata_bounds_hashes_versions_and_mutations(self):
        library = self.shared('malformed', 'int answer(void){return 42;}', versions='V1 {global: answer;};')
        original = library.read_bytes()
        sections = core.elf_sections(original)
        dynsym = next(s for s in sections if s['kind'] == 11)
        gnu = next(s for s in sections if s['kind'] == 0x6ffffff6)
        sysv = next(s for s in sections if s['kind'] == 5)
        versym = next(s for s in sections if s['kind'] == 0x6fffffff)
        verdef = next(s for s in sections if s['kind'] == 0x6ffffffd)
        cases = [('program-table', 32, 'Q', len(original)), ('section-table', 40, 'Q', len(original)),
                 ('symbol-size', dynsym['header']+32, 'Q', 25),
                 ('symbol-null', dynsym['offset']+8, 'Q', 1),
                 ('string-link', dynsym['header']+40, 'I', 0),
                 ('gnu-bloom-size', gnu['offset']+8, 'I', 0),
                 ('gnu-bloom-bits', gnu['offset']+16, 'Q', 0),
                 ('sysv-size', sysv['offset']+4, 'I', 0xffffffff),
                 ('version-size', versym['header']+32, 'Q', 1),
                 ('version-link', verdef['offset']+16, 'I', 1)]
        path = self.root / 'malformed-library.so'
        for name, at, fmt, value in cases:
            with self.subTest(name=name):
                data = bytearray(original)
                struct.pack_into('<'+fmt, data, at, value)
                path.write_bytes(data)
                self.rejected(self.simple, 'shared', path, options=self.options())
        rng = random.Random(0xD50)
        for _ in range(256):
            data = bytearray(original)
            section = rng.choice((dynsym, gnu, sysv, versym, verdef))
            data[section['offset']+rng.randrange(section['size'])] ^= rng.randrange(1, 256)
            path.write_bytes(data)
            result = subprocess.run([LINKER, *self.options(), '-o', str(self.root/'mutation-output'), str(self.simple), str(path)],
                                    capture_output=True, timeout=3)
            self.assertIn(result.returncode, (0, 1), result.stderr)
            self.assertNotIn(b'Fatal error', result.stderr)


if __name__ == '__main__':
    unittest.main(verbosity=2)
