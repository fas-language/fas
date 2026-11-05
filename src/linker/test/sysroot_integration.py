#!/usr/bin/env python3
"""Isolated target roots, GNU search ordering, runtime paths and publication."""
import os
from pathlib import Path
import shutil
import struct
import subprocess
import sys
import unittest

import hosted_integration as hosted

core = hosted.core
run = core.run
LINKER = core.LINKER
REENTRY = str(Path(sys.argv.pop(1)).resolve())


def dynamic_strings(path):
    data = path.read_bytes()
    sections = core.elf_sections(data)
    dynamic = next(s for s in sections if s['kind'] == 6)
    strings = sections[dynamic['link']]
    names = data[strings['offset']:strings['offset'] + strings['size']]
    result = {}
    for at in range(dynamic['offset'], dynamic['offset'] + dynamic['size'], 16):
        tag, value = struct.unpack_from('<qQ', data, at)
        if tag in (1, 14, 15, 29):
            result.setdefault(tag, []).append(names[value:].split(b'\0', 1)[0].decode())
    return result


class SysrootTests(unittest.TestCase):
    setUpClass = hosted.HostedTests.__dict__['setUpClass']
    tearDownClass = hosted.HostedTests.__dict__['tearDownClass']
    assemble = hosted.HostedTests.__dict__['assemble']
    link = core.LinkerTests.link
    rejected = core.LinkerTests.rejected
    compile = hosted.HostedTests.compile
    crt = hosted.HostedTests.crt

    def setUp(self):
        self.case = self.root / self._testMethodName
        self.case.mkdir()
        self.target = self.case / 'target root'
        self.lib = self.target / 'lib'
        self.lib.mkdir(parents=True)
        run('ar', 'rcs', self.lib/'libanswer.a', self.answer)
        self.wrong = self.assemble(self._testMethodName+'-wrong',
                                  '.text\n.globl answer\nanswer: mov $43,%eax; ret')

    def script(self, name, text):
        path = self.case/name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text)
        return path

    def test_library_prefixes_global_paths_and_root_selection(self):
        for linker in (LINKER, 'ld'):
            for directory in ('=lib', '=/lib', '$SYSROOT/lib'):
                for flag in (('-L', directory), ('--library-path='+directory,)):
                    with self.subTest(linker=linker, directory=directory, flag=flag):
                        out = self.link('root-search', self.simple, '-lanswer', *flag,
                                        options=('--sysroot='+str(self.target),), linker=linker)
                        run(out, expected=42)
            relative = os.path.relpath(self.target)
            out = self.link('root-relative', self.simple, '-lanswer',
                            options=('--sysroot='+relative+'/', '-L=/lib'), linker=linker)
            run(out, expected=42)
            out = self.link('root-last', self.simple, '-lanswer',
                            options=('--sysroot=/missing', '--sysroot='+str(self.target),
                                     '--library-path', '=lib'), linker=linker)
            run(out, expected=42)
            # Ordinary absolute -L and positional objects are host paths.
            out = self.link('root-host-path', self.simple, self.answer,
                            options=('--sysroot=/missing',), linker=linker)
            run(out, expected=42)
            out = self.link('root-reset', self.simple, '-lanswer',
                            options=('--sysroot='+str(self.target), '--sysroot=',
                                     '-L', self.lib), linker=linker)
            run(out, expected=42)
        run(self.link('root-space-form', self.simple, '-lanswer',
                      options=('--sysroot', self.target, '-L=/lib')), expected=42)
        self.rejected(self.simple, 'cannot find -lanswer', '-lanswer',
                      options=('--sysroot='+str(self.target),))
        self.rejected(self.simple, 'cannot find -lanswer', '-lanswer',
                      options=('--sysroot', self.case/'missing', '-L=/lib'))

    def test_script_prefixes_inside_outside_and_canonical_boundaries(self):
        shutil.copyfile(self.answer, self.lib/'answer.o')
        for linker in (LINKER, 'ld'):
            for token in ('=/lib/answer.o', '$SYSROOT/lib/answer.o'):
                script = self.script('prefix.ld', f'INPUT("{token}")')
                run(self.link('root-prefix', self.simple, script,
                              options=('--sysroot='+str(self.target),), linker=linker), expected=42)
            inside = self.script('target root/lib/inside.ld', 'INPUT(/lib/answer.o)')
            alias = self.case/'inside-alias.ld'
            alias.unlink(missing_ok=True)
            alias.symlink_to(inside)
            for script in (inside, alias):
                run(self.link('root-inside', self.simple, script,
                              options=('--sysroot='+str(self.target),), linker=linker), expected=42)
            outside = self.script('target root-sibling/outside.ld',
                                  f'INPUT("{self.answer}")')
            alias = self.lib/'outside-alias.ld'
            alias.unlink(missing_ok=True)
            alias.symlink_to(outside)
            for script in (outside, alias):
                run(self.link('root-outside', self.simple, script,
                              options=('--sysroot='+str(self.target),), linker=linker), expected=42)
            # File prefixes concatenate verbatim, unlike -L=lib.
            script = self.script('relative-prefix.ld', 'INPUT(=lib/answer.o)')
            p = run(linker, '--sysroot='+str(self.target), '-o', self.case/'bad-relative',
                    self.simple, script, expected=1)
            self.assertIn(b'lib/answer.o', p.stderr)
        missing = self.script('target root/lib/missing.ld', f'INPUT("{self.answer}")')
        self.rejected(self.simple, 'cannot find script input', missing,
                      options=('--sysroot='+str(self.target),))

    def test_search_dir_script_scope_cli_precedence_and_nostdlib(self):
        (self.lib/'libanswer.a').unlink()
        custom = self.target/'custom search'
        custom.mkdir()
        run('ar', 'rcs', custom/'libanswer.a', self.answer)
        wrong_dir = self.case/'wrong'
        wrong_dir.mkdir()
        run('ar', 'rcs', wrong_dir/'libanswer.a', self.wrong)
        for linker in (LINKER, 'ld'):
            for text in ('SEARCH_DIR("=/custom search") INPUT(-lanswer)',
                         'GROUP(-lanswer) SEARCH_DIR("$SYSROOT/custom search")'):
                script = self.script('search.ld', text)
                run(self.link('root-script-search', self.simple, script,
                              options=('--sysroot='+str(self.target),), linker=linker), expected=42)
                run(self.link('root-cli-precedence', self.simple, script, '-L', wrong_dir,
                              options=('--sysroot='+str(self.target),), linker=linker), expected=43)
            search = self.script('search-only.ld', 'SEARCH_DIR("=/custom search")')
            run(self.link('root-search-later', self.simple, search, '-lanswer',
                          options=('--sysroot='+str(self.target),), linker=linker), expected=42)
            run(linker, '--sysroot='+str(self.target), '-o', self.case/'bad-order',
                self.simple, '-lanswer', search, expected=1)
            run(linker, '--sysroot='+str(self.target), '-nostdlib', '-o', self.case/'bad-nostdlib',
                self.simple, search, '-lanswer', expected=1)
            run(self.link('root-nostdlib-explicit', self.simple, search, '-lanswer',
                          options=('--sysroot='+str(self.target), '-nostdlib', '-L=/custom search'),
                          linker=linker), expected=42)
        self.script('target root/custom search/nested-search.ld', 'SEARCH_DIR("=/custom search")')
        nested = self.script('nested-search.ld', 'INPUT("=/custom search/nested-search.ld")')
        for linker in (LINKER, 'ld'):
            run(linker, '--sysroot='+str(self.target), '-o', self.case/'bad-nested-order',
                self.simple, '-lanswer', nested, expected=1)
            run(self.link('root-nested-search-later', self.simple, nested, '-lanswer',
                          options=('--sysroot='+str(self.target),), linker=linker), expected=42)
        for text in ('SEARCH_DIR()', 'SEARCH_DIR(a b)', 'SEARCH_DIR("unterminated)'):
            self.rejected(self.simple, 'linker script', self.script('bad-search.ld', text))

    def test_nested_scripts_static_policy_partial_and_as_needed(self):
        shared = self.lib/'libanswer.so'
        run('ld', '-shared', '-soname', shared.name, '-o', shared, self.wrong)
        self.script('target root/lib/inner.ld', 'INPUT(AS_NEEDED(-lanswer))')
        outer = self.script('outer.ld', 'SEARCH_DIR("=/lib") GROUP(=/lib/inner.ld)')
        options = ('--sysroot='+str(self.target), '-pie', '--dynamic-linker', hosted.INTERPRETER,
                   '-rpath', str(self.lib))
        for linker in (LINKER, 'ld'):
            run(self.link('root-dynamic', self.simple, outer, options=options,
                          linker=linker), expected=43)
            run(self.link('root-static-selection', self.simple, '-Bstatic', outer,
                          '-Bdynamic', options=options, linker=linker), expected=42)
            run(self.link('root-pop-policy', self.simple, '--push-state', '-Bstatic',
                          '--pop-state', outer, options=options, linker=linker), expected=43)
        partial = self.link('root-partial.o', self.simple, '-lanswer',
                            options=('-r', '--sysroot='+str(self.target), '-L=/lib', '-Bstatic'))
        for linker in (LINKER, 'ld'):
            run(self.link('root-partial-final', partial, linker=linker), expected=42)

    def test_search_directories_from_library_scripts_are_positional(self):
        (self.lib/'libanswer.a').unlink()
        custom = self.target/'custom libraries'
        custom.mkdir()
        run('ar', 'rcs', custom/'libanswer.a', self.answer)
        (self.lib/'libsearch.so').write_text('SEARCH_DIR("=/custom libraries")')
        options = ('--sysroot='+str(self.target), '-L=/lib', '-pie',
                   '--dynamic-linker', hosted.INTERPRETER)
        for linker in (LINKER, 'ld'):
            run(self.link('root-library-script', self.simple, '-lsearch', '-lanswer',
                          options=options, linker=linker), expected=42)
            run(linker, *options, '-o', self.case/'bad-library-script-order',
                self.simple, '-lanswer', '-lsearch', expected=1)

    def test_rooted_inputs_are_protected_from_output_and_map_aliases(self):
        library = self.lib/'libanswer.a'
        original = library.read_bytes()
        alias = self.case/'library-alias'
        os.link(library, alias)
        for flags in (('-o', alias), ('-o', self.case/'image', '-Map', alias)):
            result = run(LINKER, '--sysroot='+str(self.target), '-L=/lib', *flags,
                         self.simple, '-lanswer', expected=1)
            self.assertIn(b'overwrite input', result.stderr)
            self.assertEqual(library.read_bytes(), original)
        script = self.script('target root/lib/input.ld', 'INPUT(/lib/libanswer.a)')
        self.rejected(self.simple, 'cannot find -labsent', script, '-labsent',
                      options=('--sysroot='+str(self.target),))
        self.assertEqual(list(self.case.rglob('.fas-ld-*.tmp')), [])

    def test_strict_dependency_paths_and_runtime_strings(self):
        leaf_obj = self.compile('root-leaf', 'int leaf(void){return 42;}')
        bridge_obj = self.compile('root-bridge',
                                  'extern int leaf(void);int answer(void){return leaf();}')
        deps = self.target/'target deps'
        deps.mkdir()
        leaf = deps/'libroot-leaf.so'
        run('ld', '-shared', '-soname', leaf.name, '-o', leaf, leaf_obj)
        bridge = self.lib/'libroot-bridge.so'
        for tag in ('--enable-new-dtags', '--disable-new-dtags'):
            for runtime, explicit in (('/target deps', ()),
                                      ('$ORIGIN/../target deps', ()),
                                      ('', ('-rpath-link', '=/target deps')),
                                      ('', ('-rpath-link', '$SYSROOT/target deps')),
                                      ('', ('-rpath-link', str(deps)))):
                run('ld', '-shared', '-soname', bridge.name, '-o', bridge, bridge_obj,
                    leaf, tag, '-rpath', runtime)
                out = self.link('root-strict', self.simple, bridge,
                                options=('--sysroot='+str(self.target), '-pie',
                                         '--dynamic-linker', hosted.INTERPRETER,
                                         '--no-allow-shlib-undefined', '-rpath', '$ORIGIN',
                                         *explicit))
                self.assertEqual(dynamic_strings(out), {1: [bridge.name], 29: ['$ORIGIN']})
                strings = next(s for s in core.elf_sections(out.read_bytes())
                               if s['name'] == '.dynstr')
                self.assertNotIn(str(self.target).encode(),
                                 out.read_bytes()[strings['offset']:strings['offset']+strings['size']])
                env = dict(os.environ, LD_LIBRARY_PATH=str(self.lib)+':'+str(deps))
                p = subprocess.run([out], env=env, capture_output=True, timeout=10)
                self.assertEqual(p.returncode, 42, p.stderr)
                segments = core.elf_segments(out.read_bytes())
                interp = next(s for s in segments if s['kind'] == 3)
                self.assertEqual(out.read_bytes()[interp['offset']:interp['offset']+interp['file_size']],
                                 hosted.INTERPRETER.encode()+b'\0')
        run('ld', '-shared', '-soname', '/target deps/libroot-leaf.so', '-o', leaf, leaf_obj)
        run('ld', '-shared', '-soname', bridge.name, '-o', bridge, bridge_obj, leaf)
        self.link('root-absolute-needed', self.simple, bridge,
                  options=('--sysroot='+str(self.target), '-pie', '--no-allow-shlib-undefined'))
        self.rejected(self.simple, 'cannot find DT_NEEDED', bridge,
                      options=('--sysroot', self.case/'absent', '-pie',
                               '--no-allow-shlib-undefined'))

    def test_isolated_glibc_script_and_hosted_startup(self):
        if not hosted.LIBC.is_file():
            self.skipTest('glibc unavailable')
        script = self.script('target root/lib/libc.so',
                             'GROUP(/lib/libc.so.6 /lib/libc_nonshared.a '
                             'AS_NEEDED(/lib/ld-linux-x86-64.so.2))')
        for name, source in (('libc.so.6', hosted.LIBC),
                             ('libc_nonshared.a', self.crt('libc_nonshared.a')),
                             ('ld-linux-x86-64.so.2', hosted.INTERPRETER)):
            shutil.copyfile(source, self.lib/name)
        crt = []
        for name in ('Scrt1.o', 'crti.o', 'crtbeginS.o', 'crtendS.o', 'crtn.o'):
            destination = self.lib/name
            shutil.copyfile(self.crt(name), destination)
            crt.append(destination)
        obj = self.compile('root-hosted',
                           '#include <stdio.h>\n__thread int x=42;'
                           'int main(void){puts("rooted libc");return x;}')
        for linker in (LINKER, 'ld'):
            out = self.link('root-hosted', *crt[:3], obj, '-lc', *crt[3:],
                            options=('--sysroot='+str(self.target), '-L=/lib', '-pie',
                                     '--dynamic-linker', hosted.INTERPRETER, '--gc-sections'),
                            linker=linker)
            self.assertEqual(run(out, expected=42).stdout, b'rooted libc\n')
            self.assertEqual(run('readelf', '-aW', out).stderr, b'')
        self.assertTrue(script.is_file())

    def test_repeat_failure_and_concurrent_distinct_roots(self):
        other = self.case/'other root'
        (other/'lib').mkdir(parents=True)
        run('ar', 'rcs', other/'lib/libanswer.a', self.wrong)
        # SEARCH_DIR is intentionally absent from the second config; state must
        # belong to each invocation, including after a failing script input.
        script = self.script('state.ld', 'SEARCH_DIR("=/lib") INPUT(-lanswer)')
        first = ('--sysroot='+str(self.target), self.simple, script)
        second = ('--sysroot', other, '-L=/lib', self.simple, '-lanswer')
        result = run(REENTRY, *first, '--next', *second)
        self.assertIn(b'concurrent distinct sysroots passed', result.stdout)

    @unittest.skipUnless(os.environ.get('FAS_MUSL_SYSROOT'),
                         'set FAS_MUSL_SYSROOT; rooted musl gate unverified')
    def test_isolated_musl_static_and_dynamic_library_search(self):
        target = Path(os.environ['FAS_MUSL_SYSROOT']).resolve()
        source = self.case/'musl.c'
        source.write_text('#include <stdio.h>\n__thread int x=42;'
                          'int main(void){puts("rooted musl");return x;}')
        obj = source.with_suffix('.o')
        run(target/'bin/musl-gcc', '-c', '-O2', '-fPIC', '-ftls-model=initial-exec',
            source, '-o', obj)
        for mode in ('static', 'fixed', 'pie'):
            options = ('--sysroot='+str(target), '-L=/lib', '--gc-sections')
            if mode == 'static':
                options += ('-static',)
            else:
                options += ('--dynamic-linker', str(target/'lib/ld-musl-x86_64.so.1'))
                if mode == 'pie':
                    options += ('-pie',)
            for linker in (LINKER, 'ld'):
                out = self.link('root-musl', target/'lib'/('Scrt1.o' if mode == 'pie' else 'crt1.o'),
                                target/'lib/crti.o', self.crt('crtbeginS.o'), obj, '-lc',
                                self.crt('libgcc.a'), self.crt('crtendS.o'), target/'lib/crtn.o',
                                options=options, linker=linker)
                self.assertEqual(run(out, expected=42).stdout, b'rooted musl\n')
                self.assertEqual(run('readelf', '-aW', out).stderr, b'')

    def test_directory_and_path_resource_limits(self):
        self.rejected(self.simple, '4096 directories', self.answer,
                      options=tuple('-L=lib' for _ in range(4097)))
        script = self.script('too-many.ld', 'SEARCH_DIR("=/lib")\n'*4097)
        self.rejected(self.simple, '4096 directories', script,
                      options=('--sysroot='+str(self.target),))
        self.rejected(self.simple, 'oversized sysroot', self.answer,
                      options=('--sysroot='+('x'*65537),))
        # Successful searches near the end of a large directory list must also
        # consume the probe budget; a missing-library test would stop too early.
        self.rejected(self.simple, '1000000 probes', *('-lanswer' for _ in range(245)),
                      options=('--sysroot='+str(self.target),
                               *('-L=/missing' for _ in range(4095)), '-L=/lib'))


if __name__ == '__main__':
    unittest.main(verbosity=2)
