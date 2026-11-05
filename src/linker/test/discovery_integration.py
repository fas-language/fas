#!/usr/bin/env python3
"""Runtime dependency search, dynamic tags and independent producer/consumers."""
import os
from pathlib import Path
import shutil
import struct
import subprocess
import sys
import unittest

import shared_integration as fixtures

core = fixtures.core
run = core.run
DYNAMIC_REENTRY = str(Path(sys.argv.pop(1)).resolve())


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


class DiscoveryTests(unittest.TestCase):
    setUpClass = classmethod(core.LinkerTests.setUpClass.__func__)
    tearDownClass = classmethod(core.LinkerTests.tearDownClass.__func__)
    assemble = classmethod(core.LinkerTests.assemble.__func__)
    link = core.LinkerTests.link
    rejected = core.LinkerTests.rejected
    setUp = fixtures.SharedTests.setUp

    @staticmethod
    def tools():
        return [core.LINKER, 'ld'] + (['ld.lld-22'] if shutil.which('ld.lld-22') else [])

    def compile(self, name, source):
        path = self.root / (name + '.c')
        path.write_text(source)
        obj = path.with_suffix('.o')
        run('gcc', '-c', '-O2', '-fPIC', '-fno-stack-protector', path, '-o', obj)
        return obj

    def execute(self, path, *, library_path=None, expected=42):
        env = {k: v for k, v in os.environ.items() if not k.startswith('LD_')}
        if library_path is not None:
            env['LD_LIBRARY_PATH'] = str(library_path)
        result = subprocess.run([str(path)], env=env, cwd='/', capture_output=True, timeout=5)
        self.assertEqual(result.returncode, expected, result.stderr)
        return result

    def test_tag_selection_default_last_wins_and_no_path(self):
        for tool in self.tools():
            variants = [(('--enable-new-dtags',), 29),
                        (('--disable-new-dtags',), 15),
                        (('--disable-new-dtags', '--enable-new-dtags'), 29),
                        (('--enable-new-dtags', '--disable-new-dtags'), 15)]
            if tool == core.LINKER:
                variants.append(((), 29))
            for flags, tag in variants:
                for shared in (False, True):
                    with self.subTest(tool=tool, flags=flags, shared=shared):
                        mode = ('-shared',) if shared else ('-pie', '--dynamic-linker', fixtures.INTERPRETER)
                        out = self.link('tag-choice', self.simple, self.answer, linker=tool,
                                        options=(*mode, *flags, '-rpath', '$ORIGIN/private',
                                                 '-rpath', '/a path/with spaces'))
                        tags = dynamic_strings(out)
                        self.assertEqual(tags.get(tag), ['$ORIGIN/private:/a path/with spaces'])
                        self.assertNotIn(15 if tag == 29 else 29, tags)
                        self.assertEqual(run('readelf', '-aW', out).stderr, b'')
                        if not shared:
                            self.execute(out)
            out = self.link('no-runtime-path', self.simple, self.answer, linker=tool,
                            options=('-pie', '--disable-new-dtags'))
            self.assertNotIn(15, dynamic_strings(out))
            self.assertNotIn(29, dynamic_strings(out))

    def test_transitive_runtime_search_inheritance_and_environment_priority(self):
        objects = [self.compile('search-leaf', 'int search_leaf(void){return 35;}'),
                   self.compile('search-middle', 'extern int search_leaf(void); int search_middle(void){return search_leaf()+3;}'),
                   self.compile('search-top', 'extern int search_middle(void); int search_top(void){return search_middle()+4;}')]
        alternate = self.compile('search-alternate-leaf', 'int search_leaf(void){return 36;}')
        start = self.assemble('search-start', '.text\n.globl _start\n_start: call search_top; mov %eax,%edi; mov $60,%eax; syscall')
        names = ['libfas_discovery_leaf.so', 'libfas_discovery_middle.so', 'libfas_discovery_top.so']
        for producer_index, producer in enumerate(self.tools()):
            directory = self.root / ('dependency-tree-' + str(producer_index))
            private = directory / 'private'
            private.mkdir(parents=True)
            replacement = directory / 'replacement'
            replacement.mkdir()
            libraries = [private / name for name in names]
            run('ld', '-shared', '-soname', names[0], '-o', replacement/names[0], alternate)

            def build_libraries(own_paths):
                for n, (obj, out) in enumerate(zip(objects, libraries)):
                    if own_paths == 'inherited-rpath' and n == 2:
                        paths = ('--disable-new-dtags', '-rpath', '${ORIGIN}')
                    else:
                        paths = ('--enable-new-dtags', '-rpath', '$ORIGIN') if own_paths is True and n else ()
                    run(producer, '-shared', '-soname', names[n], *paths, '-o', out,
                        obj, *([libraries[n-1]] if n else []))
                    tags = dynamic_strings(out)
                    self.assertEqual(tags.get(14), [names[n]])
                    self.assertEqual(tags.get(1, []), [names[n-1]] if n else [])

            for own_paths in (False, True, 'inherited-rpath'):
                build_libraries(own_paths)
                for consumer in self.tools():
                    for new_dtags in (False, True):
                        with self.subTest(producer=producer, consumer=consumer,
                                          own_paths=own_paths, new_dtags=new_dtags):
                            out = directory / 'program'
                            flags = ('--enable-new-dtags' if new_dtags else '--disable-new-dtags',)
                            # GNU normally validates DSO undefined references at link time.
                            # This case deliberately delegates dependency lookup to the loader.
                            defer = () if consumer == core.LINKER else ('--allow-shlib-undefined',)
                            if own_paths and consumer == core.LINKER:
                                defer = ('--no-allow-shlib-undefined',)
                            run(consumer, '-pie', '--dynamic-linker', fixtures.INTERPRETER,
                                *flags, *defer, '-rpath', '$ORIGIN/private', '-o', out,
                                start, libraries[2])
                            self.assertEqual(dynamic_strings(out).get(1), [names[2]])
                            if new_dtags and not own_paths:
                                result = self.execute(out, expected=127)
                                self.assertIn(names[1].encode(), result.stderr)
                                self.execute(out, library_path=private)
                            else:
                                self.execute(out)
                                # A dependency's own RUNPATH suppresses inherited
                                # RPATH for its lookup, so the environment wins.
                                self.execute(out, library_path=replacement,
                                             expected=43 if own_paths is True else 42)

    def test_strict_transitive_versions_explicit_scope_and_atomic_failures(self):
        leaf_obj = self.compile('strict-leaf', r'''
int old(void){return 35;} int current(void){return 99;}
__asm__(".symver old,dependent@V1\n.symver current,dependent@@V2");
''')
        policy = self.root / 'strict-versions.map'
        policy.write_text('V1 {}; V2 {} V1;')
        leaf = self.root / 'libstrict_leaf.so'
        run('ld', '-shared', '-soname', leaf.name, '--version-script', policy,
            '-o', leaf, leaf_obj)
        middle_obj = self.compile('strict-middle', r'''
extern int old(void); extern int optional(void) __attribute__((weak));
__asm__(".symver old,dependent@V1");
int middle(void){return old()+7+(optional ? optional() : 0);}
''')
        start = self.assemble('strict-start', '.text\n.globl _start\n_start: call middle; mov %eax,%edi; mov $60,%eax; syscall')
        direct_child = self.assemble('strict-direct-child', '.text\n.globl _start\n_start: call dependent; mov %eax,%edi; mov $60,%eax; syscall')
        strict = ('-pie', '--dynamic-linker', fixtures.INTERPRETER,
                  '--no-allow-shlib-undefined', '-rpath', '$ORIGIN')
        for producer in self.tools():
            middle = self.root / 'libstrict_middle.so'
            run(producer, '-shared', '-soname', middle.name, '-o', middle, middle_obj, leaf)
            for consumer in self.tools():
                with self.subTest(producer=producer, consumer=consumer):
                    out = self.link('strict-exe', start, middle, linker=consumer,
                                    options=(*strict, '-rpath-link', self.root))
                    self.assertEqual(dynamic_strings(out).get(1), [middle.name])
                    self.execute(out, library_path=self.root)
                    # Indirect definitions are unavailable to regular objects.
                    run(consumer, *strict, '-rpath-link', self.root, '-o', out,
                        direct_child, middle, expected=1)
            self.rejected(start, 'cannot find DT_NEEDED', middle, options=strict)
            if producer == core.LINKER:
                run(DYNAMIC_REENTRY, *strict, '-rpath-link', self.root, start, middle)
            out = self.link('strict-direct-dependencies', start, middle, leaf, options=strict)
            self.execute(out)
            out = self.link('strict-disabled', start, middle,
                            options=(*strict, '--allow-shlib-undefined'))
            self.execute(out, library_path=self.root)
            # Skipped as-needed inputs do not cause dependency validation.
            out = self.link('strict-unused', self.simple, self.answer, middle,
                            options=(*strict, '--as-needed'))
            self.execute(out)

        # Colon paths and the single-dash spelling select in declaration order.
        out = self.link('strict-colon-path', start, middle,
                        options=(*strict, '-rpath-link=/absent:'+str(self.root)))
        self.execute(out, library_path=self.root)

        # Both executable and map publication protect discovered inputs.
        original = leaf.read_bytes()
        permissions = leaf.stat().st_mode & 0o777
        for flags in (('-o', leaf), ('-o', self.root/'strict-map-exe', '-Map', leaf)):
            result = run(core.LINKER, *strict, '-rpath-link', self.root, *flags,
                         start, middle, expected=1)
            self.assertIn(b'overwrite input', result.stderr)
            self.assertEqual(leaf.read_bytes(), original)
            self.assertEqual(leaf.stat().st_mode & 0o777, permissions)

        # Same SONAME, wrong version: finding a basename is insufficient.
        wrong = self.compile('strict-wrong-version', 'int dependent(void){return 35;}')
        policy.write_text('V2 { global: dependent; };')
        run('ld', '-shared', '-soname', leaf.name, '--version-script', policy,
            '-o', leaf, wrong)
        self.rejected(start, 'required version V1', middle,
                      options=(*strict, '--rpath-link='+str(self.root)))

    def test_indirect_references_do_not_extract_archives(self):
        leaf_obj = self.compile('archive-callback-leaf',
                                'extern int archive_callback(void); int archive_leaf(void){return archive_callback();}')
        top_obj = self.compile('archive-callback-top',
                               'extern int archive_leaf(void); int archive_top(void){return archive_leaf();}')
        callback = self.compile('archive-callback-definition',
                                'int archive_callback(void){return 42;}')
        start = self.assemble('archive-callback-start',
                              '.text\n.globl _start\n_start: call archive_top; mov %eax,%edi; mov $60,%eax; syscall')
        leaf = self.root/'libarchive_callback_leaf.so'
        top = self.root/'libarchive_callback_top.so'
        run('ld', '-shared', '-soname', leaf.name, '-o', leaf, leaf_obj)
        run('ld', '-shared', '-soname', top.name, '-o', top, top_obj, leaf)
        flags = ('-pie', '--dynamic-linker', fixtures.INTERPRETER,
                 '--no-allow-shlib-undefined', '--gc-sections',
                 '-rpath-link', self.root, '-rpath', str(self.root))
        for thin in (False, True):
            archive = self.root/('indirect-callback-thin.a' if thin else 'indirect-callback.a')
            run('ar', 'rcsT' if thin else 'rcs', archive, callback)
            # GNU BFD does not turn indirect DSO references into archive roots,
            # even inside a group. Such callbacks need a regular object,
            # whole-archive, -u, or an explicit direct referring DSO.
            for tool in (core.LINKER, 'ld'):
                for inputs in ((start, top, archive), (start, archive, top),
                               (start, '--start-group', archive, top, '--end-group')):
                    with self.subTest(thin=thin, tool=tool, inputs=inputs):
                        out = self.root/'indirect-archive-rejected'
                        out.write_bytes(b'previous output')
                        before = out.stat()
                        failure = run(tool, *flags, '-o', out, *inputs, expected=1)
                        self.assertIn(b'archive_callback', failure.stderr)
                        if tool == core.LINKER:
                            self.assertEqual(out.read_bytes(), b'previous output')
                            self.assertEqual(out.stat().st_ino, before.st_ino)
                for inputs in ((start, top, callback),
                               (start, top, '--whole-archive', archive, '--no-whole-archive'),
                               (start, top, '-u', 'archive_callback', archive),
                               (start, top, leaf, archive)):
                    with self.subTest(thin=thin, tool=tool, inputs=inputs):
                        out = self.link('indirect-archive-explicit', *inputs,
                                        options=flags, linker=tool)
                        self.execute(out, library_path=self.root)
                        self.assertIn('archive_callback',
                                      [s['name'] for s in core.elf_symbols(out.read_bytes(), kind=11)])

    def test_origin_needed_expansion_and_per_referrer_paths(self):
        strict = ('-pie', '--dynamic-linker', fixtures.INTERPRETER,
                  '--no-allow-shlib-undefined')
        start = self.assemble('origin-start', '.text\n.globl _start\n_start: call origin_middle; mov %eax,%edi; mov $60,%eax; syscall')
        leaf_obj = self.compile('origin-leaf', 'int origin_leaf(void){return 42;}')
        middle_obj = self.compile('origin-middle', 'extern int origin_leaf(void); int origin_middle(void){return origin_leaf();}')
        directory = self.root/'origin tree'
        directory.mkdir()
        middle = directory/'liborigin_middle.so'
        for token in ('$ORIGIN/leaf.so', '${ORIGIN}-leaf.so'):
            leaf = directory/'leaf.so' if '/' in token else self.root/'origin tree-leaf.so'
            run('ld', '-shared', '-soname', token, '-o', leaf, leaf_obj)
            run('ld', '-shared', '-soname', middle.name, '-o', middle, middle_obj, leaf)
            for consumer in self.tools():
                with self.subTest(token=token, consumer=consumer):
                    policy = strict if consumer == core.LINKER else (*strict[:3], '--allow-shlib-undefined')
                    out = self.link('origin-program', start, middle, linker=consumer,
                                    options=(*policy, '-rpath', str(directory)))
                    self.execute(out)
        for token in ('$ORIGINsuffix', '$ORIGIN_suffix', '$ORIGIN0'):
            run('ld', '-shared', '-soname', token, '-o', leaf, leaf_obj)
            run('ld', '-shared', '-soname', middle.name, '-o', middle, middle_obj, leaf)
            self.rejected(start, 'unsupported dependency path token', middle, options=strict)

        # Identical token spellings in different referring directories identify
        # different files, even when their SONAMEs have identical spellings.
        middles = []
        for n, value in enumerate((20, 22)):
            directory = self.root/('origin-provider-'+str(n))
            directory.mkdir()
            leaf = self.root/(directory.name+'-leaf.so')
            leaf_obj = self.compile('origin-provider-leaf-'+str(n),
                                    f'int origin_leaf_{n}(void){{return {value};}}')
            run('ld', '-shared', '-soname', '${ORIGIN}-leaf.so',
                '-o', leaf, leaf_obj)
            middle_obj = self.compile('origin-provider-middle-'+str(n),
                                      f'extern int origin_leaf_{n}(void); int origin_middle_{n}(void){{return origin_leaf_{n}();}}')
            middle = directory/('liborigin_middle_'+str(n)+'.so')
            run('ld', '-shared', '-soname', middle.name, '-o', middle, middle_obj, leaf)
            middles.append(middle)
        start = self.assemble('origin-two-providers', '.text\n.globl _start\n_start: call origin_middle_0; push %rax; call origin_middle_1; pop %rdi; add %eax,%edi; mov $60,%eax; syscall')
        for consumer in self.tools():
            policy = strict if consumer == core.LINKER else (*strict[:3], '--allow-shlib-undefined')
            out = self.link('origin-two-program', start, *middles, linker=consumer,
                            options=(*policy, '-rpath', ':'.join(str(p.parent) for p in middles)))
            self.execute(out)

    def test_malformed_dependency_version_requirements_are_bounded(self):
        obj = self.compile('requirement-leaf', 'int dependency(void){return 42;}')
        leaf = self.root/'librequirement_leaf.so'
        script = self.root/'requirement.map'
        script.write_text('CHECK_1 { global: dependency; };')
        run('ld', '-shared', '-soname', leaf.name, '--version-script', script,
            '-o', leaf, obj)
        consumer = self.compile('requirement-middle', 'extern int dependency(void); int answer(void){return dependency();}')
        middle = self.root/'librequirement_middle.so'
        run('ld', '-shared', '-soname', middle.name, '-o', middle, consumer, leaf)
        original = middle.read_bytes()
        section = next(s for s in core.elf_sections(original) if s['kind']==0x6ffffffe)
        at = section['offset']
        aux = at + struct.unpack_from('<I', original, at+8)[0]
        edits = [(at, '<H', 2, 'invalid version requirement'),
                 (at+2, '<H', 0, 'invalid version requirement'),
                 (at+8, '<I', 0, 'overlapping version requirement'),
                 (at+12, '<I', 16, 'unterminated version requirement'),
                 (aux, '<I', 0, 'invalid version requirement auxiliary'),
                 (aux+6, '<H', 1, 'invalid version requirement auxiliary'),
                 (aux+12, '<I', 16, 'unterminated version requirement auxiliary')]
        for offset, fmt, value, diagnostic in edits:
            with self.subTest(offset=offset):
                changed = bytearray(original)
                struct.pack_into(fmt, changed, offset, value)
                middle.write_bytes(changed)
                self.rejected(self.simple, diagnostic, middle,
                              options=('-pie',))
        middle.write_bytes(original)

    def test_multi_provider_version_table_layouts_and_overlaps(self):
        leaves = []
        for n, value in enumerate((20, 22)):
            obj = self.compile('version-layout-leaf-'+str(n),
                               f'int layout_leaf_{n}(void){{return {value};}}')
            policy = self.root/('version-layout-'+str(n)+'.map')
            policy.write_text(f'LAYOUT_{n} {{ global: layout_leaf_{n}; }};')
            leaf = self.root/('liblayout_leaf_'+str(n)+'.so')
            run('ld', '-shared', '-soname', leaf.name, '--version-script', policy,
                '-o', leaf, obj)
            leaves.append(leaf)
        obj = self.compile('version-layout-middle', 'extern int layout_leaf_0(void),layout_leaf_1(void); int layout_middle(void){return layout_leaf_0()+layout_leaf_1();}')
        start = self.assemble('version-layout-start', '.text\n.globl _start\n_start: call layout_middle; mov %eax,%edi; mov $60,%eax; syscall')
        strict = ('-pie', '--dynamic-linker', fixtures.INTERPRETER,
                  '--no-allow-shlib-undefined', '-rpath-link', self.root, '-rpath', '$ORIGIN')
        for producer in self.tools():
            middle = self.root/'liblayout_middle.so'
            run(producer, '-shared', '-soname', middle.name, '-o', middle, obj, *leaves)
            for consumer in self.tools():
                with self.subTest(producer=producer, consumer=consumer):
                    out = self.link('version-layout-program', start, middle,
                                    linker=consumer, options=strict)
                    self.execute(out, library_path=self.root)
            original = middle.read_bytes()
            section = next(s for s in core.elf_sections(original) if s['kind']==0x6ffffffe)
            first = section['offset']
            auxiliary = first+struct.unpack_from('<I', original, first+8)[0]
            changed = bytearray(original)
            # Point the second header into the first header's auxiliary record.
            struct.pack_into('<I', changed, first+12, auxiliary-first)
            middle.write_bytes(changed)
            self.rejected(start, 'overlapping version requirements', middle, options=strict)
            middle.write_bytes(original)

    def test_strict_cycles_indirect_callbacks_gc_and_bounded_reads(self):
        leaf_obj = self.compile('callback-leaf', 'extern int callback(void); int leaf(void){return callback();}')
        middle_obj = self.compile('callback-middle', 'extern int leaf(void); int middle(void){return leaf();}')
        callback = self.compile('indirect-callback', 'int callback(void){return 42;}')
        start = self.assemble('callback-search-start', '.text\n.globl _start\n_start: call middle; mov %eax,%edi; mov $60,%eax; syscall')
        leaf = self.root / 'libcallback_leaf.so'
        middle = self.root / 'libcallback_middle.so'
        run(core.LINKER, '-shared', '-soname', leaf.name, '-o', leaf, leaf_obj)
        run(core.LINKER, '-shared', '-soname', middle.name, '-o', middle, middle_obj, leaf)
        strict = ('-pie', '--dynamic-linker', fixtures.INTERPRETER,
                  '--no-allow-shlib-undefined', '-rpath-link', self.root, '--gc-sections')
        out = self.link('callback-search', start, callback, middle, options=strict)
        self.assertIn('callback', [s['name'] for s in core.elf_symbols(out.read_bytes(), kind=11)])
        self.execute(out, library_path=self.root)
        self.rejected(start, 'unresolved shared-library reference callback', middle, options=strict)
        hidden = self.compile('hidden-indirect-callback', '__attribute__((visibility("hidden"))) int callback(void){return 42;}')
        self.rejected(start, 'unresolved shared-library reference callback', hidden, middle, options=strict)
        wrong_tls = self.compile('tls-indirect-callback', '__thread int callback;')
        self.rejected(start, 'TLS type mismatch', wrong_tls, middle, options=strict)
        self.rejected(start, '4096 directories', callback, middle,
                      options=(*strict, '--rpath-link='+':'*4096))

        # Known identities terminate cycles; all references still get checked.
        a_obj = self.compile('cycle-a', 'extern int b(void); int (*forward)(void)=b; int a(void){return 42;}')
        b_obj = self.compile('cycle-b', 'extern int a(void); int (*back)(void)=a; int b(void){return 1;}')
        a, b = self.root/'libcycle_a.so', self.root/'libcycle_b.so'
        run('ld', '-shared', '-soname', b.name, '-o', b, b_obj)
        run('ld', '-shared', '-soname', a.name, '-o', a, a_obj, b)
        run('ld', '-shared', '-soname', b.name, '-o', b, b_obj, a)
        cycle_start = self.assemble('cycle-start', '.text\n.globl _start\n_start: call a; mov %eax,%edi; mov $60,%eax; syscall')
        out = self.link('cycle-search', cycle_start, a, options=strict)
        self.execute(out, library_path=self.root)

        # Named pipes are rejected without blocking; sparse oversized inputs fail
        # before allocating their advertised contents.
        other = self.root/'bad-dependencies'
        other.mkdir()
        bad = other/leaf.name
        os.mkfifo(bad)
        self.rejected(start, 'regular file', callback, middle,
                      options=('-pie', '--no-allow-shlib-undefined', '-rpath-link', other))
        bad.unlink()
        with bad.open('wb') as f:
            f.truncate(128*1024*1024+1)
        self.rejected(start, 'byte limit', callback, middle,
                      options=('-pie', '--no-allow-shlib-undefined', '-rpath-link', other))


if __name__ == '__main__':
    unittest.main(verbosity=2)
