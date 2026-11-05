#!/usr/bin/env python3
"""Real GNU thin archives: lazy IO, relative paths, nesting and publication."""
import json
import os
from pathlib import Path
import shutil
import subprocess
import unittest

import archive_integration as fixtures

core = fixtures.core
run = core.run


class ThinTests(unittest.TestCase):
    setUpClass = classmethod(core.LinkerTests.setUpClass.__func__)
    tearDownClass = classmethod(core.LinkerTests.tearDownClass.__func__)
    assemble = classmethod(core.LinkerTests.assemble.__func__)
    link = core.LinkerTests.link
    rejected = core.LinkerTests.rejected

    def archive(self, name, *objects, indexed=True, thin=True):
        output = self.root/(name+'.a')
        command = ['ar', ('rcs' if indexed else 'rcS')+('T' if thin else ''), str(output)]
        command.extend(os.path.relpath(obj, self.root) for obj in objects)
        result = subprocess.run(command, cwd=self.root, capture_output=True, timeout=10)
        self.assertEqual(result.returncode, 0, result.stderr)
        return output

    def tools(self):
        return [core.LINKER, 'ld'] + (['ld.lld-22'] if shutil.which('ld.lld-22') else [])

    def test_relative_paths_long_names_and_moved_tree(self):
        directory = self.root/'object files with spaces'
        directory.mkdir(exist_ok=True)
        object_ = directory/'long answer object name.o'
        shutil.copyfile(self.answer, object_)
        library = self.archive('relative-thin', object_)
        self.assertTrue(library.read_bytes().startswith(b'!<thin>\n'))
        self.assertNotIn(str(self.root).encode(), library.read_bytes())
        moved = self.root/'moved-tree'
        moved.mkdir()
        shutil.copytree(directory, moved/directory.name)
        shutil.copyfile(library, moved/library.name)
        # The moved archive resolves its own neighboring object, not the CWD.
        object_.unlink()
        for tool in self.tools():
            out = self.link('relative-thin-exe', self.simple, moved/library.name, linker=tool)
            run(out, expected=42)

    def test_lazy_missing_and_changed_members_and_index_validation(self):
        absent = self.assemble('thin-absent', '.globl unused\n.data\nunused: .quad 9')
        library = self.archive('thin-lazy', absent, self.answer)
        absent.unlink()
        # GNU ld, like Fas, does not open unselected thin members. LLD 22 does.
        for tool in (core.LINKER, 'ld'):
            run(self.link('thin-lazy-exe', self.simple, library, linker=tool), expected=42)
        if shutil.which('ld.lld-22'):
            result = run('ld.lld-22', '-o', self.root/'thin-lld-missing',
                         self.simple, library, expected=1)
            self.assertIn(b'No such file', result.stderr)
        self.rejected(self.simple, 'No such file', library, options=('--whole-archive',))
        changed = self.assemble('thin-changed', '.globl answer\nanswer: mov $7,%eax; ret')
        library = self.archive('thin-updated', changed)
        # A thin archive retains references, not a snapshot of member contents.
        changed.write_bytes(self.answer.read_bytes())
        for tool in self.tools():
            run(self.link('thin-updated-exe', self.simple, library, linker=tool), expected=42)
        unrelated = self.assemble('thin-wrong', '.globl other\nother: ret')
        changed.write_bytes(unrelated.read_bytes())
        self.rejected(self.simple, 'missing definition answer', library)
        noindex = self.archive('thin-no-index', self.answer, indexed=False)
        self.rejected(self.simple, 'no symbol index', noindex)
        for tool in self.tools():
            run(self.link('thin-whole', self.simple, noindex, linker=tool,
                          options=('--whole-archive',)), expected=42)

    def test_nested_regular_archives_and_flattened_thin_archives(self):
        last = self.assemble('thin-last', '.globl last\nlast: mov $42,%eax; ret')
        middle = self.assemble('thin-middle', '.globl answer\nanswer: jmp last')
        regular = self.archive('nested-regular', last, middle, thin=False)
        nested = self.archive('nested-thin', regular)
        flattened = self.archive('flattened-thin', nested)
        for tool in (core.LINKER, 'ld'):
            for library in (nested, flattened):
                run(self.link('nested-thin-exe', self.simple, library, linker=tool), expected=42)
        if shutil.which('ld.lld-22'):
            result = run('ld.lld-22', '-o', self.root/'thin-lld-nested',
                         self.simple, nested, expected=1)
            self.assertIn(b'not all decimal numbers', result.stderr)
        report = run(core.LINKER, '--stats', '-o', self.root/'thin-stats', self.simple, nested)
        counts = json.loads(report.stderr)['counts']
        self.assertEqual(counts['thin_member_files'], 1)
        self.assertEqual(counts['extracted_members'], 2)
        # Bad nested offsets are diagnosed, never interpreted as object data.
        data = nested.read_bytes()
        at = data.rfind(b'/0:')
        self.assertGreater(at, 0)
        broken = self.root/'thin-bad-offset.a'
        broken.write_bytes(data[:at]+b'/0:9'.ljust(16)+data[at+16:])
        self.rejected(self.simple, 'nested member header offset', broken)

    def test_consumed_and_unused_external_members_are_not_overwritten(self):
        member = self.assemble('thin-alias-answer', '.globl answer\nanswer: mov $42,%eax; ret')
        unused = self.assemble('thin-alias-unused', '.globl unused_alias\nunused_alias: ret')
        library = self.archive('thin-alias', member, unused)
        for target in (member, unused):
            original = target.read_bytes()
            for map_file in (False, True):
                flags = ('-Map', target, '-o', self.root/'thin-safe') if map_file else ('-o', target)
                result = run(core.LINKER, *flags, self.simple, library, expected=1)
                self.assertIn(b'overwrite input', result.stderr)
                self.assertEqual(target.read_bytes(), original)
        alias = self.root/'thin-hardlink-alias'
        os.link(member, alias)
        run(core.LINKER, '-o', alias, self.simple, library, expected=1)
        self.assertEqual(alias.read_bytes(), member.read_bytes())

    def test_nonregular_and_oversized_external_members(self):
        member = self.assemble('thin-invalid-member', '.globl answer\nanswer: ret')
        library = self.archive('thin-invalid', member)
        member.unlink()
        os.mkfifo(member)
        self.rejected(self.simple, 'regular file', library)
        member.unlink()
        with member.open('wb') as stream:
            stream.truncate(128*1024*1024+1)
        self.rejected(self.simple, 'input exceeds', library)


if __name__ == '__main__':
    unittest.main(verbosity=2)
