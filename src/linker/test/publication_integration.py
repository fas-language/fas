#!/usr/bin/env python3
"""Creation masks for CLI/native output, atomic replacement and concurrent APIs."""
import os
from pathlib import Path
import subprocess
import stat
import sys
import unittest

import integration as core

API = str(Path(sys.argv.pop(1)).resolve())
COMPILER = str(Path(sys.argv.pop(1)).resolve())


class PublicationTests(unittest.TestCase):
    setUpClass = classmethod(core.LinkerTests.setUpClass.__func__)
    tearDownClass = classmethod(core.LinkerTests.tearDownClass.__func__)
    assemble = classmethod(core.LinkerTests.assemble.__func__)

    def masked(self, mask, *command, expected=0):
        result = subprocess.run([str(arg) for arg in command], capture_output=True,
                                timeout=30, preexec_fn=lambda: os.umask(mask))
        self.assertEqual(result.returncode, expected, result.stderr.decode(errors='replace'))
        return result

    def test_cli_output_and_map_creation_replacement_and_failure_masks(self):
        for mask in (0o022, 0o027, 0o077):
            for mode, flags, permissions in (
                ('executable', (), 0o755), ('partial', ('-r',), 0o644),
                ('shared', ('-shared',), 0o644),
            ):
                with self.subTest(mask=oct(mask), mode=mode):
                    out = self.root / f'masked-{mask}-{mode}'
                    map_path = self.root / f'masked-{mask}-{mode}.map'
                    map_flags = ('-Map', map_path) if mode != 'partial' else ()
                    command = (core.LINKER, *flags, *map_flags, '-o', out,
                               self.simple, self.answer)
                    for replacement in (False, True):
                        if replacement:
                            out.chmod(0o666)
                            if map_flags:
                                map_path.chmod(0o666)
                        self.masked(mask, *command)
                        self.assertEqual(out.stat().st_mode & 0o777, permissions & ~mask)
                        if map_flags:
                            self.assertEqual(map_path.stat().st_mode & 0o777, 0o644 & ~mask)
                    previous = out.read_bytes()
                    out.chmod(0o640)
                    self.masked(mask, *command, self.answer, expected=1)
                    self.assertEqual(out.read_bytes(), previous)
                    self.assertEqual(out.stat().st_mode & 0o777, 0o640)
        self.assertEqual(list(self.root.glob('.fas-ld-*.tmp')), [])

    def test_native_object_and_executable_use_the_same_creation_policy(self):
        source = self.root / 'masked-native.fas'
        source.write_text('fn main() i32 { return 0 }\n')
        for mask in (0o027, 0o077):
            for flags, permissions in ((('-c',), 0o644), ((), 0o755)):
                out = self.root / ('native-' + str(mask) + ('-object' if flags else '-exe'))
                self.masked(mask, COMPILER, *flags, source, '-o', out)
                self.assertEqual(out.stat().st_mode & 0o777, permissions & ~mask)

    def test_concurrent_publication_does_not_change_process_umask(self):
        for mask in (0o027, 0o077):
            directory = self.root / ('domains-' + str(mask))
            directory.mkdir()
            self.masked(mask, API, directory, str(mask))
            self.assertEqual(list(directory.glob('.fas-ld-*.tmp')), [])

    def test_null_output_validates_without_replacing_the_device(self):
        null = Path('/dev/null')
        before = null.stat()
        self.assertTrue(stat.S_ISCHR(before.st_mode))
        alias = self.root/'discard-output'
        alias.symlink_to(null)
        alias_inode = alias.lstat().st_ino
        for output in (null, alias):
            for flags in ((), ('-r',), ('-shared',), ('-pie',)):
                with self.subTest(output=output, flags=flags):
                    core.run(core.LINKER, *flags, '-o', output, self.simple, self.answer)
                    failure = core.run(core.LINKER, *flags, '-o', output,
                                       self.simple, self.answer, self.answer, expected=1)
                    self.assertIn(b'duplicate symbol answer', failure.stderr)
        # The binary and map can independently be discarded, but two aliases
        # of the same output still violate the distinct-output contract.
        program = self.root/'discard-map-program'
        core.run(core.LINKER, '-o', program, '-Map', null, self.simple, self.answer)
        core.run(program, expected=42)
        report = self.root/'discard-binary.map'
        core.run(core.LINKER, '-o', null, '-Map', report, self.simple, self.answer)
        self.assertIn('answer', report.read_text())
        rejected = core.run(core.LINKER, '-o', null, '-Map', alias,
                            self.simple, self.answer, expected=1)
        self.assertIn(b'different files', rejected.stderr)
        after = null.stat()
        self.assertEqual((after.st_dev, after.st_ino, after.st_mode, after.st_rdev),
                         (before.st_dev, before.st_ino, before.st_mode, before.st_rdev))
        self.assertTrue(alias.is_symlink())
        self.assertEqual(alias.lstat().st_ino, alias_inode)
        self.assertEqual(list(self.root.glob('.fas-ld-*.tmp')), [])

    def test_other_special_outputs_still_fail_without_opening(self):
        fifo = self.root/'publication-pipe'
        os.mkfifo(fifo)
        for output in (fifo, Path('/dev/zero'), self.root):
            with self.subTest(output=output):
                result = core.run(core.LINKER, '-o', output, self.simple,
                                  self.answer, expected=1)
                self.assertIn(b'output must be a regular file', result.stderr)
        self.assertTrue(stat.S_ISFIFO(fifo.stat().st_mode))
        self.assertEqual(list(self.root.glob('.fas-ld-*.tmp')), [])


if __name__ == '__main__':
    unittest.main(verbosity=2)
