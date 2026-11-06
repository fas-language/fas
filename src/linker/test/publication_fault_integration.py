#!/usr/bin/env python3
"""Inject real publication I/O failures without altering the host filesystem."""
import os
from pathlib import Path
import select
import signal
import resource
import subprocess
import unittest
import sys

import integration as core

run = core.run
IO_REENTRY = str(Path(sys.argv.pop(1)).resolve())


class PublicationFaultTests(unittest.TestCase):
    setUpClass = classmethod(core.LinkerTests.setUpClass.__func__)
    tearDownClass = classmethod(core.LinkerTests.tearDownClass.__func__)
    assemble = classmethod(core.LinkerTests.assemble.__func__)

    def setUp(self):
        self.directory = self.root/self._testMethodName
        self.directory.mkdir()
        self.shim = self.directory/'publication-faults.so'
        source = Path(__file__).with_name('publication_faults.c')
        run('gcc', '-Wall', '-Wextra', '-Werror', '-O2', '-shared', '-fPIC',
            source, '-ldl', '-o', self.shim)

    def env(self, fault, ordinal=1, **extra):
        env = dict(os.environ, LD_PRELOAD=str(self.shim), FAS_IO_FAULT=fault,
                   FAS_IO_ORDINAL=str(ordinal))
        env.update(extra)
        return env

    def seed(self, path, data=b'previous artifact\0\xff'):
        path.write_bytes(data)
        path.chmod(0o640)
        return data, path.stat().st_mode, path.stat().st_ino

    def unchanged(self, path, expected):
        self.assertEqual((path.read_bytes(), path.stat().st_mode, path.stat().st_ino), expected)

    def invoke(self, command, fault, ordinal=1, expected=1):
        p = subprocess.run([str(arg) for arg in command], env=self.env(fault, ordinal),
                           capture_output=True, timeout=20)
        self.assertEqual(p.returncode, expected, p.stderr)
        self.assertNotIn(b'Fatal error', p.stderr)
        if expected:
            self.assertIn(b'fas-ld:', p.stderr)
        self.assertEqual(list(self.directory.glob('.fas-ld-*.tmp')), [])
        return p

    def test_write_close_chmod_and_rename_failures_preserve_existing_outputs(self):
        for flags in ((), ('-r',), ('-shared',), ('-static-pie',)):
            for fault in ('write-enospc', 'write-partial-eio', 'close-eio',
                          'chmod-eperm', 'rename-eio'):
                for existing in (False, True):
                    with self.subTest(flags=flags, fault=fault, existing=existing):
                        out = self.directory/'image'
                        out.unlink(missing_ok=True)
                        previous = self.seed(out) if existing else None
                        p = self.invoke((core.LINKER, *flags, '-o', out,
                                         self.simple, self.answer), fault)
                        if fault == 'write-enospc':
                            self.assertIn(b'No space left', p.stderr)
                        if existing:
                            self.unchanged(out, previous)
                        else:
                            self.assertFalse(out.exists())

    def test_short_and_interrupted_writes_retry_without_corrupting_images(self):
        for flags in ((), ('-r',), ('-shared',), ('-static-pie',)):
            expected = self.directory/'reference'
            run(core.LINKER, *flags, '-o', expected, self.simple, self.answer)
            for fault in ('write-short', 'write-eintr'):
                with self.subTest(flags=flags, fault=fault):
                    out = self.directory/'image'
                    self.invoke((core.LINKER, *flags, '-o', out,
                                 self.simple, self.answer), fault, expected=0)
                    self.assertEqual(out.read_bytes(), expected.read_bytes())
                    self.assertEqual(run('readelf', '-aW', out).stderr, b'')
                    if not flags:
                        run(out, expected=42)

    def test_input_short_and_interrupted_reads_retry_without_changing_inputs(self):
        before = self.simple.read_bytes()
        for flags in ((), ('-r',), ('-shared',), ('-static-pie',)):
            reference = self.directory/'reference'
            run(core.LINKER, *flags, '-o', reference, self.simple, self.answer)
            for fault in ('read-short', 'read-eintr'):
                with self.subTest(flags=flags, fault=fault):
                    out = self.directory/'image'
                    p = subprocess.run([core.LINKER, *flags, '-o', str(out),
                        str(self.simple), str(self.answer)], capture_output=True, timeout=20,
                        env=self.env(fault, FAS_IO_INPUT=str(self.simple)))
                    self.assertEqual(p.returncode, 0, p.stderr)
                    self.assertEqual(out.read_bytes(), reference.read_bytes())
                    self.assertEqual(self.simple.read_bytes(), before)

    def test_input_failures_preserve_outputs_and_allow_library_reentry(self):
        before = self.simple.read_bytes()
        for fault in ('read-eio', 'read-eof'):
            for flags in ((), ('-r',), ('-shared',), ('-static-pie',)):
                with self.subTest(fault=fault, flags=flags):
                    out = self.directory/'image'
                    old = self.seed(out)
                    p = subprocess.run([core.LINKER, *flags, '-o', str(out),
                        str(self.simple), str(self.answer)], capture_output=True, timeout=20,
                        env=self.env(fault, FAS_IO_INPUT=str(self.simple)))
                    self.assertEqual(p.returncode, 1, p.stderr)
                    self.assertIn(b'fas-ld:', p.stderr)
                    self.assertIn(str(self.simple).encode(), p.stderr)
                    self.assertNotIn(b'Fatal error', p.stderr)
                    self.unchanged(out, old)
                    self.assertEqual(list(self.directory.glob('.fas-ld-*.tmp')), [])
            def limit_descriptors():
                resource.setrlimit(resource.RLIMIT_NOFILE, (32, 32))
            p = subprocess.run([IO_REENTRY, str(self.simple), str(self.answer)],
                capture_output=True, timeout=20, preexec_fn=limit_descriptors,
                env=self.env(fault, FAS_IO_INPUT=str(self.simple)))
            self.assertEqual(p.returncode, 0, p.stderr)
            self.assertIn(b'256 failures, no descriptor leak', p.stdout)
        self.assertEqual(self.simple.read_bytes(), before)

    def test_temp_creation_resource_errors_and_name_collision_retry(self):
        for flags in ((), ('-r',), ('-shared',), ('-static-pie',)):
            for fault in ('create-emfile', 'create-enospc', 'create-eacces'):
                for existing in (False, True):
                    with self.subTest(flags=flags, fault=fault, existing=existing):
                        out = self.directory/'image'
                        out.unlink(missing_ok=True)
                        old = self.seed(out) if existing else None
                        self.invoke((core.LINKER, *flags, '-o', out,
                                     self.simple, self.answer), fault)
                        if existing:
                            self.unchanged(out, old)
                        else:
                            self.assertFalse(out.exists())
            out = self.directory/'image'
            self.invoke((core.LINKER, *flags, '-o', out, self.simple, self.answer),
                        'create-collision', expected=0)
            if not flags:
                run(out, expected=42)

    def test_real_file_size_limit_preserves_previous_artifact(self):
        def limit_size():
            signal.signal(signal.SIGXFSZ, signal.SIG_IGN)
            resource.setrlimit(resource.RLIMIT_FSIZE, (0, 0))
        for flags in ((), ('-r',), ('-shared',), ('-static-pie',)):
            with self.subTest(flags=flags):
                out = self.directory/'image'
                old = self.seed(out)
                p = subprocess.run([core.LINKER, *flags, '-o', str(out),
                    str(self.simple), str(self.answer)], capture_output=True,
                    timeout=20, preexec_fn=limit_size)
                self.assertEqual(p.returncode, 1, p.stderr)
                self.assertIn(b'fas-ld:', p.stderr)
                self.assertNotIn(b'Fatal error', p.stderr)
                self.unchanged(out, old)
                self.assertEqual(list(self.directory.glob('.fas-ld-*.tmp')), [])

    def test_map_and_image_failures_follow_individual_file_atomicity(self):
        reference = self.directory/'reference'
        reference_map = self.directory/'reference.map'
        run(core.LINKER, '-o', reference, '-Map', reference_map, self.simple, self.answer)
        for fault in ('write-enospc', 'write-partial-eio', 'close-eio',
                      'chmod-eperm', 'rename-eio'):
            for ordinal in (1, 2):
                with self.subTest(fault=fault, ordinal=ordinal):
                    out = self.directory/'image'
                    map_path = self.directory/'image.map'
                    old_image = self.seed(out)
                    old_map = self.seed(map_path, b'old map\n')
                    self.invoke((core.LINKER, '-o', out, '-Map', map_path,
                                 self.simple, self.answer), fault, ordinal)
                    self.unchanged(out, old_image)
                    if ordinal == 1:
                        self.unchanged(map_path, old_map)
                    else:
                        # The map publishes before the image. This is a declared
                        # two-file limitation, not a transaction guarantee.
                        self.assertEqual(map_path.read_bytes(), reference_map.read_bytes())

    def test_killed_writer_keeps_output_and_leaves_only_private_incomplete_temp(self):
        out = self.directory/'image'
        old_image = self.seed(out)
        read_fd, write_fd = os.pipe()
        process = None
        try:
            env = self.env('stop-during-write', FAS_IO_READY_FD=str(write_fd))
            process = subprocess.Popen([core.LINKER, '-o', str(out),
                                        str(self.simple), str(self.answer)],
                                       env=env, pass_fds=(write_fd,),
                                       stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            os.close(write_fd)
            write_fd = None
            ready, _, _ = select.select([read_fd], [], [], 10)
            self.assertTrue(ready, 'writer did not reach the injected interruption')
            self.assertEqual(os.read(read_fd, 1), b'R')
            temporary = list(self.directory.glob('.fas-ld-*.tmp'))
            self.assertEqual(len(temporary), 1)
            self.assertEqual(temporary[0].stat().st_mode & 0o777, 0o600)
            self.assertEqual(temporary[0].stat().st_size, 17)
            self.unchanged(out, old_image)
            process.kill()
            process.communicate(timeout=10)
            self.assertEqual(process.returncode, -signal.SIGKILL)
            self.unchanged(out, old_image)
            # A later invocation must neither consume nor delete another
            # invocation's temporary. No automatic crash cleanup is promised.
            run(core.LINKER, '-o', out, self.simple, self.answer)
            run(out, expected=42)
            self.assertTrue(temporary[0].is_file())
            temporary[0].unlink()
        finally:
            if process is not None and process.poll() is None:
                process.kill()
                process.communicate(timeout=10)
            os.close(read_fd)
            if write_fd is not None:
                os.close(write_fd)


if __name__ == '__main__':
    unittest.main(verbosity=2)
