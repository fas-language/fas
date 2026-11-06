#!/usr/bin/env python3
"""Check retained command diagnostics and cleanup of timed-out build trees."""
import importlib.util
import json
import os
from pathlib import Path
import signal
import subprocess
import sys
import tempfile
import time
import unittest

spec = importlib.util.spec_from_file_location('corpus', sys.argv.pop(1))
corpus = importlib.util.module_from_spec(spec)
spec.loader.exec_module(corpus)


class CorpusHarnessTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix='fas-corpus-harness-')
        self.addCleanup(self.temporary.cleanup)
        self.runner = object.__new__(corpus.Corpus)
        self.runner.root = Path(self.temporary.name)

    def rows(self):
        return [json.loads(line) for line in
                (self.runner.root/'commands.jsonl').read_text().splitlines()]

    def test_success_and_failed_commands_retain_output(self):
        result = self.runner.run([sys.executable, '-c',
            'import sys; print("out"); print("err",file=sys.stderr); sys.exit(3)'],
            expected=None)
        self.assertEqual(result.returncode, 3)
        row, = self.rows()
        self.assertEqual((row['stdout'], row['stderr']), ('out\n', 'err\n'))
        self.assertFalse(row['timed_out'])
        with self.assertRaisesRegex(RuntimeError, 'exit 4, expected 0'):
            self.runner.run([sys.executable, '-c', 'raise SystemExit(4)'])
        self.assertEqual(self.rows()[-1]['status'], 4)

    def test_timeout_kills_inherited_process_group_and_retains_diagnostics(self):
        script = ('import os,subprocess,sys,time; '
            'child=subprocess.Popen([sys.executable,"-c","import time; time.sleep(60)"]); '
            'print(os.getpid(),child.pid,flush=True); '
            'print("timeout diagnostic",file=sys.stderr,flush=True); time.sleep(60)')
        with self.assertRaises(subprocess.TimeoutExpired) as caught:
            self.runner.run([sys.executable, '-c', script], timeout=5)
        row, = self.rows()
        self.assertTrue(row['timed_out'])
        self.assertEqual(row['status'], -signal.SIGKILL)
        self.assertIn('timeout diagnostic', row['stderr'])
        self.assertEqual(caught.exception.output.decode(), row['stdout'])
        pids = list(map(int, row['stdout'].split()))
        self.assertEqual(len(pids), 2)
        def alive(pid):
            try:
                # A grandchild awaiting init's reap is already terminated.
                return Path('/proc', str(pid), 'stat').read_text().split(') ')[1][0] != 'Z'
            except FileNotFoundError:
                return False
        try:
            deadline = time.monotonic()+2
            while any(map(alive, pids)) and time.monotonic() < deadline:
                time.sleep(0.01)
            self.assertFalse(any(map(alive, pids)), 'timed-out build left a live descendant')
        finally:
            for pid in pids:
                if alive(pid):
                    os.kill(pid, signal.SIGKILL)


if __name__ == '__main__':
    unittest.main(verbosity=2)
