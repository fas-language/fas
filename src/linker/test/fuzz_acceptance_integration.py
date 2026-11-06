"""Negative controls for campaign acceptance; no fuzzer or generated code runs."""
import copy
import importlib.util
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

VERIFY = Path(sys.argv.pop(1)).resolve()
spec = importlib.util.spec_from_file_location('fuzz_verify', VERIFY)
verify = importlib.util.module_from_spec(spec)
spec.loader.exec_module(verify)


class FuzzAcceptanceTests(unittest.TestCase):
    def setUp(self):
        self.head = 'a' * 40
        self.report = {'seconds_per_target': 600, 'modes': ['elf', 'link'],
                       'git_head': self.head, 'git_status': '', 'target_sha256': 'b' * 64,
                       'targets': {mode: {'exit_code': 0, 'elapsed_seconds': 602.5,
                                         'stats': {'run_time': '600', 'execs_done': '1234',
                                                   'saved_crashes': '0', 'saved_hangs': '0',
                                                   'edges_found': '123', 'total_edges': '4096'}}
                                   for mode in ('elf', 'link')}}

    def check(self, data, passed=False):
        result = verify.validate(data, seconds=600, modes=['elf', 'link'], head=self.head)
        self.assertEqual(result['status'], 'passed' if passed else 'failed', result)
        self.assertEqual(bool(result['failures']), not passed)
        return result

    def test_complete_selection_and_whole_second_rounding(self):
        self.report['targets']['elf']['stats']['run_time'] = '599'
        result = self.check(self.report, passed=True)
        self.assertEqual(result['executions'], 2468)
        self.assertEqual(set(result['targets']), {'elf', 'link'})

    def test_short_interrupted_missing_and_mismatched_selections(self):
        mutations = [lambda d: d.update(seconds_per_target=2),
                     lambda d: d.update(modes=['elf', 'elf']),
                     lambda d: d['targets'].pop('link'),
                     lambda d: d['targets']['elf'].update(exit_code=1),
                     lambda d: d['targets']['elf'].update(elapsed_seconds=599.9),
                     lambda d: d['targets']['elf']['stats'].update(run_time='598'),
                     lambda d: d.update(git_head='c' * 40),
                     lambda d: d.update(git_status=' M src/linker/elf_reader.ml\n'),
                     lambda d: d.update(target_sha256='unknown')]
        for index, mutate in enumerate(mutations):
            with self.subTest(mutation=index):
                data = copy.deepcopy(self.report)
                mutate(data)
                self.check(data)

    def test_missing_metrics_and_saved_findings_fail_closed(self):
        for key in self.report['targets']['elf']['stats']:
            for value in (None, '-1', 'invalid', True):
                with self.subTest(key=key, value=value):
                    data = copy.deepcopy(self.report)
                    if value is None:
                        data['targets']['elf']['stats'].pop(key)
                    else:
                        data['targets']['elf']['stats'][key] = value
                    self.check(data)
        for key, value in [('execs_done', '0'), ('saved_crashes', '1'),
                           ('saved_hangs', '1'), ('edges_found', '0'), ('total_edges', '1')]:
            with self.subTest(key=key, value=value):
                data = copy.deepcopy(self.report)
                data['targets']['elf']['stats'][key] = value
                self.check(data)

    def test_cli_rejects_upload_smoke_as_full_campaign(self):
        with tempfile.TemporaryDirectory(prefix='fas-fuzz-acceptance-') as name:
            path, output = Path(name)/'report.json', Path(name)/'verification.json'
            data = copy.deepcopy(self.report)
            data['seconds_per_target'] = 2
            for target in data['targets'].values():
                target['elapsed_seconds'] = 4.5
                target['stats']['run_time'] = '2'
            path.write_text(json.dumps(data))
            command = [sys.executable, str(VERIFY), '--report', str(path), '--modes', 'elf', 'link',
                       '--head', self.head, '--output', str(output)]
            smoke = subprocess.run([*command, '--seconds', '2'], capture_output=True, text=True)
            self.assertEqual(smoke.returncode, 0, smoke.stderr)
            full = subprocess.run([*command, '--seconds', '600'], capture_output=True, text=True)
            self.assertEqual(full.returncode, 1, full.stderr)
            self.assertEqual(json.loads(output.read_text())['status'], 'failed')
            path.write_text('not JSON')
            invalid = subprocess.run([*command, '--seconds', '600'], capture_output=True, text=True)
            self.assertEqual(invalid.returncode, 1)
            self.assertNotIn('Traceback', invalid.stderr)


if __name__ == '__main__':
    unittest.main(verbosity=2)
