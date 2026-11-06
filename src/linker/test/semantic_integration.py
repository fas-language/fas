"""Valid-program generator and its deliberately incorrect-oracle control."""
import importlib.util
import json
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import unittest
from unittest import mock

HARNESS = Path(sys.argv.pop(1)).resolve()
LINKER = Path(sys.argv.pop(1)).resolve()


class SemanticTests(unittest.TestCase):
    def test_generated_valid_programs_match_oracle_through_all_pipelines(self):
        with tempfile.TemporaryDirectory(prefix='fas-semantic-suite-') as temp:
            output = Path(temp)/'campaign'
            lld = shutil.which('ld.lld-22') or shutil.which('ld.lld')
            args = [sys.executable, HARNESS, '--linker', LINKER, '--output', output,
                    '--cases', '2']
            if lld:
                args += ['--lld', lld]
            result = subprocess.run(list(map(str, args)), capture_output=True, timeout=90)
            self.assertEqual(result.returncode, 0, result.stderr)
            report = json.loads((output/'report.json').read_text())
            self.assertEqual(report['status'], 'passed')
            count = 2*4*4*(3 if lld else 2)
            self.assertEqual(report['final_links'], count)
            self.assertEqual(report['executions'], count)
            self.assertEqual(report['partial_links'], 8)
            for case in report['cases']:
                self.assertEqual(case['status'], 'passed')
                self.assertEqual({r['pipeline'] for r in case['runs']},
                                 {'direct', 'archive', 'fas-partial', 'gnu-partial'})
                self.assertEqual({r['mode'] for r in case['runs']},
                                 {'static', 'static-gc', 'static-pie', 'static-pie-gc'})
                self.assertTrue(all(r['status'] == 'passed' for r in case['runs']))

    def test_incorrect_python_oracle_is_rejected_after_real_execution(self):
        spec = importlib.util.spec_from_file_location('fas_semantic', HARNESS)
        harness = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(harness)
        original = harness.program
        def incorrect(seed):
            model = original(seed)
            model['expected'] ^= 1
            return model
        with tempfile.TemporaryDirectory(prefix='fas-semantic-control-') as temp:
            campaign = harness.Campaign(Path(temp)/'campaign', LINKER)
            with mock.patch.object(harness, 'program', side_effect=incorrect):
                with self.assertRaisesRegex(ValueError, 'runtime value differs from Python oracle'):
                    campaign.case(123, modes=('static',), pipelines=('direct',))
            self.assertEqual(campaign.report['final_links'], 1)
            self.assertEqual(campaign.report['executions'], 1)
            commands = [json.loads(line) for line in (campaign.root/'commands.jsonl').read_text().splitlines()]
            self.assertEqual(commands[-1]['exit'], 0)
            self.assertEqual(len(bytes.fromhex(commands[-1]['stdout_hex'])), 8)
            self.assertEqual(campaign.report['cases'], [])


if __name__ == '__main__':
    unittest.main(verbosity=2)
