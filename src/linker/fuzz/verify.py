#!/usr/bin/env python3
"""Check a retained AFL campaign against an explicit duration/target selection."""
import argparse
import hashlib
import json
import math
from pathlib import Path
import re


def validate(report, *, seconds, modes, head=None):
    failures = []
    metrics = {}
    if type(seconds) is not int or seconds < 1:
        raise ValueError('seconds must be a positive integer')
    if not modes or len(set(modes)) != len(modes):
        raise ValueError('expected modes must be nonempty and unique')
    if not isinstance(report, dict):
        raise ValueError('campaign report must be an object')
    if type(report.get('seconds_per_target')) is not int or report['seconds_per_target'] != seconds:
        failures.append('requested duration does not match the acceptance selection')
    selected = report.get('modes')
    if (not isinstance(selected, list) or not all(isinstance(x, str) for x in selected)
            or len(selected) != len(modes) or set(selected) != set(modes)):
        failures.append('requested modes do not match the acceptance selection')
    targets = report.get('targets')
    if not isinstance(targets, dict):
        targets = {}
    if set(targets) != set(modes):
        failures.append('completed targets do not match the acceptance selection')
    target_sha = report.get('target_sha256')
    if not isinstance(target_sha, str) or not re.fullmatch(r'[0-9a-f]{64}', target_sha):
        failures.append('missing or invalid instrumented target identity')
    if head is not None:
        if report.get('git_head') != head:
            failures.append('source commit does not match the acceptance selection')
        if report.get('git_status') != '':
            failures.append('source snapshot is not clean')
    for mode in modes:
        target = targets.get(mode)
        if not isinstance(target, dict):
            failures.append(mode + ': missing target record')
            continue
        if type(target.get('exit_code')) is not int or target['exit_code'] != 0:
            failures.append(mode + ': AFL did not exit successfully')
        elapsed = target.get('elapsed_seconds')
        if (type(elapsed) not in (int, float) or not math.isfinite(elapsed)
                or elapsed < seconds):
            failures.append(mode + ': elapsed time is shorter than the requested duration')
        stats = target.get('stats')
        if not isinstance(stats, dict):
            stats = {}
        values = {}
        for key in ('run_time', 'execs_done', 'saved_crashes', 'saved_hangs',
                    'edges_found', 'total_edges'):
            value = stats.get(key)
            if not isinstance(value, str) or not re.fullmatch(r'[0-9]+', value):
                failures.append(mode + ': missing or invalid AFL statistic ' + key)
            else:
                values[key] = int(value)
        if len(values) != 6:
            continue
        # AFL reports whole seconds. Permit its final sample to round down by
        # one second, while the separately measured elapsed time must meet -V.
        if values['run_time'] < seconds - 1:
            failures.append(mode + ': AFL run time is shorter than the requested duration')
        if values['execs_done'] == 0:
            failures.append(mode + ': no inputs executed')
        if values['edges_found'] == 0 or values['edges_found'] > values['total_edges']:
            failures.append(mode + ': invalid instrumented edge statistics')
        if values['saved_crashes'] or values['saved_hangs']:
            failures.append(mode + ': saved findings require replay and triage')
        metrics[mode] = {**values, 'elapsed_seconds': elapsed}
    return {'status': 'failed' if failures else 'passed', 'failures': failures,
            'seconds_per_target': seconds, 'modes': list(modes),
            'git_head': report.get('git_head'), 'target_sha256': target_sha,
            'targets': metrics,
            'executions': sum(x['execs_done'] for x in metrics.values()),
            'scope': 'bounded campaign acceptance; AFL edge counts are not source coverage'}


def main():
    from run import MODES
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report', type=Path, required=True)
    parser.add_argument('--seconds', type=int, required=True)
    parser.add_argument('--modes', nargs='+', choices=MODES, default=list(MODES))
    parser.add_argument('--head', help='require this source commit and a clean working tree')
    parser.add_argument('--output', type=Path)
    args = parser.parse_args()
    try:
        data = args.report.read_bytes()
        result = validate(json.loads(data), seconds=args.seconds, modes=args.modes, head=args.head)
        result['report_sha256'] = hashlib.sha256(data).hexdigest()
        encoded = json.dumps(result, indent=2) + '\n'
        if args.output:
            args.output.write_text(encoded)
        print(encoded, end='')
        return int(result['status'] != 'passed')
    except (OSError, ValueError) as error:
        parser.exit(1, 'fas-ld fuzz verification: ' + str(error) + '\n')


if __name__ == '__main__':
    raise SystemExit(main())
