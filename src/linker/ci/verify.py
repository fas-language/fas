#!/usr/bin/env python3
"""Require the complete declared correctness selection and record its reports."""
import argparse
import hashlib
import json
from pathlib import Path
import platform
import re
import subprocess


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output', required=True, type=Path)
    parser.add_argument('--expected-python-cases', type=int, default=250)
    args = parser.parse_args()
    if args.expected_python_cases <= 0:
        parser.error('--expected-python-cases must be positive')
    root = args.output.resolve()
    log = root/'linker-regressions.log'
    text = log.read_text()
    counts = [int(x) for x in re.findall(r'^Ran (\d+) tests? in ', text, re.M)]
    skips = [line for line in text.splitlines()
             if re.search(r'\.\.\. skipped [\'"]|^OK \(skipped=', line)]
    reports = {}
    failures = []
    repository = Path(__file__).resolve().parents[3]
    linker_sha = sha(repository/'_build/default/src/linker/bin/main.exe')
    versions = {}
    for tool, option in [('ocamlopt', '-version'), ('dune', '--version'), ('gcc', '--version'),
                         ('clang', '--version'), ('ld', '--version'), ('gdb', '--version'),
                         ('valgrind', '--version'), ('ldd', '--version')]:
        versions[tool] = subprocess.check_output([tool, option], text=True).splitlines()[0]
    if versions['ocamlopt'] != '5.4.1' or versions['dune'] != '3.24.2':
        failures.append('OCaml/Dune versions do not match the declared baseline')
    if sum(counts) != args.expected_python_cases:
        failures.append(f'expected {args.expected_python_cases} Python cases, observed {sum(counts)}')
    if skips:
        failures.append('regression skips present')
    for name in ('linker-scale', 'linker-zlib-dynamic', 'linker-zlib-static',
                 'linker-zlib-static-pie', 'linker-musl-zlib',
                 'linker-chibicc-selfhost', 'linker-sqlite'):
        path = root/name/'report.json'
        data = json.loads(path.read_text())
        if name == 'linker-scale':
            passed = data['passed'] == 4 and data['failed'] == 0
        else:
            passed = bool(data['tests']) and all(t['status'] == 'passed' for t in data['tests'])
        passed = passed and data['linker_sha256'] == linker_sha
        reports[name] = {'path': str(path), 'sha256': sha(path), 'passed': passed}
        if not passed:
            failures.append(name+' report failed')
    installed_sha = sha(root/'linker-install/usr/bin/fas-ld')
    if installed_sha != linker_sha:
        failures.append('installed linker differs from qualified executable')
    summary = {'status': 'failed' if failures else 'passed', 'failures': failures,
               'python_cases': sum(counts), 'python_groups': len(counts), 'skips': skips,
               'regression_log_sha256': sha(log), 'reports': reports,
               'tool_versions': versions, 'platform': platform.platform(),
               'os_release': Path('/etc/os-release').read_text(),
               'packages_sha256': sha(root/'packages.tsv'),
               'linker_sha256': linker_sha, 'installed_linker_sha256': installed_sha,
               'scope': 'shared correctness recipe; no hosted-run or performance claim'}
    (root/'qualification-summary.json').write_text(json.dumps(summary, indent=2)+'\n')
    print(json.dumps(summary, indent=2))
    raise SystemExit(bool(failures))


if __name__ == '__main__':
    main()
