#!/usr/bin/env python3
"""Warm-cache linker comparisons from explicit, already-qualified link commands.

The manifest contains workloads with name/cwd/args/check fields. Each args list
must contain exactly one '-o PATH'. check accepts args, cwd, exit and stdin (text).
Input compilation and runtime checks are outside the measured link interval.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import platform
import shutil
import statistics
import subprocess
import sys
import time

HELPERS = Path(__file__).resolve().with_name('bench.py')
if not HELPERS.is_file():
    HELPERS = Path(__file__).resolve().parents[1]/'bench.py'
sys.path.insert(0, str(HELPERS.parent))
from bench import measure, measure_memory, version


def digest(path):
    with Path(path).open('rb') as stream:
        return hashlib.file_digest(stream, 'sha256').hexdigest()


def explicit_inputs(args, cwd):
    """Hash explicit argv files; do not imply resolution of -l/script closures."""
    found = {}
    output_index = args.index('-o') + 1
    for index, arg in enumerate(args):
        if index == output_index or arg.startswith('-'):
            continue
        path = Path(arg)
        if not path.is_absolute():
            path = cwd/path
        if path.is_file():
            found[str(path.resolve())] = digest(path)
    return found


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('manifest', type=Path)
    parser.add_argument('--output', required=True, type=Path, help='new artifact directory')
    parser.add_argument('--tool', action='append', required=True, metavar='LABEL=PATH')
    parser.add_argument('--runs', type=int, default=15)
    parser.add_argument('--warmups', type=int, default=3)
    parser.add_argument('--verify-only', action='store_true', help='check replays without collecting timings')
    args = parser.parse_args()
    if args.runs < 1 or args.warmups < 1:
        parser.error('runs and warmups must be positive')
    manifest = json.loads(args.manifest.read_text())
    output = args.output.resolve()
    output.mkdir(parents=True, exist_ok=False)
    shutil.copyfile(args.manifest, output/'manifest.json')
    shutil.copyfile(__file__, output/'replay.py')
    shutil.copyfile(HELPERS, output/'bench.py')
    tools = {}
    for index, selection in enumerate(args.tool):
        label, separator, filename = selection.partition('=')
        if not separator or not label or label in tools:
            parser.error('each tool needs a unique LABEL=PATH')
        original = Path(filename).absolute()
        destination = output/'tools'/str(index)/original.name
        destination.parent.mkdir(parents=True)
        shutil.copy2(original, destination)
        tools[label] = {'path': str(destination), 'original': str(original),
                        'sha256': digest(destination), 'version': version(str(destination))}
    cpu = next((line.partition(':')[2].strip()
                for line in Path('/proc/cpuinfo').read_text().splitlines()
                if line.startswith('model name')), 'unknown')
    report = {'started': time.strftime('%Y-%m-%dT%H:%M:%S%z'),
              'platform': platform.platform(), 'cpu': cpu,
              'available_cpus': sorted(os.sched_getaffinity(0)),
              'tools': tools, 'manifest_sha256': digest(args.manifest),
              'harness_sha256': digest(__file__), 'bench_helpers_sha256': digest(output/'bench.py'),
              'methodology': 'Warm cache; spawn-through-exit wall/CPU; rotated order; default linker workers; runtime checks and separate GNU-time RSS samples outside timed intervals. No cache eviction, affinity or governor changes.',
              'runs': 0 if args.verify_only else args.runs, 'warmups': args.warmups,
              'timing_comparison': not args.verify_only,
              'input_provenance': manifest.get('input_provenance', {}),
              'limitations': ['Explicit argv inputs are hashed; -l/script/transitive closures require the associated corpus/sysroot provenance.',
                              'Single machine and warm cache; no general linker ranking or production qualification.'],
              'workloads': []}
    for index, workload in enumerate(manifest['workloads']):
        print('Replay '+workload['name'], flush=True)
        argv = workload['args']
        if argv.count('-o') != 1 or argv.index('-o')+1 == len(argv):
            raise ValueError('workload needs exactly one -o PATH')
        cwd = Path(workload['cwd']).resolve()
        inputs_before = explicit_inputs(argv, cwd)
        directory = output/str(index)
        directory.mkdir()
        commands = {}
        for number, (label, tool) in enumerate(tools.items()):
            arguments = list(argv)
            arguments[arguments.index('-o')+1] = str(directory/str(number))
            commands[label] = [tool['path'], *arguments]
        original_cwd = Path.cwd()
        try:
            os.chdir(cwd)
            baseline = {}
            for label, command in commands.items():
                program = Path(command[command.index('-o')+1])
                subprocess.run(command, capture_output=True, check=True, timeout=60)
                check = workload['check']
                result = subprocess.run([str(program), *check.get('args', [])],
                                        cwd=check.get('cwd', str(cwd)),
                                        input=check.get('stdin', '').encode(),
                                        capture_output=True, timeout=30)
                if result.returncode != check.get('exit', 0):
                    raise RuntimeError(f'{label}: runtime check failed: {result.stderr!r}')
                baseline[label] = digest(program)
                if not args.verify_only:
                    for _ in range(args.warmups):
                        measure(command)
            samples = {label: [] for label in tools}
            if not args.verify_only:
                labels = list(tools)
                for run in range(args.runs):
                    rotation = run % len(labels)
                    for label in labels[rotation:]+labels[:rotation]:
                        samples[label].append(measure(commands[label]))
            results = {}
            for label, command in commands.items():
                program = Path(command[command.index('-o')+1])
                memory = None if args.verify_only else measure_memory(command)
                if digest(program) != baseline[label]:
                    raise RuntimeError(label+': repeated output changed')
                values = [sample['wall_ms'] for sample in samples[label]]
                results[label] = {'command': command, 'output_sha256': baseline[label],
                                  'output_bytes': program.stat().st_size,
                                  'peak_rss_kib': memory, 'samples': samples[label],
                                  'median_wall_ms': statistics.median(values) if values else None,
                                  'min_wall_ms': min(values) if values else None,
                                  'max_wall_ms': max(values) if values else None}
        finally:
            os.chdir(original_cwd)
        if explicit_inputs(argv, cwd) != inputs_before:
            raise RuntimeError('Explicit input files changed during replay')
        report['workloads'].append({'name': workload['name'], 'cwd': str(cwd),
                                    'explicit_input_sha256': inputs_before,
                                    'check': workload['check'], 'results': results})
        (output/'report.json').write_text(json.dumps(report, indent=2)+'\n')
    print('Report: '+str(output/'report.json'))


if __name__ == '__main__':
    main()
