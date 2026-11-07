#!/usr/bin/env python3
"""Prepare a retained, small serial-link replay corpus; no timed compilation.

The synthetic object shape matches bench.py. Optional real inputs come from
retained qualification runs, not fresh downloads or rebuilds.
"""
import argparse
import json
from pathlib import Path
import shutil
import subprocess


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output', required=True, type=Path)
    parser.add_argument('--sqlite-build', type=Path)
    parser.add_argument('--fas-fixtures', type=Path)
    args = parser.parse_args()
    root = args.output.resolve()
    root.mkdir(parents=True, exist_ok=False)
    workloads = []

    def assemble(name, text):
        source = root/(name+'.s')
        source.write_text(text+'\n.section .note.GNU-stack,"",@progbits\n')
        target = source.with_suffix('.o')
        subprocess.run(['as', '--64', '-o', str(target), str(source)], check=True)
        return str(target)

    def add(name, inputs, cwd=root, check=None):
        workloads.append(dict(name=name, cwd=str(cwd),
                              args=['-o', str(root/'unused-output'), *inputs],
                              check=check or {'exit': 0}))

    startup = '.text\n.globl _start\n_start: call fn_0_0\ncmp $42,%eax\nsetne %dil\nmovzbl %dil,%edi\nmov $60,%eax\nsyscall\n'
    start = assemble('start', startup)
    objects = []
    for index in range(256):
        lines = ['.text']
        for fn in range(64):
            name = f'fn_{index}_{fn}'
            lines += [f'.globl {name}', f'.type {name},@function',
                      f'{name}: mov $42,%eax', 'ret', f'.size {name},.-{name}']
        lines += ['.data', *(f'.quad fn_{index}_{fn}+1' for fn in range(64))]
        objects.append(assemble(f'object-{index}', '\n'.join(lines)))
    for count in (1, 32, 256):
        add(f'objects-{count}', [start, *objects[:count]])
        archive = root/f'archive-{count}.a'
        subprocess.run(['ar', 'rcs', str(archive), *objects[:count]], check=True)
        add(f'archive-lazy-{count}', [start, str(archive)])
        roots = assemble(f'roots-{count}', startup+'\n.data\n'+
                         '\n'.join(f'.quad fn_{n}_0' for n in range(count)))
        add(f'archive-all-{count}', [roots, str(archive)])
    provenance = {'synthetic': 'bench.py shape: 64 functions and 64 absolute relocations per object; archive-lazy selects one member, archive-all selects all members',
                  'scope': 'single-thread exploratory warm-cache probe, shared PC; output files replaced on each run'}
    if args.sqlite_build:
        build = args.sqlite_build.resolve()
        rows = [json.loads(line) for line in (build/'linker-invocations.jsonl').read_text().splitlines()]
        rows = [row for row in rows if '-o' in row['argv'] and row['argv'][row['argv'].index('-o')+1] == 'sqlite3']
        if len(rows) != 1:
            raise ValueError('expected one retained SQLite CLI command')
        row = rows[0]
        workloads.append(dict(name='sqlite-cli', cwd=row['cwd'], args=row['replay'],
                              check={'stdin': 'select 6*7;\n', 'stdout_contains': '42\n', 'exit': 0}))
        provenance['sqlite'] = str(build.parent.parent/'report.json')
    if args.fas_fixtures:
        base = args.fas_fixtures.resolve()
        inputs = []
        for name in ('agg-d0-s1.o', 'putnum.o'):
            target = root/name
            shutil.copy2(base/name, target)
            inputs.append(str(target))
        add('fas-native-static-pie', ['-pie', '--no-dynamic-linker', *inputs],
            check={'exit': 0, 'stdout_sha256': 'de87fa68b5d3a4f36ce3c2dcc902ae1b10317f702156bad3c68086e8209b23af'})
        provenance['fas'] = str(base/'report.json')
    (root/'manifest.json').write_text(json.dumps({'input_provenance': provenance,
                                                'workloads': workloads}, indent=2)+'\n')
    print(root/'manifest.json')


if __name__ == '__main__':
    main()
