#!/usr/bin/env python3
"""Run retained Lua/libarchive programs through thin and mixed partial archives."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess

from run import Corpus, ROOT, digest

LUA_SCRIPTS = ('bisect', 'cf', 'echo', 'env', 'factorial', 'fib', 'fibfor',
               'hello', 'printf', 'readonly', 'sieve', 'sort', 'table')
ARCHIVE_TESTS = ('test_read_format_tar', 'test_write_format_tar',
                 'test_read_format_zip', 'test_write_format_zip',
                 'test_read_format_cpio_bin', 'test_write_format_cpio',
                 'test_read_format_7zip', 'test_write_format_7zip')


class Qualification:
    # Reuse process-group cleanup and complete command logging from the corpus.
    run = Corpus.run

    def __init__(self, args):
        self.root = args.output.resolve()
        if self.root.is_relative_to(ROOT):
            raise ValueError('output must be outside the repository')
        self.root.mkdir(parents=True, exist_ok=False)
        self.corpus = args.corpus.resolve()
        self.prior = json.loads((self.corpus/'report.json').read_text())
        self.tools = self.root/'tools'
        self.tools.mkdir()
        self.linker = self.tools/'fas-ld'
        shutil.copy2(args.linker, self.linker)
        for name in ('archive_pipeline.py', 'run.py'):
            shutil.copy2(Path(__file__).with_name(name), self.tools/name)
        self.consumers = {'fas': str(self.linker), 'gnu': shutil.which('ld')}
        if args.lld:
            # Preserve the driver's basename when it is a symlink to lld.
            self.consumers['lld'] = str(Path(shutil.which(args.lld) or args.lld).absolute())
        self.report = {'status': 'running', 'timing_comparison': False,
                       'corpus': str(self.corpus),
                       'corpus_report_sha256': digest(self.corpus/'report.json'),
                       'linker_sha256': digest(self.linker),
                       'harness_sha256': {n: digest(self.tools/n) for n in
                                          ('archive_pipeline.py', 'run.py')},
                       'consumers': {}, 'archives': {}, 'projects': {}}
        self.save()

    def save(self):
        (self.root/'report.json').write_text(json.dumps(self.report, indent=2)+'\n')

    def archive(self, original):
        key = str(original)
        if key in self.report['archives']:
            return self.report['archives'][key]['variants']
        directory = self.root/'archives'/str(len(self.report['archives']))
        directory.mkdir(parents=True)
        copy = directory/'original.a'
        shutil.copy2(original, copy)
        members = self.run(['ar', 't', copy]).stdout.decode().splitlines()
        if not members or len(set(members)) != len(members) or any(
                Path(n).name != n or n in ('.', '..') for n in members):
            raise ValueError('qualification requires unique, flat archive members: '+key)
        extracted = directory/'members'
        extracted.mkdir()
        self.run(['ar', 'x', copy], cwd=extracted)
        objects = [extracted/n for n in members]
        variants = {'regular': str(copy), 'thin': str(directory/'thin.a')}
        self.run(['ar', 'crsT', variants['thin'], *objects])
        # Preserve lazy extraction at the final archive boundary. Aggregation
        # deliberately changes granularity; it is checked through real programs.
        for name, producers, thin in (
                ('fas-gnu', (self.linker, self.consumers['gnu']), False),
                ('gnu-fas', (self.consumers['gnu'], self.linker), True)):
            first, second = directory/(name+'-1.o'), directory/(name+'-2.o')
            self.run([producers[0], '-r', '--whole-archive', variants['thin'],
                      '--no-whole-archive', '-o', first])
            self.run([producers[1], '-r', first, '-o', second])
            target = directory/(name+'.a')
            self.run(['ar', 'crsT' if thin else 'crs', target, second])
            variants[name] = str(target)
        self.report['archives'][key] = {
            'sha256': digest(original),
            'members': {n: digest(extracted/n) for n in members},
            'variants': variants,
            'variant_sha256': {n: digest(Path(p)) for n, p in variants.items()}}
        self.save()
        return variants

    def check(self, commands, cwd):
        rows = []
        for argv, expected, diagnostic in commands:
            result = self.run(argv, cwd=cwd, env=dict(os.environ, LC_ALL='C'),
                              expected=expected, timeout=120)
            if diagnostic and diagnostic not in result.stderr:
                raise ValueError('missing expected Lua readonly diagnostic')
            if Path(argv[0]).name == 'libarchive_test':
                # Upstream's exit status alone is insufficient to establish
                # that a named case actually ran and all assertions passed.
                text = result.stdout.decode(errors='replace')
                counts = {}
                for label in ('Tests run', 'Tests failed', 'Assertions checked',
                              'Assertions failed', 'Skips reported'):
                    matches = re.findall(re.escape(label)+r':\s+(\d+)', text)
                    if len(matches) != 1:
                        raise ValueError('missing/ambiguous libarchive summary: '+label)
                    counts[label] = int(matches[0])
                if (counts['Tests run'] != 1 or counts['Tests failed'] != 0
                        or counts['Assertions failed'] != 0 or counts['Skips reported'] != 0
                        or counts['Assertions checked'] < 1):
                    raise ValueError('libarchive case failed or skipped: '+str(counts))
            else:
                counts = None
            rows.append({'argv': list(map(str, argv)), 'exit': result.returncode,
                         'assertions': counts,
                         'stdout_sha256': hashlib.sha256(result.stdout).hexdigest(),
                         'stderr_sha256': hashlib.sha256(result.stderr).hexdigest()})
        return rows

    def project(self, name):
        expected = {'lua': {'lua', 'luac'}, 'libarchive': {'libarchive_test'}}[name]
        previous = [r for r in self.prior['tests'] if r['name'] == name]
        if len(previous) != 1 or previous[0]['status'] != 'passed':
            raise ValueError('requires passing upstream corpus: '+name)
        project = self.corpus/name
        # Check all retained source files, including the upstream runtime fixtures.
        for filename, sha in self.prior['sources'][name]['sha256'].items():
            if digest(project/'source'/filename) != sha:
                raise ValueError('changed source: '+filename)
        rows = {}
        for line in (project/'linker-invocations.jsonl').read_text().splitlines():
            row = json.loads(line)
            if (row['tool'] == self.prior['linker_snapshot'] and row['status'] == 0
                    and row['output'] and Path(row['output']).name in expected):
                rows[Path(row['output']).name] = row
        if set(rows) != expected:
            raise ValueError('missing declared program selection: '+name)
        # Verify every recorded object/archive and common CRT input before linking.
        for row in rows.values():
            for filename, sha in row['input_sha256'].items():
                if digest(Path(row['input_replay'][filename])) != sha:
                    raise ValueError('changed link input: '+filename)
        results = self.report['projects'][name] = []
        for pipeline in ('regular', 'thin', 'fas-gnu', 'gnu-fas'):
            for consumer, tool in self.consumers.items():
                directory = self.root/name/pipeline/consumer
                directory.mkdir(parents=True)
                images = {}
                for program, row in rows.items():
                    arguments = list(row['replay'])
                    replacements = 0
                    for i, arg in enumerate(arguments):
                        path = (Path(row['cwd'])/arg).resolve()
                        if arg.endswith('.a') and path.is_relative_to(project):
                            arguments[i] = self.archive(path)[pipeline]
                            replacements += 1
                    if replacements == 0:
                        raise ValueError('no project archive in '+program)
                    output = directory/program
                    arguments[arguments.index('-o')+1] = str(output)
                    self.run([tool, *arguments], cwd=row['cwd'], timeout=120)
                    images[program] = output
                if name == 'lua':
                    commands = [([images['lua'], project/'source/test'/(s+'.lua')],
                                 1 if s == 'readonly' else 0,
                                 b"cannot redefine global variable `y'" if s == 'readonly' else None)
                                for s in LUA_SCRIPTS]
                    bytecode = directory/'sort.luac'
                    commands += [([images['luac'], '-o', bytecode,
                                   project/'source/test/sort.lua'], 0, None),
                                 ([images['lua'], bytecode], 0, None)]
                else:
                    commands = [([images['libarchive_test'], '-vv', '-r',
                                  project/'source/libarchive/test', '-s', test], 0, None)
                                for test in ARCHIVE_TESTS]
                checks = self.check(commands, directory)
                results.append({'pipeline': pipeline, 'consumer': consumer,
                                'images': {n: digest(p) for n, p in images.items()},
                                'checks': checks, 'status': 'passed'})
                self.save()
                print('PASSED', name, pipeline, consumer, len(checks), 'runtime checks', flush=True)

    def qualify(self, projects):
        try:
            for name, tool in self.consumers.items():
                self.report['consumers'][name] = {
                    'path': tool, 'sha256': digest(Path(tool)),
                    'version': self.run([tool, '--version']).stdout.decode().strip()}
            for filename, sha in self.prior['toolchain_inputs'].items():
                if digest(Path(filename)) != sha:
                    raise ValueError('changed toolchain input: '+filename)
            for name in projects:
                self.project(name)
            self.report['status'] = 'passed'
        except BaseException as error:
            self.report['status'] = 'failed'
            self.report['error'] = str(error)
            raise
        finally:
            self.save()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--corpus', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--linker', type=Path, default=ROOT/'_build/default/src/linker/bin/main.exe')
    parser.add_argument('--project', action='append', choices=('lua', 'libarchive'))
    parser.add_argument('--lld', help='optional independent final consumer')
    args = parser.parse_args()
    Qualification(args).qualify(args.project or ['lua', 'libarchive'])


if __name__ == '__main__':
    main()
