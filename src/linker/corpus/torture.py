#!/usr/bin/env python3
"""Run GCC's official C torture execution driver with Fas/GNU same-object links."""
import argparse
from collections import Counter
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import re
import shutil
import sys
from types import SimpleNamespace

HERE = Path(__file__).resolve().parent
spec = importlib.util.spec_from_file_location('corpus_base',
    HERE/('corpus-run.py' if (HERE/'corpus-run.py').exists() else 'run.py'))
base = importlib.util.module_from_spec(spec)
spec.loader.exec_module(base)
GCC_COMMIT = '5115c7e447fc07457443df874bf57840e8316d5f'


def qualify(c, args):
    source = args.gcc_source.resolve()
    commit = c.run(['git', '-C', source, 'rev-parse', 'HEAD']).stdout.decode().strip()
    assert commit == GCC_COMMIT, 'use the pinned GCC 15.2.0 source commit'
    assert not c.run(['git', '-C', source, 'status', '--porcelain']).stdout, 'GCC sources changed'
    tests = source/'gcc/testsuite'
    execute = tests/'gcc.c-torture/execute'
    assert (execute/'execute.exp').is_file(), 'missing sparse checkout paths'
    for dependency in ('gcc.dg/pr98304-1.c', 'gcc.dg/tree-ssa/pr105777.c',
                       'gcc.dg/tree-ssa/pr109938.c', 'gcc.dg/tree-ssa/pr109986.c',
                       'gcc.dg/tree-ssa/pr30314.c'):
        assert (tests/dependency).is_file(), 'sparse checkout lacks shared test input: '+dependency
    c.report['sources']['gcc'] = {'commit': commit, 'path': str(source),
        'sha256': {str(p.relative_to(tests)): base.digest(p)
                   for p in sorted(tests.rglob('*')) if p.is_file()}}
    tools = c.root/'toolchain'
    tools.mkdir()
    phase = tools/'phase'
    retained = c.root/'retained-inputs'
    retained.mkdir()
    calls = c.root/'linker-invocations.jsonl'
    cc_calls = c.root/'compiler-invocations.jsonl'
    ld = tools/'ld'
    ld.write_text('#!'+sys.executable+'\n'+f'''import hashlib,json,os,shutil,subprocess,sys
from pathlib import Path
args=sys.argv[1:]; actual=list(args)
phase=Path({str(phase)!r}).read_text().strip()
key=os.environ.get('FAS_TORTURE_KEY')
root=Path({str(retained)!r})
record=None; inputs=[]
if key:
    directory=root/key
    if phase=='fas':
        directory.mkdir(exist_ok=True)
    else:
        record=json.loads((directory/'link.json').read_text())
    output_index=args.index('-o')+1 if '-o' in args else -1
    for i,arg in enumerate(args):
        p=Path(arg).resolve()
        if i==output_index or p.suffix not in ('.o','.a') or not p.is_file():
            continue
        sha=hashlib.sha256(p.read_bytes()).hexdigest()
        if phase=='fas':
            saved=p
            if not p.is_relative_to('/usr'):
                saved=directory/(str(i)+'-'+p.name)
                shutil.copy2(p,saved)
            inputs.append({{'index':i,'original':str(p),'saved':str(saved),'sha256':sha}})
        else:
            old=record['inputs'][len(inputs)]
            if old['index']!=i: raise RuntimeError('link argument ordering changed')
            saved=Path(old['saved'])
            if hashlib.sha256(saved.read_bytes()).hexdigest()!=old['sha256']:
                raise RuntimeError('retained input changed: '+str(saved))
            actual[i]=str(saved)
            inputs.append(dict(old,recompiled_sha256=sha))
    if record is not None and len(inputs)!=len(record['inputs']):
        raise RuntimeError('link input count changed')
tool={str(c.linker)!r} if phase=='fas' else {shutil.which('ld')!r}
result=subprocess.run([tool,*actual])
row={{'phase':phase,'key':key,'cwd':os.getcwd(),'argv':args,'actual':actual,
     'inputs':inputs,'status':result.returncode}}
if key and phase=='fas': (directory/'link.json').write_text(json.dumps(row))
with open({str(calls)!r},'a') as out: out.write(json.dumps(row)+'\\n')
sys.exit(result.returncode)
''')
    ld.chmod(0o755)
    cc = tools/'gcc'
    gcc = shutil.which('gcc')
    gcc_version = c.run([gcc, '-dumpfullversion']).stdout.decode().strip()
    assert gcc_version.startswith('15.2'), 'this pinned gate requires GCC 15.2'
    cc.write_text('#!'+sys.executable+'\n'+f'''import hashlib,json,os,subprocess,sys
from pathlib import Path
args=sys.argv[1:]
phase=Path({str(phase)!r}).read_text().strip()
primary=any(str(Path(a).resolve()).startswith({str(execute)+os.sep!r})
            and Path(a).suffix in ('.c','.S') for a in args)
linking=primary and not any(a in args for a in ('-c','-S','-E','-fsyntax-only'))
normalized=[]; skip=False
for arg in args:
    if skip: skip=False; continue
    if arg=='-o': skip=True; continue
    normalized.append(arg)
key=hashlib.sha256(json.dumps(normalized).encode()).hexdigest() if linking else None
environment=dict(os.environ)
if key: environment['FAS_TORTURE_KEY']=key
else: environment.pop('FAS_TORTURE_KEY',None)
lto=any(a=='-flto' or a.startswith('-flto=') for a in args)
if lto:
    # Report LTO unavailable to the official effective-target machinery.
    # No bitcode or fallback linker is used in this native-object corpus.
    print('fas torture qualification: LTO is outside the declared scope',file=sys.stderr)
    status=1
else:
    status=subprocess.run([{gcc!r},{'-B'+str(tools)+'/'!r},'-fno-use-linker-plugin',
                           '-U_FORTIFY_SOURCE',*args],env=environment).returncode
with open({str(cc_calls)!r},'a') as out:
    out.write(json.dumps({{'phase':phase,'argv':args,'primary':primary,'key':key,
                         'lto_out_of_scope':lto,'status':status}})+'\\n')
sys.exit(status)
''')
    cc.chmod(0o755)
    work = c.root/'dejagnu'
    work.mkdir()
    (work/'tmp').mkdir()
    triplet = c.run([gcc, '-dumpmachine']).stdout.decode().strip()
    # Braces in arbitrary user paths would change Tcl syntax. Fail explicitly.
    for path in (work, tests, cc):
        assert not any(char in str(path) for char in '{}\n'), 'unsupported Tcl path character'
    options = ' '.join('{'+option+'}' for option in args.optimization)
    (work/'site.exp').write_text(f'''set rootme {{{work}}}
set host_triplet {triplet}
set target_triplet {triplet}
set build_triplet {triplet}
set srcdir {{{tests}}}
set tmpdir {{{work/'tmp'}}}
set GCC_UNDER_TEST {{{cc}}}
set TORTURE_OPTIONS [list {options}]
''')
    if args.no_trampolines:
        boards = work/'boards'
        boards.mkdir()
        (boards/'fas-native.exp').write_text('''load_generic_config "unix"
process_multilib_options ""
unset_board_info isremote
set_board_info isremote 0
set_board_info bmk,use_alarm 1
set_board_info gcc,no_trampolines 1
''')
        with (work/'site.exp').open('a') as site:
            site.write(f'set boards_dir [list {{{boards}}}]\n')
    env = dict(os.environ, LC_ALL='C')
    if args.dejagnu_root:
        local = args.dejagnu_root.resolve()
        runtest = local/'usr/bin/runtest'
        env['EXPECT'] = str(local/'usr/bin/expect')
        env['LD_LIBRARY_PATH'] = str(local/'usr/lib/x86_64-linux-gnu')+(
            ':'+env['LD_LIBRARY_PATH'] if env.get('LD_LIBRARY_PATH') else '')
    else:
        runtest = shutil.which('runtest')
    assert runtest, 'install DejaGnu/Expect or provide --dejagnu-root'
    c.report['gcc_torture'] = detail = {'source_commit': commit,
        'optimizations': args.optimization, 'pattern': args.pattern, 'gcc_version': gcc_version,
        'driver_flags': ['-fno-use-linker-plugin', '-U_FORTIFY_SOURCE'],
        'no_trampolines': args.no_trampolines,
        'scope': 'official execute.exp and dg directives; native Linux x86-64; LTO excluded',
        'same_objects': 'GNU replaces compiler temporaries with retained Fas-link inputs',
        'passes': [], 'runtest_sha256': base.digest(Path(runtest))}
    c.save()
    # Use the script basename: DejaGnu 1.6.3 applies file filters to that key.
    command = [runtest, '--tool', 'gcc', '--srcdir', tests,
               'execute.exp='+args.pattern]
    if args.no_trampolines:
        command += ['--target_board=fas-native']
    for tag in ('fas', 'gnu'):
        print('GCC TORTURE', tag, flush=True)
        phase.write_text(tag)
        result = c.run(command, cwd=work, env=env, expected=None, timeout=args.timeout)
        summary = work/'gcc.sum'
        assert summary.exists(), 'DejaGnu produced no gcc.sum'
        text = summary.read_text()
        counts = Counter(re.findall(r'^(PASS|FAIL|XPASS|XFAIL|UNSUPPORTED|UNRESOLVED|ERROR|WARNING):', text, re.M))
        executions = re.findall(r'^PASS: (.* execution test)$', text, re.M)
        failures = re.findall(r'^(?:FAIL|XPASS|UNRESOLVED|ERROR):.*$', text, re.M)
        row = {'linker': tag, 'exit': result.returncode, 'counts': dict(counts),
               'execution_passes': len(executions), 'failures': failures,
               'execution_labels_sha256': hashlib.sha256('\n'.join(executions).encode()).hexdigest()}
        for suffix in ('sum', 'log'):
            path = work/('gcc.'+suffix)
            target = c.root/('gcc-'+tag+'.'+suffix)
            shutil.copy2(path, target)
            row[suffix+'_sha256'] = base.digest(target)
        detail['passes'].append(row)
        c.save()
    links = [json.loads(line) for line in calls.read_text().splitlines()]
    primary = {tag: [row for row in links if row['key'] and row['phase'] == tag]
               for tag in ('fas', 'gnu')}
    detail['primary_links'] = {tag: len(rows) for tag, rows in primary.items()}
    detail['same_object_keys'] = len({row['key'] for row in primary['fas']} &
                                    {row['key'] for row in primary['gnu']})
    detail['linker_invocations_sha256'] = base.digest(calls)
    detail['compiler_invocations_sha256'] = base.digest(cc_calls)
    c.save()
    assert all(row['execution_passes'] and not row['failures'] and row['exit'] == 0
               for row in detail['passes']), 'official GCC results contain failures; see retained summaries'
    assert detail['same_object_keys'], 'no same-object GNU links'
    assert {row['key'] for row in primary['fas']} == {row['key'] for row in primary['gnu']}, \
        'Fas/GNU native link selections differ'
    assert len({row['execution_labels_sha256'] for row in detail['passes']}) == 1, \
        'Fas/GNU executed different official test selections'
    return detail


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--gcc-source', required=True, type=Path)
    parser.add_argument('--dejagnu-root', type=Path, help='isolated extracted Debian packages (optional)')
    parser.add_argument('--output', required=True, type=Path)
    parser.add_argument('--linker', type=Path, default=base.ROOT/'_build/default/src/linker/bin/main.exe')
    parser.add_argument('--pattern', default='*', help='official execute.exp file glob')
    parser.add_argument('--no-trampolines', action='store_true',
                        help='declare the non-executable-stack subset through GCC board capabilities')
    parser.add_argument('--optimization', action='append', choices=('-O0', '-O1', '-O2', '-O3', '-Os'))
    parser.add_argument('--timeout', type=int, default=7200, help='seconds per official pass')
    args = parser.parse_args()
    args.optimization = args.optimization or ['-O0', '-O2', '-Os']
    settings = SimpleNamespace(output=args.output, linker=args.linker,
        compiler=base.ROOT/'_build/default/bin/main.exe', sieve=None, sqlite=None,
        static=False, static_pie=False, dependency_path=[], strict_dependencies=False)
    c = base.Corpus(settings)
    shutil.copy2(Path(__file__), c.root/'tools/torture.py')
    c.report['torture_harness_sha256'] = base.digest(Path(__file__))
    c.test('gcc-c-torture-execute', lambda: qualify(c, args))
    raise SystemExit(any(row['status'] != 'passed' for row in c.report['tests']))


if __name__ == '__main__':
    main()
