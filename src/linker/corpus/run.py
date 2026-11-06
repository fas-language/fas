#!/usr/bin/env python3
"""Reproducible local-project acceptance, isolated outputs, no timing claims."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import shlex
import shutil
import signal
import sqlite3
import struct
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parents[3]


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


class Corpus:
    def __init__(self, args):
        self.args = args
        self.root = args.output.resolve()
        if (ROOT/'dune-project').is_file() and self.root.is_relative_to(ROOT):
            raise ValueError('output must be outside the repository')
        self.root.mkdir(parents=True, exist_ok=False)
        linker_source = args.linker.resolve()
        tools = self.root/'tools'
        tools.mkdir()
        self.linker = tools/'fas-ld'
        shutil.copy2(linker_source, self.linker)
        harness = tools/'corpus-run.py'
        shutil.copy2(Path(__file__), harness)
        compiler_source = args.compiler.resolve()
        self.compiler = tools/'fas-compiler'
        if args.sieve or args.sqlite:
            shutil.copy2(compiler_source, self.compiler)
        self.sqlite_fixtures = {}
        if args.sqlite:
            for name in ('sqlite_consumer.c', 'sqlite_consumer.fas', 'sqlite_extension.c'):
                fixture = tools/name
                shutil.copy2(Path(__file__).with_name(name), fixture)
                self.sqlite_fixtures[name] = fixture
        self.report = {'started': time.strftime('%Y-%m-%dT%H:%M:%S%z'),
                       'harness_sha256': digest(Path(__file__)),
                       'harness_snapshot': str(harness),
                       'linker_sha256': digest(self.linker),
                       'linker_source': str(linker_source),
                       'linker_snapshot': str(self.linker), 'tests': [], 'sources': {},
                       'limitations': [], 'timing_comparison': False}
        if args.sieve or args.sqlite:
            self.report['compiler'] = {'source': str(compiler_source),
                                       'snapshot': str(self.compiler),
                                       'sha256': digest(self.compiler)}
        if args.sqlite:
            self.report['sqlite_fixture_sha256'] = {
                name: digest(path) for name, path in self.sqlite_fixtures.items()}
        self.report['linkage'] = 'static-pie' if args.static_pie else 'static' if args.static else 'dynamic'
        self.dependency_paths = [str(p.resolve()) for p in args.dependency_path]
        self.report['dependency_policy'] = {
            'strict': args.strict_dependencies, 'search_paths': self.dependency_paths}
        self.crts = {}
        crt_names = ['crt1.o', 'Scrt1.o', 'crti.o', 'crtbegin.o', 'crtbeginS.o',
                     'crtend.o', 'crtendS.o', 'crtn.o', 'libgcc.a']
        if args.static or args.static_pie or args.sqlite:
            crt_names += ['crtbeginT.o', 'libgcc_eh.a', 'libc.a', 'libm.a', 'libmvec.a']
        if args.static_pie or args.sqlite:
            crt_names += ['rcrt1.o']
        for name in crt_names:
            path = Path(self.run(['gcc', '-print-file-name='+name]).stdout.decode().strip()).resolve()
            if not path.is_file():
                raise ValueError('missing toolchain input: '+name)
            self.crts[name] = path
        self.report['toolchain_inputs'] = {str(p): digest(p) for p in self.crts.values()}

    def save(self):
        (self.root / 'report.json').write_text(json.dumps(self.report, indent=2)+'\n')

    def run(self, args, *, cwd=None, env=None, expected=0, timeout=90):
        args = list(map(str, args))
        # Build tools and upstream suites start compilers and local servers.
        # Killing only their immediate parent on timeout leaves those children
        # running against a snapshot that later phases may reuse.
        process = subprocess.Popen(args, cwd=cwd, env=env,
            stdout=subprocess.PIPE, stderr=subprocess.PIPE, start_new_session=True)
        expired = None
        try:
            stdout, stderr = process.communicate(timeout=timeout)
        except BaseException as error:
            try:
                os.killpg(process.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
            stdout, stderr = process.communicate()
            if not isinstance(error, subprocess.TimeoutExpired):
                raise
            expired = subprocess.TimeoutExpired(args, timeout, output=stdout, stderr=stderr)
        result = subprocess.CompletedProcess(args, process.returncode, stdout, stderr)
        with (self.root / 'commands.jsonl').open('a') as log:
            log.write(json.dumps({'argv': args, 'cwd': str(cwd) if cwd else None,
                                  'environment_overrides': {k: env[k] for k in
                                      ('SDL_VIDEODRIVER', 'SDL_AUDIODRIVER', 'DISPLAY',
                                       'CC', 'CFLAGS', 'LDFLAGS', 'LD_LIBRARY_PATH',
                                       'FAS_OPT', 'FAS_LLC', 'FAS_CC', 'FAS_FIR', 'DOOMWADDIR')
                                      if env and k in env},
                                  'status': result.returncode,
                                  'timed_out': expired is not None,
                                  'stdout': result.stdout.decode(errors='replace'),
                                  'stderr': result.stderr.decode(errors='replace')})+'\n')
        if expired is not None:
            raise expired
        if expected is not None and result.returncode != expected:
            raise RuntimeError(f'{shlex.join(args)}: exit {result.returncode}, expected {expected}\n'
                               +result.stderr.decode(errors='replace')[-3000:])
        return result

    def test(self, name, fn):
        try:
            detail = fn()
            row = {'name': name, 'status': 'passed', 'detail': detail}
        except (RuntimeError, OSError, subprocess.TimeoutExpired, AssertionError) as error:
            row = {'name': name, 'status': 'failed', 'error': str(error)}
        self.report['tests'].append(row)
        self.save()
        print(row['status'].upper(), name, flush=True)
        return row

    def snapshot(self, name, source):
        source = source.resolve()
        target = self.root / name / 'source'
        shutil.copytree(source, target, ignore=shutil.ignore_patterns(
            '.git', 'build', 'stage2', '*.o', '*.exe', '*.d', '__pycache__'))
        manifest = {str(p.relative_to(target)): digest(p) for p in sorted(target.rglob('*'))
                    if p.is_file()}
        self.report['sources'][name] = {'path': str(source), 'sha256': manifest}
        return target

    def compile(self, source, target, flags=(), cwd=None):
        self.run(['gcc', '-c', '-O2', '-g', '-fPIC', '-ffunction-sections',
                  '-fdata-sections', *flags, source, '-o', target], cwd=cwd)
        return target

    def hosted(self, output, objects, libraries=(), *, tool=None, pie=True):
        crt = self.crts
        if self.args.static or self.args.static_pie:
            static_pie = self.args.static_pie
            static_libraries = []
            for library in libraries:
                if Path(library).name != 'libm.so.6':
                    raise ValueError('no declared static equivalent for '+str(library))
                static_libraries += [crt['libm.a'], crt['libmvec.a']]
            mode_flags = ['-static', '-pie', '--no-dynamic-linker', '-z', 'text'] if static_pie else ['-static']
            self.run([tool or self.linker, *mode_flags, '--eh-frame-hdr', '--gc-sections',
                      '-o', output, crt['rcrt1.o' if static_pie else 'crt1.o'], crt['crti.o'],
                      crt['crtbeginS.o' if static_pie else 'crtbeginT.o'],
                      *objects, '--start-group', *static_libraries,
                      crt['libgcc.a'], crt['libgcc_eh.a'], crt['libc.a'], '--end-group',
                      crt['crtendS.o' if static_pie else 'crtend.o'], crt['crtn.o']])
            data = output.read_bytes()
            assert struct.unpack_from('<H', data, 16)[0] == (3 if static_pie else 2), 'unexpected static ELF type'
            phoff = struct.unpack_from('<Q', data, 32)[0]
            phnum = struct.unpack_from('<H', data, 56)[0]
            kinds = [struct.unpack_from('<I', data, phoff+n*56)[0] for n in range(phnum)]
            assert 3 not in kinds, 'static output has an interpreter'
            assert (2 in kinds) == static_pie, 'unexpected static dynamic-header policy'
            self.run(['readelf', '-aW', output])
            return output
        dependency_flags = []
        if self.args.strict_dependencies:
            dependency_flags.append('--no-allow-shlib-undefined')
            for path in self.dependency_paths:
                dependency_flags.extend(('-rpath-link', path))
        self.run([tool or self.linker, *(['-pie'] if pie else []), '--gc-sections',
                  *dependency_flags,
                  '--dynamic-linker', '/lib64/ld-linux-x86-64.so.2', '-o', output,
                  crt['Scrt1.o' if pie else 'crt1.o'], crt['crti.o'],
                  crt['crtbeginS.o' if pie else 'crtbegin.o'], *objects, *libraries,
                  '/usr/lib/x86_64-linux-gnu/libc.so', crt['libgcc.a'],
                  crt['crtendS.o' if pie else 'crtend.o'], crt['crtn.o']])
        self.run(['readelf', '-aW', output])
        return output

    def sqlite(self, source):
        src = self.snapshot('sqlite', source)
        build = src.parent/'build'
        build.mkdir()
        wrappers = build/'toolchain'
        wrappers.mkdir()
        calls = build/'linker-invocations.jsonl'
        inputs = build/'link-inputs'
        inputs.mkdir()
        gcc = shutil.which('gcc')
        # Retain temporary compiler objects before collect2 removes them. This
        # lets GNU consume exactly the same testfixture/CLI/DSO input objects.
        for name, body in (
            ('ld', f'''import hashlib,json,os,shutil,sys,tempfile
from pathlib import Path
directory=Path(tempfile.mkdtemp(prefix='link-',dir={str(inputs)!r}))
arguments=sys.argv[1:]; replay=list(arguments); saved={{}}
for index,arg in enumerate(arguments):
    path=Path(arg)
    if arg.endswith('.o') and path.is_file():
        target=directory/(str(index)+'-'+path.name)
        shutil.copy2(path,target); replay[index]=str(target)
        saved[str(target)]=hashlib.sha256(target.read_bytes()).hexdigest()
with open({str(calls)!r},'a') as out:
    out.write(json.dumps({{'argv':arguments,'replay':replay,'cwd':os.getcwd(),'saved_object_sha256':saved}})+'\\n')
os.execv({str(self.linker)!r},[{str(self.linker)!r},*arguments])
'''),
            ('cc', f'''import os,sys
os.execv({gcc!r},[{gcc!r},{'-B'+str(wrappers)+'/'!r},'-fno-use-linker-plugin',*sys.argv[1:]])
''')):
            path = wrappers/name
            path.write_text('#!'+sys.executable+'\n'+body)
            path.chmod(0o755)
        environment = dict(os.environ, CC=str(wrappers/'cc'), CFLAGS='-O2 -g -fPIC')
        configure = [src/'configure', '--all', '--disable-readline']
        if self.args.sqlite_tcl_config:
            configure.append('--with-tcl='+str(self.args.sqlite_tcl_config.resolve()))
        if self.args.sqlite_tclsh:
            configure.append('--with-tclsh='+str(self.args.sqlite_tclsh.resolve()))
        self.run(configure, cwd=build, env=environment)
        self.run(['nice', '-n', '10', 'make', '-j2', 'sqlite3', 'libsqlite3.so',
                  'libsqlite3.a', 'testfixture'], cwd=build, env=environment, timeout=600)
        invocations = [json.loads(line) for line in calls.read_text().splitlines()]
        assert invocations, 'upstream SQLite build did not invoke Fas'

        def oracle(name):
            matches = [row for row in invocations if '-o' in row['argv']
                       and Path(row['argv'][row['argv'].index('-o')+1]).name == name]
            assert len(matches) == 1, (name, len(matches))
            row = matches[0]
            arguments = list(row['replay'])
            target = build/(name+'-gnu')
            arguments[arguments.index('-o')+1] = str(target)
            self.run(['ld', *arguments], cwd=row['cwd'])
            return target

        gnu_shell = oracle('sqlite3')
        gnu_fixture = oracle('testfixture')
        gnu_library = oracle('libsqlite3.so')
        library_dirs = {}
        for tag, library in (('fas', build/'libsqlite3.so'), ('gnu', gnu_library)):
            directory = build/('library-'+tag)
            directory.mkdir()
            shutil.copy2(library, directory/'libsqlite3.so')
            library_dirs[tag] = directory
        extension_object = self.compile(self.sqlite_fixtures['sqlite_extension.c'],
                                        build/'extension.o', ('-I'+str(build),))
        extensions = {}
        for tag, linker in (('fas', self.linker), ('gnu', 'ld')):
            extensions[tag] = build/('extension-'+tag+'.so')
            self.run([linker, '-shared', '--hash-style=both', '-o', extensions[tag],
                      extension_object], cwd=build)
        compared = []
        compilers = [('gcc', gcc)]
        clang = shutil.which('clang-22') or shutil.which('clang')
        if clang:
            compilers.append(('clang', clang))
        else:
            self.report['limitations'].append('SQLite Clang consumers unverified: Clang unavailable')
        for cc_tag, cc in compilers:
            obj = build/('consumer-'+cc_tag+'.o')
            self.run([cc, '-c', '-O2', '-g', '-fPIC', '-pthread', '-I'+str(build),
                      self.sqlite_fixtures['sqlite_consumer.c'], '-o', obj])
            modes = [('archive-pie', ['-pie'], [build/'libsqlite3.a'], False),
                     ('archive-fixed', ['-no-pie'], [build/'libsqlite3.a'], False),
                     ('static', ['-static'], [build/'libsqlite3.a'], True),
                     ('static-pie', ['-static-pie'], [build/'libsqlite3.a'], True)]
            for producer, directory in library_dirs.items():
                modes.append(('shared-'+producer, ['-pie'],
                              ['-L'+str(directory), '-Wl,-rpath,'+str(directory),
                               '-lsqlite3'], False))
            for mode, flags, library, static in modes:
                outputs = []
                for tag in ('fas', 'gnu'):
                    program = build/f'{cc_tag}-{mode}-{tag}'
                    selection = ['-B'+str(wrappers)+'/'] if tag == 'fas' else ['-fuse-ld=bfd']
                    if cc_tag == 'gcc':
                        selection.append('-fno-use-linker-plugin')
                    self.run([cc, *selection, *flags, '-o', program, obj, *library,
                              '-lm', '-ldl', '-pthread'], cwd=build)
                    inspected = self.run(['readelf', '-aW', program])
                    assert not inspected.stderr, inspected.stderr
                    data = program.read_bytes()
                    elf_type = struct.unpack_from('<H', data, 16)[0]
                    assert elf_type == (2 if mode in ('archive-fixed', 'static') else 3)
                    phoff = struct.unpack_from('<Q', data, 32)[0]
                    phnum = struct.unpack_from('<H', data, 56)[0]
                    kinds = [struct.unpack_from('<I', data, phoff+n*56)[0] for n in range(phnum)]
                    assert (3 in kinds) == (not static), 'unexpected interpreter policy'
                    for ext_tag, extension in ([('none', '-')] if static else extensions.items()):
                        runtime = build/f'run-{cc_tag}-{mode}-{tag}-{ext_tag}'
                        runtime.mkdir()
                        result = self.run([program, runtime/'test.db', extension], cwd=runtime)
                        outputs.append(result.stdout)
                        compared.append({'compiler': cc_tag, 'mode': mode, 'linker': tag,
                                         'extension_linker': ext_tag, 'program_sha256': digest(program),
                                         'stdout_sha256': hashlib.sha256(result.stdout).hexdigest()})
                assert all(value == outputs[0] for value in outputs), 'SQLite API outputs differ'
        native = []
        for mode, flags, library in (
                ('archive', [], [build/'libsqlite3.a']),
                ('shared', [], ['-L', library_dirs['fas'], '-lsqlite3',
                                '-Wl,-rpath,'+str(library_dirs['fas'])]),
                ('static', ['-Wl,-static'], [build/'libsqlite3.a']),
                ('static-pie', ['-Wl,-static-pie'], [build/'libsqlite3.a'])):
            program = build/('fas-native-'+mode)
            self.run([self.compiler, '--backend=native', self.sqlite_fixtures['sqlite_consumer.fas'],
                      *flags, *library, '-lm', '-ldl', '-o', program],
                     env=dict(os.environ, FAS_CC=gcc, FAS_FIR='',
                              FAS_OPT='/nonexistent/opt', FAS_LLC='/nonexistent/llc'), timeout=120)
            self.run([program])
            data = program.read_bytes()
            assert struct.unpack_from('<H', data, 16)[0] == (3 if mode == 'static-pie' else 2)
            phoff = struct.unpack_from('<Q', data, 32)[0]
            phnum = struct.unpack_from('<H', data, 56)[0]
            kinds = [struct.unpack_from('<I', data, phoff+n*56)[0] for n in range(phnum)]
            assert (3 in kinds) == (mode not in ('static', 'static-pie'))
            native.append({'mode': mode, 'program_sha256': digest(program)})
        # Cross-read an on-disk database and a shell dump through both linkers.
        database = build/'shell.db'
        sql = ('CREATE TABLE t(x INTEGER); INSERT INTO t VALUES(19),(23);'
               'SELECT sum(x) FROM t; PRAGMA integrity_check;\n')
        result = self.run([build/'sqlite3', database, sql])
        assert result.stdout == b'42\nok\n', result.stdout
        assert self.run([gnu_shell, database, 'SELECT sum(x) FROM t']).stdout == b'42\n'
        dumped = self.run([build/'sqlite3', database, '.dump']).stdout.decode()
        restored = build/'restored.db'
        self.run([gnu_shell, restored, dumped])
        assert self.run([build/'sqlite3', restored, 'SELECT sum(x) FROM t']).stdout == b'42\n'
        upstream = []
        # Save completed API/native coverage even if an upstream test fails.
        detail = {'version': (src/'VERSION').read_text().strip(),
                  'api_executions': compared, 'native_fas_consumers': native,
                  'upstream': upstream, 'shell_cross_read_and_restore': 'passed'}
        suite_environment = dict(os.environ)
        for variable in ('QUICKTEST_OMIT', 'QUICKTEST_INCLUDE', 'TEST_FAILURE'):
            suite_environment.pop(variable, None)
        detail['test_selection_environment'] = 'QUICKTEST_OMIT/QUICKTEST_INCLUDE/TEST_FAILURE unset'
        self.report['sqlite_qualification'] = detail
        self.save()
        for tag, fixture in (('fas', build/'testfixture'), ('gnu', gnu_fixture)):
            # tester.tcl discovers subprocesses beside its own executable.
            # Give each oracle its matching CLI rather than sharing Fas's CLI.
            home = build/('fixture-'+tag)
            home.mkdir()
            shutil.copy2(fixture, home/'testfixture')
            shutil.copy2(build/'sqlite3' if tag == 'fas' else gnu_shell, home/'sqlite3')
            runtime = build/('upstream-'+tag)
            runtime.mkdir()
            # Use upstream's current runner: each test gets its own process and
            # scratch directory. The legacy extraquick.test sources everything
            # into one interpreter and leaks test state between scripts.
            selection = ['extraquick'] if self.args.sqlite_extraquick else ['veryquick', '^test/main.test$']
            print('SQLite upstream '+tag+': '+' '.join(selection), flush=True)
            result = self.run(['nice', '-n', '10', home/'testfixture',
                               src/'test/testrunner.tcl', '--jobs', '2', *selection],
                              cwd=runtime, env=suite_environment, expected=None, timeout=1200)
            text = result.stdout.decode(errors='replace')
            summary = re.search(r'(\d+) errors out of (\d+) tests', text)
            with sqlite3.connect('file:'+str(runtime/'testrunner.db')+'?mode=ro', uri=True) as db:
                jobs = [dict(zip(('name', 'state', 'tests', 'errors'), row)) for row in
                        db.execute('SELECT displayname,state,ntest,nerr FROM jobs ORDER BY displayname')]
            upstream.append({'linker': tag, 'runner': 'test/testrunner.tcl',
                             'selection': selection, 'exit_status': result.returncode,
                             'tests': int(summary[2]) if summary else None,
                             'errors': int(summary[1]) if summary else None,
                             'jobs': jobs, 'summary_tail': text[-4000:],
                             'stdout_sha256': hashlib.sha256(result.stdout).hexdigest(),
                             'test_log_sha256': digest(runtime/'testrunner.log')})
            self.save()
        # Run and record both oracles before deciding whether qualification passed.
        for row in upstream:
            assert row['exit_status'] == 0 and row['errors'] == 0 and row['tests'], row['summary_tail']
            assert row['jobs'] and all(job['state'] == 'done' for job in row['jobs']), row['linker']
        assert [job['name'] for job in upstream[0]['jobs']] == [
                job['name'] for job in upstream[1]['jobs']], 'upstream test selections differ'
        # Some upstream scripts use Tcl rand() to generate a variable number
        # of assertions (e.g. FTS5 distinct random terms). Each oracle must pass
        # its own assertions; their assertion counts need not be identical.
        detail['upstream_assertion_count_differences'] = [
            {'name': left['name'], 'fas': left['tests'], 'gnu': right['tests']}
            for left, right in zip(upstream[0]['jobs'], upstream[1]['jobs'])
            if left['tests'] != right['tests']]
        invocations = [json.loads(line) for line in calls.read_text().splitlines()]
        expected_outputs = {'sqlite3', 'testfixture', 'libsqlite3.so'}
        expected_outputs.update(f'{cc}-{mode}-fas' for cc, _ in compilers
                                for mode, *_ in modes)
        linked_outputs = {Path(row['argv'][row['argv'].index('-o')+1]).name
                          for row in invocations if '-o' in row['argv']}
        assert expected_outputs <= linked_outputs, 'selected builds bypassed Fas'
        for name in ('sqlite3', 'libsqlite3.so', 'testfixture'):
            self.run(['readelf', '-aW', build/name])
        detail.update({'actual_linker_invocations': len(invocations),
                'linker_invocations_sha256': digest(calls), 'api_executions': compared,
                'native_fas_consumers': native, 'upstream': upstream,
                'shell_cross_read_and_restore': 'passed',
                'artifacts': {name: digest(build/name) for name in
                              ('sqlite3', 'libsqlite3.so', 'libsqlite3.a', 'testfixture')},
                'limits': 'Public Tcl suite only; upstream testfixture defines SQLITE_NO_SYNC. No TH3, full SQLite release suite, power-loss or timing claim.',
                'scope': 'upstream configure/make --all; GCC/Clang API consumers; static/fixed/PIE/static-PIE; Fas/GNU DSOs and loadable extensions; WAL/thread/backup/JSON/FTS5/RTREE; native Fas'})
        return detail

    def zlib(self, source):
        src = self.snapshot('zlib', source)
        build = src.parent
        wrappers = build/'toolchain'
        wrappers.mkdir()
        calls = build/'linker-invocations.jsonl'
        gcc = shutil.which('gcc')
        # Test-only wrappers preserve GCC's real CRT/library policy and record
        # every invocation of our linker, including configure's link probes.
        for name, body in (
            ('ld', f'''import json,os,sys
with open({str(calls)!r},'a') as out: out.write(json.dumps(sys.argv[1:])+'\\n')
os.execv({str(self.linker)!r},[{str(self.linker)!r},*sys.argv[1:]])
'''),
            ('cc', f'''import os,sys
os.execv({gcc!r},[{gcc!r},{'-B'+str(wrappers)+'/'!r},'-fno-use-linker-plugin',*sys.argv[1:]])
''')):
            path = wrappers/name
            path.write_text('#!'+sys.executable+'\n'+body)
            path.chmod(0o755)
        flags = []
        if self.args.static:
            flags.append('-static')
        elif self.args.static_pie:
            flags.append('-static-pie')
        if self.args.strict_dependencies:
            flags.append('-Wl,--no-allow-shlib-undefined')
            flags += ['-Wl,-rpath-link,'+path for path in self.dependency_paths]
        environment = dict(os.environ, CC=str(wrappers/'cc'),
                           CFLAGS='-O2 -g -fPIC -ffunction-sections -fdata-sections',
                           LDFLAGS=shlex.join(flags))
        static = self.args.static or self.args.static_pie
        self.run(['./configure', '--static' if static else '--shared'], cwd=src, env=environment)
        self.run(['nice', '-n', '10', 'make', '-j1', 'all', 'all64'], cwd=src,
                 env=environment, timeout=240)
        result = self.run(['make', 'test', 'test64'], cwd=src, env=environment)
        assert calls.exists(), 'upstream build did not invoke fas-ld'
        invocations = [json.loads(line) for line in calls.read_text().splitlines()]
        assert invocations, 'empty linker invocation record'
        artifacts = {}
        for name in ('example', 'minigzip', 'example64', 'minigzip64'):
            target = src/name
            inspected = self.run(['readelf', '-aW', target])
            assert not inspected.stderr, inspected.stderr
            if static:
                data = target.read_bytes()
                assert struct.unpack_from('<H', data, 16)[0] == (3 if self.args.static_pie else 2)
                offset = struct.unpack_from('<Q', data, 32)[0]
                count = struct.unpack_from('<H', data, 56)[0]
                kinds = [struct.unpack_from('<I', data, offset+n*56)[0] for n in range(count)]
                assert 3 not in kinds, 'upstream static executable has PT_INTERP'
                assert (2 in kinds) == self.args.static_pie, 'upstream static dynamic-header policy'
            # GNU comparisons consume the same upstream objects and archive.
            oracle = self.hosted(build/(name+'-gnu'), [src/(name+'.o'), src/'libz.a'], tool='ld')
            if name.startswith('example'):
                actual = self.run([target], cwd=src).stdout
                expected = self.run([oracle], cwd=src).stdout
                assert actual == expected, name+' output differs from GNU ld'
            artifacts[name] = {'sha256': digest(target), 'gnu_sha256': digest(oracle)}
        payload = build/'payload.bin'
        payload.write_bytes(bytes(range(256))*32 + b'Fas linker gzip corpus\n'*128)
        gzip_outputs = []
        programs = [src/'minigzip', build/'minigzip-gnu', src/'minigzip64', build/'minigzip64-gnu']
        if not static:
            programs.append(src/'minigzipsh')
        runtime = dict(environment, LD_LIBRARY_PATH=str(src))
        for number, program in enumerate(programs):
            compressed = self.run([program, '-c', payload], cwd=src, env=runtime).stdout
            gzip_outputs.append(compressed)
            archive = build/f'payload-{number}.gz'
            archive.write_bytes(compressed)
            # Every Fas/GNU producer is decoded by every consumer.
            for consumer in programs:
                decoded = self.run([consumer, '-d', '-c', archive], cwd=src, env=runtime).stdout
                assert decoded == payload.read_bytes(), 'gzip cross-consumer mismatch'
        assert all(data == gzip_outputs[0] for data in gzip_outputs), 'gzip bytes differ from GNU ld'
        if not static:
            shared = src/'libz.so.1.3.1'
            artifacts[shared.name] = {'sha256': digest(shared)}
            assert not self.run(['readelf', '-aW', shared]).stderr
            # Independently linked consumers also load the Fas-produced DSO.
            oracle = self.hosted(build/'examplesh-gnu', [src/'example.o'], [shared], tool='ld')
            assert (self.run([src/'examplesh'], cwd=src, env=runtime).stdout
                    == self.run([oracle], cwd=src, env=runtime).stdout)
        return {'upstream_make_tests': result.stdout.decode(errors='replace'),
                'actual_linker_invocations': len(invocations), 'artifacts': artifacts,
                'gzip_producers': len(programs), 'gzip_cross_consumer_runs': len(programs)**2,
                'payload_sha256': digest(payload), 'gzip_sha256': hashlib.sha256(gzip_outputs[0]).hexdigest(),
                'scope': 'GCC upstream configure/make, examples, gzip, large-file API; GNU consumers'}

    def chibicc(self, source):
        src = self.snapshot('chibicc', source)
        build = src.parent
        objects = [self.compile(p, build/(p.stem+'.o'), ('-std=c11', '-fno-common'))
                   for p in sorted(src.glob('*.c'))]
        compiler = self.hosted(build/'chibicc', objects)
        self.run([compiler, '--help'])
        common = build/'common.o'
        self.compile(src/'test/common', common, ('-x', 'c'))
        for source in sorted((src/'test').glob('*.c')):
            def check(source=source):
                obj = build/(source.stem+'-test.o')
                # Upstream macro.c asserts that __FILE__ is "test/macro.c".
                self.run([compiler, '-Iinclude', '-Itest', '-c',
                          source.relative_to(src), '-o', obj], cwd=src)
                outputs = []
                for tool, tag in ((self.linker, 'fas'), ('ld', 'gnu')):
                    program = self.hosted(build/(source.stem+'-'+tag), [obj, common],
                                          tool=tool, pie=False)
                    outputs.append(self.run([program], cwd=src).stdout)
                assert outputs[0] == outputs[1], 'Fas/GNU program output differs'
                return {'stdout_sha256': hashlib.sha256(outputs[0]).hexdigest()}
            self.test('chibicc/'+source.name, check)
        return {'objects': len(objects), 'tests': len(list((src/'test').glob('*.c')))}

    def chibicc_selfhost(self, source):
        src = self.snapshot('chibicc-selfhost', source)
        build = src.parent
        tools = build/'toolchain'
        tools.mkdir()
        calls = build/'linker-invocations.jsonl'
        gcc = shutil.which('gcc')
        for name, body in (
            ('ld', f'''import json,os,sys
with open({str(calls)!r},'a') as out: out.write(json.dumps(sys.argv[1:])+'\\n')
os.execv({str(self.linker)!r},[{str(self.linker)!r},*sys.argv[1:]])
'''),
            ('cc', f'''import os,sys
os.execv({gcc!r},[{gcc!r},{'-B'+str(tools)+'/'!r},'-fno-use-linker-plugin','-fPIC',*sys.argv[1:]])
''')):
            path = tools/name
            path.write_text('#!'+sys.executable+'\n'+body)
            path.chmod(0o755)
        # CC routes upstream Makefile links; PATH also routes Chibicc's own
        # driver and the driver script's literal cc calls through these tools.
        environment = dict(os.environ, CC=str(tools/'cc'),
                           PATH=str(tools)+os.pathsep+os.environ.get('PATH', ''))
        self.run(['make', 'clean'], cwd=src, env=environment)
        self.run(['nice', '-n', '10', 'make', '-j1', 'chibicc', 'stage2/chibicc'],
                 cwd=src, env=environment, timeout=300)
        tested = self.run(['nice', '-n', '10', 'make', '-j1', 'test', 'test-stage2'],
                          cwd=src, env=environment, timeout=300)
        assert tested.stdout.count(b'testing --help ... passed') == 2, 'both driver suites must run'
        common = self.compile(src/'test/common', build/'common.o', ('-x', 'c'))
        programs = sorted((src/'test').glob('*.c'))
        assert programs, 'upstream C test set is empty'
        compared = []
        for stage in ('', 'stage2'):
            for source in programs:
                stem = source.stem
                obj = src/stage/'test'/(stem+'.o')
                program = src/stage/'test'/(stem+'.exe')
                oracle = self.hosted(build/((stage or 'stage1')+'-'+stem+'-gnu'),
                                     [obj, common], tool='ld')
                actual = self.run([program], cwd=src).stdout
                expected = self.run([oracle], cwd=src).stdout
                assert actual == expected, f'{stage or "stage1"}/{stem}: GNU output differs'
                compared.append({'stage': stage or 'stage1', 'test': source.name,
                                 'fas_sha256': digest(program), 'gnu_sha256': digest(oracle),
                                 'stdout_sha256': hashlib.sha256(actual).hexdigest()})
        invocations = [json.loads(line) for line in calls.read_text().splitlines()]
        compilers = {stage or 'stage1': digest(src/stage/'chibicc') for stage in ('', 'stage2')}
        return {'compiler_sha256': compilers, 'tests_per_stage': len(programs),
                'gnu_runtime_comparisons': compared,
                'upstream_driver_runs': 2,
                'upstream_driver_checks': len(re.findall(rb'^testing .* passed$', tested.stdout, re.M)),
                'actual_linker_invocations': len(invocations),
                'scope': 'Unmodified upstream Makefile, both compiler stages, C tests and driver scripts; GCC native bootstrap with PIC and no plugin'}

    def sieve(self, source):
        src = self.snapshot('sieve', source)
        build = src.parent
        paths = sorted(src.glob('*.fas'))
        if not paths:
            raise RuntimeError('Sieve corpus has no .fas sources')
        backends = ('llvm', 'native') if self.args.sieve_backend == 'both' else (self.args.sieve_backend,)
        rows = []
        for path in paths:
            for backend in backends:
                def check(path=path, backend=backend):
                    flags = ['--backend=native'] if backend == 'native' else []
                    env = dict(os.environ, FAS_FIR='')
                    if backend == 'native':
                        # A real native build must not fall back to opt/llc.
                        env.update(FAS_OPT='/nonexistent/opt', FAS_LLC='/nonexistent/llc')
                    obj = build/(path.stem+'-'+backend+'.o')
                    self.run([self.compiler, *flags, '-O3', '-c', path, '-o', obj],
                             env=env, timeout=240)
                    executions = []
                    def execute(program, producer):
                        result = self.run([program], timeout=30)
                        line = result.stdout.strip()
                        match = re.fullmatch(
                            rb'fas_extreme-hybrid;([0-9]+);([0-9]+)\.([0-9]{9});1;'
                            rb'algorithm=base,faithful=yes,bits=1', line)
                        assert match and int(match[1]) > 0, result.stdout
                        assert not result.stderr, result.stderr
                        executions.append({'producer': producer, 'path': str(program),
                                           'sha256': digest(program),
                                           'stdout': result.stdout.decode()})
                    for tool, tag in ((self.linker, 'fas'), ('ld', 'gnu')):
                        program = self.hosted(build/(path.stem+'-'+backend+'-'+tag),
                                              [obj], tool=tool, pie=False)
                        execute(program, tag)
                    if backend == 'native':
                        driver = build/(path.stem+'-native-driver')
                        mode = ['-Wl,-static'] if self.args.static else []
                        self.run([self.compiler, *flags, '-O3', path, *mode, '-o', driver],
                                 env=env, timeout=240)
                        execute(driver, 'native-driver')
                    return {'backend': backend, 'object_sha256': digest(obj),
                            'executions': executions,
                            'validation': 'unmodified built-in EXPECTED prime-count check',
                            'timing_comparison': False}
                rows.append(self.test('sieve/'+path.name+'/'+backend, check))
        failures = [row['name'] for row in rows if row['status'] != 'passed']
        if failures:
            raise AssertionError('Sieve qualification failed: '+', '.join(failures))
        return {'sources': len(paths), 'backends': list(backends),
                'native_driver': 'native' in backends, 'oracle': 'GNU ld',
                'timing_comparison': False}

    def cjson(self, source):
        src = self.snapshot('cjson', source)
        build = src.parent
        unity = self.compile(src/'tests/unity/src/unity.c', build/'unity.o')
        utils = self.compile(src/'cJSON_Utils.c', build/'utils.o')
        names = re.search(r'set\(unity_tests\s+(.*?)\)',
                          (src/'tests/CMakeLists.txt').read_text(), re.S).group(1).split()
        names += ['json_patch_tests', 'old_utils_tests', 'misc_utils_tests']
        # Several upstream tests include cJSON.c to exercise private parser
        # functions. They must not also link a second cJSON implementation.
        for name in names:
            def check(name=name):
                source = src/'tests'/(name+'.c')
                obj = self.compile(source, build/(name+'.o'))
                objects = [obj, unity]
                if name in ('json_patch_tests', 'old_utils_tests', 'misc_utils_tests'):
                    objects.append(utils)
                outputs = []
                for tool, tag in ((self.linker, 'fas'), ('ld', 'gnu')):
                    program = self.hosted(build/(name+'-'+tag), objects,
                                          ['/usr/lib/x86_64-linux-gnu/libm.so.6'], tool=tool)
                    result = self.run([program], cwd=src/'tests')
                    assert b'0 Failures' in result.stdout, result.stdout
                    outputs.append(result.stdout)
                assert outputs[0] == outputs[1], 'cJSON test output differs'
                return {'stdout_sha256': hashlib.sha256(outputs[0]).hexdigest()}
            self.test('cjson/'+name, check)
        return {'upstream_test_programs': len(names)}

    def doom(self, source):
        src = self.snapshot('doom', source)
        build = src.parent
        flags = ('-std=gnu89', '-fcommon', '-Wno-error=implicit-function-declaration',
                 '-Wno-error=incompatible-pointer-types', '-DNORMALUNIX', '-DLINUX')
        objects = [self.compile(p, build/(p.stem+'.o'), flags, src)
                   for p in sorted(src.glob('*.c')) if p.stem != 'sndserv']
        libraries = [Path('/usr/lib/x86_64-linux-gnu')/name
                     for name in ('libXext.so', 'libX11.so', 'libm.so.6')]
        outputs = []
        for tool, tag in ((self.linker, 'fas'), ('ld', 'gnu')):
            for pie in (False, True):
                program = self.hosted(build/f'doom-{tag}-{pie}', objects, libraries, tool=tool, pie=pie)
                result = self.run([program, '-iwad', build/'absent.wad'], cwd=build,
                                  env=dict(os.environ, DISPLAY=''), expected=255)
                assert b'W_InitFiles: no files found' in result.stderr, result.stderr
                outputs.append((result.stdout, result.stderr))
        assert all(value == outputs[0] for value in outputs), 'Doom startup differs'
        detail = {'objects': len(objects), 'modes': ['fixed', 'pie'], 'oracle': 'GNU ld'}
        if self.args.doom_wad:
            detail['rendering'] = self.doom_render(src, build, objects, libraries, flags)
        else:
            self.report['limitations'].append({'project': 'doom',
                'scope': 'link/startup failure path; pass --doom-wad for bounded headless rendering'})
        return detail

    def doom_render(self, src, build, objects, libraries, flags):
        wad_source = self.args.doom_wad.resolve()
        # Linux Doom 1.10 predates -iwad and discovers lowercase names here.
        if wad_source.name.lower() not in ('doom1.wad', 'doom.wad', 'doomu.wad',
                                            'doom2.wad', 'doom2f.wad'):
            raise RuntimeError('Doom 1.10 requires a recognized IWAD filename')
        wad = build/wad_source.name.lower()
        shutil.copy2(wad_source, wad)
        self.report['sources']['doom-wad'] = {'path': str(wad_source),
                                              'sha256': digest(wad), 'snapshot': str(wad)}
        changes = []
        def replace(path, before, after):
            text = path.read_text()
            assert text.count(before) == 1, f'unknown Doom fixture: {path.name}'
            path.write_text(text.replace(before, after))
            changes.append({'path': str(path.relative_to(src)),
                            'before': before, 'after': after})
        # Keep all gameplay/rendering code. Replace only display/audio device
        # interaction in the snapshot and advance exactly one game tic per frame.
        replace(src/'i_video.c', 'void I_InitGraphics(void)\n{',
                'void I_InitGraphics(void)\n{\n    singletics = true;\n'
                '    wipegamestate = GS_LEVEL;\n    return;')
        replace(src/'i_video.c', 'void I_SetPalette (byte* palette)\n{',
                'void I_SetPalette (byte* palette)\n{\n    return;')
        replace(src/'i_video.c', 'void I_FinishUpdate (void)\n{',
                'extern void corpus_frame(const unsigned char *);\n'
                'void I_FinishUpdate (void)\n{\n    corpus_frame(screens[0]);\n    return;')
        sound = (src/'i_sound.c').read_text()
        start = sound.index('  // Secure and configure sound device first.')
        end = sound.index('  // Initialize external data (all sounds) at start, keep static.', start)
        replace(src/'i_sound.c', sound[start:end],
                '  /* Headless device sink; keep sound loading and mixing. */\n'
                '  audio_fd = open("/dev/null", O_WRONLY);\n\n')
        hook = build/'doom-frame.c'
        hook.write_text(r'''
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include "doomstat.h"
void corpus_frame(const unsigned char *pixels){
  static int frames,last_tic=-1;
  static uint64_t hash=UINT64_C(14695981039346656037);
  if(gamestate!=GS_LEVEL || gametic==last_tic)return;
  last_tic=gametic;
  for(int n=0;n<SCREENWIDTH*SCREENHEIGHT;n++){
    hash^=pixels[n];hash*=UINT64_C(1099511628211);
  }
  if(++frames==32){
    printf("frames=32 gametic=%d framebuffer=%016llx\n",gametic,(unsigned long long)hash);
    exit(0);
  }
}
''')
        objects = [obj for obj in objects if obj.stem not in ('i_video', 'i_sound')]
        objects += [self.compile(src/(name+'.c'), build/(name+'-probe.o'), flags, src)
                    for name in ('i_video', 'i_sound')]
        objects.append(self.compile(hook, build/'doom-frame.o', ('-I'+str(src), '-std=gnu99')))
        (build/'instrumentation.json').write_text(json.dumps(changes, indent=2)+'\n')
        config = build/'empty.cfg'
        config.write_text('')
        outputs = []
        for tool, tag in ((self.linker, 'fas'), ('ld', 'gnu')):
            for pie in (False, True):
                program = self.hosted(build/f'render-{tag}-{pie}', objects, libraries,
                                      tool=tool, pie=pie)
                result = self.run([program, '-config', config,
                                   '-warp', '1', '1', '-skill', '3'], cwd=build,
                                  env=dict(os.environ, DISPLAY='', DOOMWADDIR=str(build)), timeout=30)
                checksum = re.search(rb'frames=32 gametic=32 framebuffer=[0-9a-f]{16}',
                                     result.stdout)
                assert checksum, result.stdout
                outputs.append(checksum.group())
        assert all(value == outputs[0] for value in outputs), 'Doom framebuffer differs'
        return {'instrumented_runtime': outputs[0].decode(), 'oracle': 'GNU ld',
                'modes': ['fixed', 'pie'], 'frames': 32,
                'instrumentation_sha256': digest(build/'instrumentation.json'),
                'hook_sha256': digest(hook),
                'scope': 'E1M1 skill 3, 32 single-tic frames, headless device harness; no input/gameplay acceptance'}

    def wolf3d(self, source):
        src = self.snapshot('wolf3d', source)
        build = src.parent
        flags = ('-std=gnu99', *shlex.split(self.run(['sdl-config', '--cflags']).stdout.decode()))
        libraries = [Path('/usr/lib/x86_64-linux-gnu')/name
                     for name in ('libSDL.so', 'libSDL_mixer.so', 'libm.so.6')]
        files = sorted(src.glob('*.c')) + sorted((src/'mame').glob('*.c'))
        objects = [self.compile(p, build/(p.stem+'.o'), flags, src) for p in files]
        for tool, tag in ((self.linker, 'fas'), ('ld', 'gnu')):
            self.hosted(build/('wolf3d-'+tag), objects, libraries, tool=tool)
        if not self.args.wolf_data:
            self.report['limitations'].append({'project': 'wolf3d', 'scope': 'link only; pass --wolf-data for headless rendering'})
            return {'objects': len(objects), 'runtime': 'unverified'}
        data = self.snapshot('wolf-assets', self.args.wolf_data)
        # Instrument only the snapshot. Enter E1M1, fix the random seed and hash
        # the first three rendered game frames, bypassing menu interaction.
        changes = []
        def replace(path, old, new):
            text = path.read_text()
            assert old in text, f'unknown Wolf3D fixture: {path.name}'
            path.write_text(text.replace(old, new))
            changes.append({'path': str(path.relative_to(src)), 'before': old, 'after': new})
        replace(src/'wl_main.c', '    InitGame();', '    InitGame();\n    NewGame();\n    GameLoop();\n    return 0;')
        for name in ('wl_main.c', 'wl_game.c'):
            replace(src/name, 'US_InitRndT (true);', 'US_InitRndT (false);')
        replace(src/'wl_draw.c', 'void    ThreeDRefresh (void)',
                'extern void corpus_frame(SDL_Surface *);\nvoid    ThreeDRefresh (void)')
        replace(src/'wl_draw.c', '        SDL_Flip(screen);', '        SDL_Flip(screen);\n        corpus_frame(screenBuffer);')
        hook = build/'frame.c'
        hook.write_text(r'''
#include <SDL.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
void corpus_frame(SDL_Surface *surface){
  static int frames;static uint64_t hash=UINT64_C(14695981039346656037);
  for(int y=0;y<surface->h;y++)for(int x=0;x<surface->w;x++){
    hash^=((unsigned char*)surface->pixels)[y*surface->pitch+x];
    hash*=UINT64_C(1099511628211);
  }
  if(++frames==3){printf("frames=3 framebuffer=%016llx\n",(unsigned long long)hash);exit(0);}
}
''')
        modified = {'wl_main', 'wl_game', 'wl_draw'}
        objects = [obj for obj in objects if obj.stem not in modified]
        objects += [self.compile(src/(name+'.c'), build/(name+'-probe.o'), flags, src)
                    for name in sorted(modified)]
        objects.append(self.compile(hook, build/'frame.o', flags))
        (build/'instrumentation.json').write_text(json.dumps(changes, indent=2)+'\n')
        outputs = []
        for tool, tag in ((self.linker, 'fas'), ('ld', 'gnu')):
            program = self.hosted(build/('probe-'+tag), objects, libraries, tool=tool)
            result = self.run([program], cwd=data,
                              env=dict(os.environ, SDL_VIDEODRIVER='dummy', SDL_AUDIODRIVER='dummy'), timeout=30)
            checksum = re.search(rb'frames=3 framebuffer=[0-9a-f]{16}', result.stdout)
            assert checksum, result.stdout
            outputs.append(checksum.group())
        assert outputs[0] == outputs[1], 'Wolf3D framebuffer differs from GNU ld'
        return {'objects': len(objects), 'instrumented_runtime': outputs[0].decode(),
                'oracle': 'GNU ld', 'scope': 'first three E1M1 frames, deterministic harness'}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output', required=True, type=Path)
    parser.add_argument('--linker', type=Path, default=ROOT/'_build/default/src/linker/bin/main.exe')
    parser.add_argument('--compiler', type=Path, default=ROOT/'_build/default/bin/main.exe')
    parser.add_argument('--sieve-backend', choices=('llvm', 'native', 'both'), default='both',
                        help='required Sieve backends (default: both); failures fail qualification')
    parser.add_argument('--doom-wad', type=Path,
                        help='local Doom WAD for a bounded 32-frame headless rendering check')
    parser.add_argument('--sqlite-tcl-config', type=Path,
                        help='SQLite --with-tcl directory containing tclConfig.sh')
    parser.add_argument('--sqlite-tclsh', type=Path,
                        help='SQLite code-generation Tcl interpreter (otherwise upstream discovery)')
    parser.add_argument('--sqlite-extraquick', action='store_true',
                        help='run the upstream isolated extraquick selection instead of main.test')
    parser.add_argument('--strict-dependencies', action='store_true')
    linkage = parser.add_mutually_exclusive_group()
    linkage.add_argument('--static', action='store_true',
                         help='static ET_EXEC glibc corpus (Chibicc/Sieve/cJSON/zlib only)')
    linkage.add_argument('--static-pie', action='store_true',
                         help='CRT-relocated static PIE glibc corpus (cJSON/zlib PIC objects only)')
    parser.add_argument('--dependency-path', action='append', type=Path, default=[])
    for name in ('sieve', 'chibicc', 'chibicc-selfhost', 'cjson', 'zlib', 'sqlite', 'doom', 'wolf3d', 'wolf-data'):
        parser.add_argument('--'+name, type=Path)
    args = parser.parse_args()
    if args.doom_wad and not args.doom:
        parser.error('--doom-wad requires --doom')
    if not any((args.sieve, args.chibicc, args.chibicc_selfhost, args.cjson, args.zlib, args.sqlite, args.doom, args.wolf3d)):
        parser.error('select at least one source project')
    if (args.sqlite_tcl_config or args.sqlite_tclsh or args.sqlite_extraquick) and not args.sqlite:
        parser.error('--sqlite-* options require --sqlite')
    if args.sqlite and (args.static or args.static_pie or args.strict_dependencies):
        parser.error('--sqlite runs its own archive/shared/fixed/PIE/static/static-PIE matrix; select it separately')
    if args.chibicc_selfhost and (args.static or args.static_pie or args.strict_dependencies):
        parser.error('--chibicc-selfhost qualifies the upstream driver policy; use --chibicc for alternate raw-link modes')
    if args.dependency_path and not args.strict_dependencies:
        parser.error('--dependency-path requires --strict-dependencies')
    if args.static and (args.doom or args.wolf3d):
        parser.error('--static is declared only for Chibicc/Sieve/cJSON/zlib; game static dependency closure is unqualified')
    if args.static_pie and (args.chibicc or args.sieve or args.doom or args.wolf3d):
        parser.error('--static-pie currently qualifies cJSON/zlib PIC inputs only; other object/dependency policies need separate gates')
    if (args.static or args.static_pie) and args.strict_dependencies:
        parser.error('static modes have no DSO dependency graph to validate')
    corpus = Corpus(args)
    for name in ('chibicc', 'chibicc_selfhost', 'sieve', 'cjson', 'zlib', 'sqlite', 'doom', 'wolf3d'):
        path = getattr(args, name)
        if path:
            corpus.test(name, lambda name=name, path=path: getattr(corpus, name)(path))
    corpus.save()
    raise SystemExit(any(test['status'] != 'passed' for test in corpus.report['tests']))


if __name__ == '__main__':
    main()
