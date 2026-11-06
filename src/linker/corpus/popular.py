#!/usr/bin/env python3
"""Pinned real C project builds, upstream tests and same-object GNU replays."""
import argparse
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
from types import SimpleNamespace

HERE = Path(__file__).resolve().parent
base = HERE/'corpus-run.py' if (HERE/'corpus-run.py').is_file() else HERE/'run.py'
spec = importlib.util.spec_from_file_location('corpus_base', base)
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)
Corpus, digest, ROOT = module.Corpus, module.digest, module.ROOT
PROJECTS = ('lua', 'bzip2', 'libpng', 'libarchive', 'curl', 'redis')
REDIS_UNITS = ('unit/auth', 'unit/protocol', 'unit/keyspace', 'unit/type/string',
               'unit/type/list', 'unit/type/set', 'unit/type/zset', 'unit/type/hash',
               'unit/other', 'unit/multi', 'unit/scripting', 'unit/dump',
               'integration/rdb', 'integration/aof')


class Project:
    def __init__(self, corpus, name, source):
        self.c, self.name = corpus, name
        self.root = corpus.root/name
        self.src = self.root/'source'
        # libarchive ships essential CMake modules in a directory named build.
        shutil.copytree(source.resolve(), self.src,
                        ignore=shutil.ignore_patterns('.git', '__pycache__'))
        corpus.report['sources'][name] = {'path': str(source.resolve()),
            'sha256': {str(p.relative_to(self.src)): digest(p)
                       for p in sorted(self.src.rglob('*')) if p.is_file()}}
        self.build = self.root/'build'
        self.tools = self.root/'toolchain'
        self.tools.mkdir()
        self.calls = self.root/'linker-invocations.jsonl'
        self.active = self.tools/'active-linker'
        self.active.write_text(str(corpus.linker))
        self.saved = self.root/'retained-inputs'
        self.saved.mkdir()
        ld = self.tools/'ld'
        ld.write_text('#!'+sys.executable+'\n'+f'''import hashlib,json,os,shutil,subprocess,sys,tempfile
from pathlib import Path
args=sys.argv[1:]; replay=list(args); hashes={{}}; retained={{}}
output_index=args.index('-o')+1 if '-o' in args else -1
for i,arg in enumerate(args):
    p=Path(arg).resolve()
    if i!=output_index and p.is_file() and p.suffix in ('.o','.a'):
        hashes[str(p)]=hashlib.sha256(p.read_bytes()).hexdigest()
        if p.suffix=='.o' and not p.is_relative_to({str(self.root)!r}) and not p.is_relative_to('/usr'):
            d=Path(tempfile.mkdtemp(dir={str(self.saved)!r})); q=d/p.name
            shutil.copy2(p,q); replay[i]=str(q)
        retained[str(p)]=str(Path(replay[i]).resolve())
tool=Path({str(self.active)!r}).read_text().strip()
result=subprocess.run([tool,*args])
output=None
if '-o' in args: output=str(Path(args[args.index('-o')+1]).resolve())
with open({str(self.calls)!r},'a') as f:
    f.write(json.dumps({{'tool':tool,'argv':args,'replay':replay,'cwd':os.getcwd(),
                        'output':output,'status':result.returncode,'input_sha256':hashes,
                        'input_replay':retained}})+'\\n')
sys.exit(result.returncode)
''')
        ld.chmod(0o755)
        self.cc = self.tools/'cc'
        self.cc.write_text('#!'+sys.executable+'\n'+f'''import os,sys
os.execv({shutil.which('gcc')!r},[{shutil.which('gcc')!r},{'-B'+str(self.tools)+'/'!r},'-fno-use-linker-plugin',*sys.argv[1:]])
''')
        self.cc.chmod(0o755)
        # curl's IDN URL tests intentionally use international hostnames. The
        # system libidn2 converts from the process locale, so ASCII C rejects
        # valid input under GNU ld as well as Fas. Keep the UTF-8 locale explicit.
        test_locale = 'C.UTF-8' if name == 'curl' else 'C'
        self.env = dict(os.environ, CC=str(self.cc), LD=str(ld),
                        CFLAGS='-O2 -g -fPIC', LC_ALL=test_locale, TFLAGS='-j2')
        self.detail = {'upstream': [], 'oracle_outputs': [], 'timing_comparison': False,
                       'test_environment': {'TFLAGS': self.env['TFLAGS'], 'LC_ALL': test_locale}}
        self.redis_scratch = None
        if name == 'redis':
            for unit in REDIS_UNITS:
                assert (self.src/'tests'/(unit+'.tcl')).is_file(), 'missing Redis unit: '+unit
            # Redis normalizes each socket to an absolute tests/tmp path. Long
            # corpus paths exceed sockaddr_un even though TCP works. A private
            # disk-backed short scratch path preserves upstream socket tests.
            temporary = self.src/'tests/tmp'
            self.redis_scratch = Path(tempfile.mkdtemp(prefix='fas-redis-', dir='/var/tmp'))/'tests/tmp'
            self.redis_scratch.mkdir(parents=True)
            for path in temporary.iterdir():
                shutil.move(str(path), self.redis_scratch/path.name)
            temporary.rmdir()
            temporary.symlink_to(self.redis_scratch, target_is_directory=True)
            self.detail['runtime_scratch'] = str(self.redis_scratch)
        corpus.report.setdefault('popular_qualification', {})[name] = self.detail
        corpus.save()

    def run(self, args, *, cwd=None, expected=0, timeout=900):
        return self.c.run(args, cwd=cwd or self.src, env=self.env,
                          expected=expected, timeout=timeout)

    def make(self, *args, cwd=None):
        return self.run(['nice', '-n', '10', 'make', '-j2', *args], cwd=cwd)

    def cmake(self, flags):
        self.run(['cmake', '-S', self.src, '-B', self.build,
                  '-DCMAKE_C_COMPILER='+str(self.cc), '-DCMAKE_BUILD_TYPE=RelWithDebInfo',
                  '-DCMAKE_C_FLAGS=-fPIC', *flags])
        self.run(['nice', '-n', '10', 'cmake', '--build', self.build, '-j2'], timeout=1200)

    def compile(self):
        print('BUILD', self.name, flush=True)
        if self.name == 'lua':
            self.make('-C', 'src', 'all', 'CC='+str(self.cc),
                      'MYCFLAGS=-g -fPIC -DLUA_USE_POSIX -DLUA_USE_DLOPEN', 'MYLIBS=-ldl')
        elif self.name == 'bzip2':
            # The historical makefiles share object names and do not track
            # flag changes. PIC objects work for both outputs; build them first.
            self.make('-f', 'Makefile-libbz2_so', 'CC='+str(self.cc))
            self.make('all', 'CC='+str(self.cc))
        elif self.name == 'libpng':
            self.cmake(['-DPNG_TESTS=ON', '-DPNG_SHARED=ON', '-DPNG_STATIC=ON'])
        elif self.name == 'libarchive':
            self.cmake(['-DENABLE_TEST=ON', '-DENABLE_WERROR=OFF',
                        '-DENABLE_TAR_SHARED=ON', '-DENABLE_CPIO_SHARED=ON',
                        '-DENABLE_CAT_SHARED=ON', '-DENABLE_UNZIP_SHARED=ON'])
        elif self.name == 'curl':
            self.cmake(['-DBUILD_TESTING=ON', '-DBUILD_SHARED_LIBS=ON',
                        '-DCURL_USE_OPENSSL=ON', '-DCURL_USE_LIBPSL=OFF',
                        '-DUSE_NGHTTP2=OFF', '-DBUILD_LIBCURL_DOCS=OFF',
                        '-DBUILD_MISC_DOCS=OFF', '-DENABLE_CURL_MANUAL=OFF'])
            self.run(['nice', '-n', '10', 'cmake', '--build', self.build,
                      '--target', 'testdeps', '-j2'], timeout=1200)
        elif self.name == 'redis':
            self.make('MALLOC=libc', 'OPTIMIZATION=-O2', 'CC='+str(self.cc),
                      'CFLAGS=-fPIC', 'REDIS_CFLAGS=', 'REDIS_LDFLAGS=')

    def checks(self, tag):
        print('TEST', self.name, tag, flush=True)
        results = []
        def check(command, cwd=None, timeout=1200, expected=0, diagnostic=None):
            result = self.run(command, cwd=cwd, expected=None, timeout=timeout)
            row = {'argv': list(map(str, command)), 'exit': result.returncode,
                   'expected_exit': expected,
                   'passed': result.returncode == expected and
                             (diagnostic is None or diagnostic in result.stderr),
                   'stdout_sha256': hashlib.sha256(result.stdout).hexdigest(),
                   'stdout_tail': result.stdout.decode(errors='replace')[-5000:],
                   'stderr_tail': result.stderr.decode(errors='replace')[-2000:]}
            results.append(row)
            return result
        if self.name == 'lua':
            for script in ('bisect', 'cf', 'echo', 'env', 'factorial', 'fib', 'fibfor',
                           'hello', 'printf', 'readonly', 'sieve', 'sort', 'table'):
                check([self.src/'src/lua', self.src/'test'/(script+'.lua')],
                      expected=1 if script == 'readonly' else 0,
                      diagnostic=b"cannot redefine global variable `y'" if script == 'readonly' else None)
            check([self.src/'src/luac', '-o', self.root/('sort-'+tag+'.luac'),
                   self.src/'test/sort.lua'])
            check([self.src/'src/lua', self.root/('sort-'+tag+'.luac')])
        elif self.name == 'bzip2':
            check(['make', 'test', 'CC='+str(self.cc)])
            payload = self.root/'shared-input'
            payload.write_bytes(bytes(range(256))*1024)
            packed = self.root/('shared-'+tag+'.bz2')
            self.env['LD_LIBRARY_PATH'] = str(self.src)
            compressed = check([self.src/'bzip2-shared', '-c', payload])
            packed.write_bytes(compressed.stdout)
            restored = check([self.src/'bzip2', '-dc', packed])
            assert restored.stdout == payload.read_bytes(), 'shared compressor/static decompressor mismatch'
            compressed = check([self.src/'bzip2', '-c', payload])
            packed.write_bytes(compressed.stdout)
            restored = check([self.src/'bzip2-shared', '-dc', packed])
            assert restored.stdout == payload.read_bytes(), 'static compressor/shared decompressor mismatch'
        elif self.name in ('libpng', 'libarchive'):
            check(['ctest', '--test-dir', self.build, '--output-on-failure', '-j2'], timeout=1800)
            log = self.build/'Testing/Temporary/LastTest.log'
            if log.exists():
                shutil.copy2(log, self.root/('upstream-'+tag+'.log'))
        elif self.name == 'curl':
            # Upstream's nonflaky selection keeps timing-sensitive tests visible
            # as ignored results. Local test servers are managed by runtests.pl.
            check(['cmake', '--build', self.build, '--target', 'test-nonflaky'], timeout=1800)
            for log in (self.build/'tests/log').rglob('*'):
                if log.is_file() and log.stat().st_size < 4*1024*1024:
                    target = self.root/('curl-logs-'+tag)/log.relative_to(self.build/'tests/log')
                    target.parent.mkdir(parents=True, exist_ok=True)
                    shutil.copy2(log, target)
        elif self.name == 'redis':
            tcl = shutil.which('tclsh')
            # Bounded core execution corpus: no benchmark, large-memory, cluster
            # or external service. The upstream Tcl harness owns its servers.
            command = [tcl, 'tests/test_helper.tcl', '--clients', '2', '--baseport',
                       '23800' if tag == 'fas' else '24800', '--dont-clean']
            for unit in REDIS_UNITS:
                command += ['--single', unit]
            check(command, timeout=1800)
        self.detail['upstream'].append({'linker': tag, 'checks': results})
        self.c.save()
        return all(row['passed'] for row in results)

    def oracle(self):
        rows = [json.loads(line) for line in self.calls.read_text().splitlines()]
        outputs = {}
        for row in rows:
            name = row['output']
            if row['status'] == 0 and name and Path(name).is_file():
                if Path(name).read_bytes()[:4] == b'\x7fELF':
                    outputs.pop(name, None)
                    outputs[name] = row
        assert outputs, 'no successful Fas-produced ELF outputs retained'
        self.active.write_text(shutil.which('ld'))
        for name, row in outputs.items():
            path = Path(name)
            assert path.is_relative_to(self.root), 'oracle output escapes corpus'
            backup = self.root/'fas-outputs'/path.relative_to(self.root)
            backup.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(path, backup)
            for original, sha in row['input_sha256'].items():
                assert digest(Path(row['input_replay'][original])) == sha, 'compiled input changed: '+original
            result = self.c.run(['ld', *row['replay']], cwd=row['cwd'], expected=None, timeout=120)
            self.detail['oracle_outputs'].append({'path': name, 'fas_sha256': digest(backup),
                                                  'gnu_exit': result.returncode,
                                                  'gnu_sha256': digest(path) if path.exists() else None})
            self.c.save()
            assert result.returncode == 0, result.stderr.decode(errors='replace')[-3000:]
            self.run(['readelf', '-hW', path])
        self.detail['linker_invocations_sha256'] = digest(self.calls)
        return len(outputs)

    def qualify(self):
        self.compile()
        fas = self.checks('fas')
        self.detail['oracle_output_count'] = self.oracle()
        gnu = self.checks('gnu')
        self.detail['linker_invocations_sha256'] = digest(self.calls)
        self.detail['scope'] = 'Upstream build and declared test selection, GCC native objects, Fas then same-object/archive GNU replay; LTO disabled'
        if self.redis_scratch:
            temporary = self.src/'tests/tmp'
            temporary.unlink()
            shutil.move(str(self.redis_scratch), temporary)
            self.redis_scratch.parent.rmdir()
            self.redis_scratch.parent.parent.rmdir()
            self.detail['runtime_scratch_retained_at'] = str(temporary)
        self.c.save()
        assert fas and gnu, 'upstream failures retained in popular_qualification and commands.jsonl'
        return self.detail


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--project', nargs=2, action='append', required=True,
                        metavar=('PROJECT', 'SOURCE'))
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--linker', type=Path, default=ROOT/'_build/default/src/linker/bin/main.exe')
    args = parser.parse_args()
    names = [name for name, _ in args.project]
    if any(name not in PROJECTS for name in names) or len(set(names)) != len(names):
        parser.error('select each project at most once: '+', '.join(PROJECTS))
    for name, source in args.project:
        if not Path(source).is_dir(): parser.error('missing source: '+source)
    settings = SimpleNamespace(output=args.output, linker=args.linker,
                               compiler=ROOT/'_build/default/bin/main.exe', sieve=None,
                               sqlite=None, static=False, static_pie=False,
                               dependency_path=[], strict_dependencies=False)
    corpus = Corpus(settings)
    shutil.copy2(Path(__file__), corpus.root/'tools/popular.py')
    corpus.report['popular_harness_sha256'] = digest(Path(__file__))
    corpus.report['qualification_kind'] = 'real C builds and upstream tests; no timings'
    for name, source in args.project:
        corpus.test(name, lambda name=name, source=source: Project(corpus,name,Path(source)).qualify())
    raise SystemExit(any(row['status'] != 'passed' for row in corpus.report['tests']))


if __name__ == '__main__':
    main()
