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
        if self.root.is_relative_to(ROOT):
            raise ValueError('output must be outside the repository')
        self.root.mkdir(parents=True, exist_ok=False)
        linker_source = args.linker.resolve()
        tools = self.root/'tools'
        tools.mkdir()
        self.linker = tools/'fas-ld'
        shutil.copy2(linker_source, self.linker)
        self.compiler = args.compiler.resolve()
        self.report = {'started': time.strftime('%Y-%m-%dT%H:%M:%S%z'),
                       'harness_sha256': digest(Path(__file__)),
                       'linker_sha256': digest(self.linker),
                       'linker_source': str(linker_source),
                       'linker_snapshot': str(self.linker), 'tests': [], 'sources': {},
                       'limitations': [], 'timing_comparison': False}
        self.report['linkage'] = 'static-pie' if args.static_pie else 'static' if args.static else 'dynamic'
        self.dependency_paths = [str(p.resolve()) for p in args.dependency_path]
        self.report['dependency_policy'] = {
            'strict': args.strict_dependencies, 'search_paths': self.dependency_paths}
        self.crts = {}
        crt_names = ['crt1.o', 'Scrt1.o', 'crti.o', 'crtbegin.o', 'crtbeginS.o',
                     'crtend.o', 'crtendS.o', 'crtn.o', 'libgcc.a']
        if args.static or args.static_pie:
            crt_names += ['crtbeginT.o', 'libgcc_eh.a', 'libc.a', 'libm.a', 'libmvec.a']
        if args.static_pie:
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
        result = subprocess.run(args, cwd=cwd, env=env, capture_output=True, timeout=timeout)
        with (self.root / 'commands.jsonl').open('a') as log:
            log.write(json.dumps({'argv': args, 'cwd': str(cwd) if cwd else None,
                                  'environment_overrides': {k: env[k] for k in
                                      ('SDL_VIDEODRIVER', 'SDL_AUDIODRIVER', 'DISPLAY',
                                       'CC', 'CFLAGS', 'LDFLAGS', 'LD_LIBRARY_PATH')
                                      if env and k in env},
                                  'status': result.returncode,
                                  'stdout': result.stdout.decode(errors='replace'),
                                  'stderr': result.stderr.decode(errors='replace')})+'\n')
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
        for path in sorted(src.glob('*.fas')):
            def check(path=path):
                obj = build/(path.stem+'.o')
                self.run([self.compiler, '-O3', '-c', path, '-o', obj], timeout=240)
                program = self.hosted(build/path.stem, [obj], pie=False)
                result = self.run([program], timeout=30)
                assert b';1;' in result.stdout, result.stdout
                return {'backend': 'llvm', 'stdout': result.stdout.decode()}
            self.test('sieve/'+path.name, check)
        native_obj = build/'native.o'
        native = self.run([self.compiler, '--backend=native', '-O3', '-c',
                           src/'primes_zero3.fas', '-o', native_obj], expected=None, timeout=240)
        if native.returncode:
            self.report['limitations'].append({'project': 'sieve/native-backend',
                                               'diagnostic': native.stderr.decode(errors='replace')})
        else:
            program = self.hosted(build/'native', [native_obj], pie=False)
            self.run([program], timeout=30)
        return {'native_compile_status': native.returncode}

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
        self.report['limitations'].append({'project': 'doom', 'scope': 'link/startup failure path; no gameplay/WAD acceptance'})
        return {'objects': len(objects), 'modes': ['fixed', 'pie'], 'oracle': 'GNU ld'}

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
    parser.add_argument('--strict-dependencies', action='store_true')
    linkage = parser.add_mutually_exclusive_group()
    linkage.add_argument('--static', action='store_true',
                         help='static ET_EXEC glibc corpus (Chibicc/Sieve/cJSON/zlib only)')
    linkage.add_argument('--static-pie', action='store_true',
                         help='CRT-relocated static PIE glibc corpus (cJSON/zlib PIC objects only)')
    parser.add_argument('--dependency-path', action='append', type=Path, default=[])
    for name in ('sieve', 'chibicc', 'chibicc-selfhost', 'cjson', 'zlib', 'doom', 'wolf3d', 'wolf-data'):
        parser.add_argument('--'+name, type=Path)
    args = parser.parse_args()
    if not any((args.sieve, args.chibicc, args.chibicc_selfhost, args.cjson, args.zlib, args.doom, args.wolf3d)):
        parser.error('select at least one source project')
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
    for name in ('chibicc', 'chibicc_selfhost', 'sieve', 'cjson', 'zlib', 'doom', 'wolf3d'):
        path = getattr(args, name)
        if path:
            corpus.test(name, lambda name=name, path=path: getattr(corpus, name)(path))
    corpus.save()
    raise SystemExit(any(test['status'] != 'passed' for test in corpus.report['tests']))


if __name__ == '__main__':
    main()
