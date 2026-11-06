#!/usr/bin/env python3
"""Real zlib objects with GCC/Clang and isolated musl: PIE/static/static PIE."""
import argparse
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import shutil
import struct
import sys
from types import SimpleNamespace

HERE = Path(__file__).resolve().parent
base = HERE/'corpus-run.py' if (HERE/'corpus-run.py').is_file() else HERE/'run.py'
spec = importlib.util.spec_from_file_location('corpus_base', base)
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)
Corpus, ROOT, digest = module.Corpus, module.ROOT, module.digest


PROGRAMS = ('example', 'minigzip', 'example64', 'minigzip64')


def inspect(corpus, path, mode, interpreter):
    data = path.read_bytes()
    assert data[:6] == b'\x7fELF\x02\x01', 'expected little-endian ELF64'
    assert struct.unpack_from('<H', data, 16)[0] == (2 if mode == 'static' else 3)
    offset = struct.unpack_from('<Q', data, 32)[0]
    count = struct.unpack_from('<H', data, 56)[0]
    headers = [struct.unpack_from('<IIQQQQQQ', data, offset+n*56) for n in range(count)]
    interps = [data[h[2]:h[2]+h[5]].rstrip(b'\0').decode() for h in headers if h[0] == 3]
    assert interps == ([str(interpreter)] if mode == 'pie' else [])
    dynamic = [h for h in headers if h[0] == 2]
    if mode == 'static-pie':
        assert len(dynamic) == 1
        h = dynamic[0]
        tags = [struct.unpack_from('<qQ', data, n)[0] for n in range(h[2], h[2]+h[5], 16)]
        assert 1 not in tags and 22 not in tags, 'DT_NEEDED or DT_TEXTREL in static PIE'
    elif mode == 'static':
        assert not dynamic
    result = corpus.run(['readelf', '-aW', path])
    assert not result.stderr, result.stderr


def qualify(corpus, source, sysroot, compiler, number):
    tag = str(number)+'-'+Path(compiler).name
    src = corpus.snapshot('musl-zlib-'+tag, source)
    work = src.parent
    tools = work/'toolchain'
    tools.mkdir()
    version = corpus.run([compiler, '--version']).stdout.decode(errors='replace')
    family = 'clang' if 'clang' in version.lower() else 'gcc'
    if family == 'clang':
        command = [compiler, '--target=x86_64-linux-musl', '--sysroot='+str(sysroot)]
    else:
        command = [compiler, '-specs='+str(sysroot/'lib/musl-gcc.specs')]
    cc = tools/'cc'
    cc.write_text('#!'+sys.executable+'\nimport os,sys\n'+
                  f'os.execv({compiler!r},{command!r}+sys.argv[1:])\n')
    cc.chmod(0o755)
    environment = dict(os.environ, CC=str(sysroot/'bin/musl-gcc'),
                       CFLAGS='-O2 -g -fPIC -ffunction-sections -fdata-sections', LC_ALL='C')
    # Upstream configure links its probes with the installed musl GCC driver.
    # The selected compiler then builds every payload object; final links are
    # explicit below. This does not claim Clang driver/startup discovery support.
    corpus.run(['./configure', '--static'], cwd=src, env=environment)
    environment['CC'] = str(cc)
    corpus.run(['nice', '-n', '10', 'make', '-j2', 'libz.a',
                *[p+'.o' for p in PROGRAMS]], cwd=src, env=environment, timeout=300)
    native = [src/'libz.a', *[src/(p+'.o') for p in PROGRAMS]]
    native_hashes = {str(p): digest(p) for p in native}
    libc = sysroot/'lib'
    interpreter = libc/'ld-musl-x86_64.so.1'
    support = {n: Path(corpus.run(['gcc', '-print-file-name='+n]).stdout.decode().strip()).resolve()
               for n in ('crtbeginS.o', 'crtbeginT.o', 'crtendS.o', 'crtend.o', 'libgcc.a', 'libgcc_eh.a')}
    assert all(p.is_file() for p in support.values())
    toolchain = [libc/n for n in ('crt1.o', 'Scrt1.o', 'rcrt1.o', 'crti.o', 'crtn.o', 'libc.a', 'libc.so')]
    detail = {'compiler': compiler, 'compiler_sha256': digest(Path(compiler)), 'version': version,
              'object_compile_command': command, 'configure_compiler': str(sysroot/'bin/musl-gcc'),
              'native_input_sha256': native_hashes,
              'toolchain_inputs': {str(p): digest(p) for p in [*toolchain, *support.values()]},
              'modes': [], 'scope': 'musl zlib static archive consumers; no zlib DSO or Clang driver-policy claim'}
    corpus.report.setdefault('musl_zlib', []).append(detail)
    corpus.save()
    payload = work/'payload.bin'
    payload.write_bytes(bytes(range(256))*1024+b'Fas musl gzip\n'*4096)
    expected = payload.read_bytes()
    for mode in ('pie', 'static', 'static-pie'):
        row = {'mode': mode, 'phases': []}
        detail['modes'].append(row)
        directories = {}
        for phase, linker in [('fas', corpus.linker), ('gnu', shutil.which('ld'))]:
            directory = work/mode/phase
            directory.mkdir(parents=True)
            directories[phase] = directory
            programs = {}
            for name in PROGRAMS:
                output = directory/name
                flags = ['--gc-sections', '--eh-frame-hdr']
                if mode == 'pie':
                    flags += ['-pie', '--dynamic-linker', str(interpreter)]
                elif mode == 'static-pie':
                    flags += ['-static', '-pie', '--no-dynamic-linker', '-z', 'text']
                else:
                    flags += ['-static']
                start = 'crt1.o' if mode == 'static' else 'rcrt1.o' if mode == 'static-pie' else 'Scrt1.o'
                corpus.run([linker, *flags, '-o', output, libc/start, libc/'crti.o',
                            support['crtbeginT.o' if mode == 'static' else 'crtbeginS.o'],
                            src/(name+'.o'), src/'libz.a', '--start-group',
                            libc/('libc.so' if mode == 'pie' else 'libc.a'),
                            support['libgcc.a'], support['libgcc_eh.a'], '--end-group',
                            support['crtend.o' if mode == 'static' else 'crtendS.o'], libc/'crtn.o'])
                inspect(corpus, output, mode, interpreter)
                programs[name] = digest(output)
                shutil.copy2(output, src/name)
            result = corpus.run(['make', 'test', 'test64'], cwd=src, env=environment)
            assert b'*** zlib test OK ***' in result.stdout and b'*** zlib 64-bit test OK ***' in result.stdout, 'upstream test selection did not execute'
            assert all(digest(src/name) == h for name, h in programs.items()), 'upstream make replaced a qualified executable'
            examples = {name: hashlib.sha256(corpus.run([directory/name], cwd=directory).stdout).hexdigest()
                        for name in ('example', 'example64')}
            row['phases'].append({'linker': phase, 'outputs': programs,
                                  'upstream_stdout': result.stdout.decode(errors='replace'),
                                  'example_stdout_sha256': examples})
            corpus.save()
        assert row['phases'][0]['example_stdout_sha256'] == row['phases'][1]['example_stdout_sha256']
        programs = [directory/name for directory in directories.values() for name in ('minigzip', 'minigzip64')]
        packed_hashes = []
        for number, producer in enumerate(programs):
            packed = corpus.run([producer, '-c', payload]).stdout
            packed_hashes.append(hashlib.sha256(packed).hexdigest())
            archive = work/mode/('payload-'+str(number)+'.gz')
            archive.write_bytes(packed)
            for consumer in programs:
                assert corpus.run([consumer, '-d', '-c', archive]).stdout == expected, 'cross-consumer payload mismatch'
        assert len(set(packed_hashes)) == 1, 'Fas/GNU gzip bytes differ'
        row.update(gzip_cross_consumer_runs=len(programs)**2, gzip_sha256=packed_hashes[0], passed=True)
        corpus.save()
    assert all(digest(Path(p)) == h for p, h in native_hashes.items()), 'compiled inputs changed'
    detail['inputs_unchanged'] = True
    return {'compiler': compiler, 'modes': 3, 'final_links': 24, 'gzip_cross_consumer_runs': 48}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--zlib', type=Path, required=True)
    parser.add_argument('--musl-sysroot', type=Path, required=True)
    parser.add_argument('--cc', action='append', help='GCC or Clang executable; repeatable')
    parser.add_argument('--linker', type=Path, default=ROOT/'_build/default/src/linker/bin/main.exe')
    args = parser.parse_args()
    compilers = args.cc or ['gcc', 'clang']
    paths = [shutil.which(c) for c in compilers]
    if not all(paths):
        parser.error('every requested compiler must be installed')
    options = SimpleNamespace(output=args.output, linker=args.linker, compiler=Path('/unused'),
                              sieve=None, sqlite=None, static=False, static_pie=False,
                              dependency_path=[], strict_dependencies=False)
    corpus = Corpus(options)
    shutil.copy2(Path(__file__), corpus.root/'tools/musl.py')
    corpus.report['musl_harness_sha256'] = digest(Path(__file__))
    corpus.report['musl_sysroot'] = str(args.musl_sysroot.resolve())
    corpus.report['limitations'] += ['Native zlib objects only; configure probes use musl-gcc/GNU.',
                                    'No C++/LTO, zlib DSO, static TLS relaxation or cross-kernel qualification.']
    corpus.save()
    for number, compiler in enumerate(paths):
        corpus.test('musl-zlib-'+str(number)+'-'+Path(compiler).name,
                    lambda c=compiler, n=number: qualify(corpus, args.zlib.resolve(),
                                                       args.musl_sysroot.resolve(), c, n))
    raise SystemExit(int(any(t['status'] != 'passed' for t in corpus.report['tests'])))


if __name__ == '__main__':
    main()
