#!/usr/bin/env python3
"""Bounded GNU/Fas shared-library producer/consumer checks after popular.py."""
import argparse
import functools
import hashlib
import http.server
import json
import os
from pathlib import Path
import re
import shutil
import struct
import subprocess
import sys
import threading


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def shared(path):
    data = path.read_bytes()[:64]
    return (data[:5] == b'\x7fELF\x02' and struct.unpack_from('<H', data, 16)[0] == 3
            and struct.unpack_from('<Q', data, 24)[0] == 0)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--corpus', required=True, type=Path)
    parser.add_argument('--output', required=True, type=Path)
    parser.add_argument('--project', action='append', choices=('bzip2', 'libpng', 'libarchive', 'curl'))
    args = parser.parse_args()
    corpus = args.corpus.resolve()
    output = args.output.resolve()
    repository = Path(__file__).resolve().parents[3]
    assert not ((repository/'dune-project').is_file() and output.is_relative_to(repository)), 'output must be outside repository'
    output.mkdir(parents=True, exist_ok=False)
    shutil.copy2(Path(__file__), output/'cross.py')
    original = json.loads((corpus/'report.json').read_text())
    report = {'scope': 'bounded shared-library checks; both producer/consumer directions; no timings',
              'corpus': str(corpus), 'corpus_report_sha256': sha(corpus/'report.json'),
              'harness_sha256': sha(Path(__file__)), 'projects': []}
    def save():
        (output/'report.json').write_text(json.dumps(report, indent=2)+'\n')
    for name in args.project or ['bzip2', 'libpng', 'libarchive', 'curl']:
        project = corpus/name
        detail = original['popular_qualification'][name]
        assert any(row['name'] == name and row['status'] == 'passed' for row in original['tests']), 'upstream qualification must pass first: '+name
        prefix = {'bzip2': 'libbz2', 'libpng': 'libpng', 'libarchive': 'libarchive', 'curl': 'libcurl'}[name]
        libraries = [Path(row['path']) for row in detail['oracle_outputs']
                     if Path(row['path']).name.startswith(prefix) and shared(Path(row['path']))]
        assert libraries, 'no shared library: '+name
        assert all(path.is_relative_to(project) for path in libraries), 'library outside snapshot'
        work = output/name
        work.mkdir()
        env = dict(os.environ, LC_ALL='C', LD_LIBRARY_PATH=':'.join(sorted({str(p.parent) for p in libraries})))
        record = {'name': name, 'libraries': [], 'directions': []}
        report['projects'].append(record)
        if name != 'bzip2':
            # A GNU relink reuses Fas's configured feature set. Independently
            # configure with GNU to catch features silently lost to failed probes.
            command = None
            with (corpus/'commands.jsonl').open() as commands:
                for line in commands:
                    row = json.loads(line)
                    argv = row['argv']
                    if argv[:2] == ['cmake', '-S'] and argv[2] == str(project/'source'):
                        command = argv[:]
                        break
            assert command, 'original CMake recipe missing'
            command[command.index('-B')+1] = str(work/'gnu-configure')
            assert Path(project/'toolchain/active-linker').read_text().strip() == shutil.which('ld'), 'GNU control requires GNU toolchain selection'
            # Keep qualification logs immutable: independent probes get their
            # own driver instead of appending to the saved Fas/GNU link trace.
            tools = work/'gnu-tools'
            tools.mkdir()
            (tools/'ld').symlink_to(shutil.which('ld'))
            cc = tools/'cc'
            cc.write_text('#!'+sys.executable+'\n'+
                'import os,sys\n'+f'os.execv({shutil.which("gcc")!r},'
                f'[{shutil.which("gcc")!r},{"-B"+str(tools)+"/"!r},'
                "'-fno-use-linker-plugin',*sys.argv[1:]])\n")
            cc.chmod(0o755)
            command = ['-DCMAKE_C_COMPILER='+str(cc) if arg.startswith('-DCMAKE_C_COMPILER=')
                       else arg for arg in command]
            environment = dict(os.environ, CC=str(cc), LD=str(tools/'ld'),
                               CFLAGS='-O2 -g -fPIC', LC_ALL=detail['test_environment']['LC_ALL'])
            p = subprocess.run(command, cwd=project/'source', env=environment,
                               capture_output=True, timeout=900)
            log = work/'gnu-configure.log'
            log.write_bytes(p.stdout+b'\n'+p.stderr)
            assert p.returncode == 0, p.stderr.decode(errors='replace')[-3000:]
            def features(path):
                values = {}
                for line in path.read_text().splitlines():
                    match = re.fullmatch(r'([^:#][^:]*):(BOOL|INTERNAL)=(.*)', line)
                    if match:
                        key, _, value = match.groups()
                        if not key.startswith('CMAKE_') and (value in ('', 'ON', 'OFF', 'TRUE', 'FALSE', 'YES', 'NO') or value.isdigit()):
                            values[key] = value
                return values
            fas = features(project/'build/CMakeCache.txt')
            gnu = features(work/'gnu-configure/CMakeCache.txt')
            differences = {key: {'fas': fas.get(key), 'gnu': gnu.get(key)}
                           for key in sorted(fas.keys() | gnu.keys()) if fas.get(key) != gnu.get(key)}
            record['configure_control'] = {'numeric_boolean_features': len(fas),
                'differences': differences, 'gnu_log_sha256': sha(log)}
            save()
            assert not differences, 'Fas/GNU configured features differ: '+json.dumps(differences)
        backups = []
        for i, path in enumerate(libraries):
            saved = work/('gnu-library-'+str(i))
            shutil.copy2(path, saved)
            fas = project/'fas-outputs'/path.relative_to(project)
            assert fas.is_file(), 'missing retained Fas library'
            backups.append((path, saved, fas))
            record['libraries'].append({'path': str(path), 'gnu_sha256': sha(saved), 'fas_sha256': sha(fas)})
        payload = work/'payload'
        payload.write_bytes(bytes(range(256))*1024)
        def run(command, row):
            p = subprocess.run(list(map(str, command)), cwd=work, env=env,
                               capture_output=True, timeout=60)
            row['commands'].append({'argv': list(map(str, command)), 'exit': p.returncode,
                'stdout_sha256': hashlib.sha256(p.stdout).hexdigest(),
                'stderr_tail': p.stderr.decode(errors='replace')[-3000:]})
            save()
            assert p.returncode == 0, p.stderr.decode(errors='replace')[-3000:]
            return p.stdout
        def probe(tag, executable):
            row = {'direction': tag, 'executable': str(executable),
                   'executable_sha256': sha(executable), 'commands': []}
            record['directions'].append(row)
            loaded = run(['ldd', executable], row).decode()
            for path in libraries:
                assert str(path.parent) in loaded, 'shared library was not loaded from snapshot: '+str(path)
            row['ldd'] = loaded
            if name == 'bzip2':
                packed = work/(tag+'.bz2')
                packed.write_bytes(run([executable, '-c', payload], row))
                assert run([executable, '-dc', packed], row) == payload.read_bytes()
            elif name == 'libpng':
                run([executable, project/'source/pngtest.png', work/(tag+'.png')], row)
            elif name == 'libarchive':
                archive = work/(tag+'.tar.gz')
                run([executable, '-czf', archive, 'payload'], row)
                assert b'payload' in run([executable, '-tf', archive], row)
                assert run([executable, '-xOf', archive, 'payload'], row) == payload.read_bytes()
            elif name == 'curl':
                class Quiet(http.server.SimpleHTTPRequestHandler):
                    def log_message(self, *args):
                        pass
                server = http.server.ThreadingHTTPServer(('127.0.0.1', 0),
                    functools.partial(Quiet, directory=str(work)))
                thread = threading.Thread(target=server.serve_forever, daemon=True)
                thread.start()
                try:
                    url = 'http://127.0.0.1:'+str(server.server_port)+'/payload'
                    assert run([executable, '--noproxy', '*', '--fail', '--silent', url], row) == payload.read_bytes()
                finally:
                    server.shutdown()
                    server.server_close()
                    thread.join()
            row['passed'] = True
            save()
        executable = project/({'bzip2': 'source/bzip2-shared', 'libpng': 'build/pngtest',
                               'libarchive': 'build/bin/bsdtar', 'curl': 'build/src/curl'}[name])
        try:
            for path, _, fas in backups:
                shutil.copy2(fas, path)
            print('CROSS', name, 'GNU consumer / Fas library', flush=True)
            probe('gnu-consumer-fas-library', executable)
        finally:
            for path, gnu, _ in backups:
                shutil.copy2(gnu, path)
                assert sha(path) == sha(gnu), 'GNU library restore failed'
        print('CROSS', name, 'Fas consumer / GNU library', flush=True)
        probe('fas-consumer-gnu-library', project/'fas-outputs'/executable.relative_to(project))
        record['passed'] = True
        save()
    save()


if __name__ == '__main__':
    main()
