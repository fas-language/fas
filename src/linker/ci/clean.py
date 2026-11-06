#!/usr/bin/env python3
"""Run the CI correctness recipe in a fresh Docker container and source copy."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import time
import uuid

ROOT = Path(__file__).resolve().parents[3]


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--image', default='fas-linker-qualification:ubuntu24-ocaml5.4.1')
    parser.add_argument('--no-build', action='store_true', help='use an already built image')
    parser.add_argument('--extraquick', action='store_true', help='SQLite weekly selection')
    parser.add_argument('--timeout', type=int, default=7200)
    args = parser.parse_args()
    if args.timeout <= 0:
        parser.error('--timeout must be positive')
    output = args.output.resolve()
    if output.is_relative_to(ROOT):
        parser.error('--output must be outside the repository')
    output.mkdir(parents=True, exist_ok=False)
    report = {'started': time.strftime('%Y-%m-%dT%H:%M:%S%z'), 'status': 'running',
              'scope': 'fresh Ubuntu userspace container, shared host kernel; local CI recipe, not hosted CI',
              'git_head': subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=ROOT, text=True).strip(),
              'dockerfile_sha256': sha(ROOT/'src/linker/ci/Dockerfile'),
              'recipe_sha256': sha(ROOT/'src/linker/ci/correctness.sh'),
              'runner_sha256': sha(Path(__file__)), 'extraquick': args.extraquick,
              'commands': [], 'timing_comparison': False}
    def save():
        (output/'report.json').write_text(json.dumps(report, indent=2)+'\n')
    def run(argv, name, timeout):
        row = {'argv': list(map(str, argv)), 'log': name}
        report['commands'].append(row)
        save()
        try:
            with (output/name).open('wb') as log:
                p = subprocess.run(row['argv'], cwd=ROOT, stdout=log,
                                   stderr=subprocess.STDOUT, timeout=timeout)
            row['exit'] = p.returncode
            if p.returncode:
                raise RuntimeError(name+': exit '+str(p.returncode))
        finally:
            row['log_sha256'] = sha(output/name)
            save()
    name = 'fas-linker-clean-'+uuid.uuid4().hex[:12]
    created = False
    try:
        if not args.no_build:
            run(['docker', 'build', '--progress=plain', '-t', args.image, '-f',
                 ROOT/'src/linker/ci/Dockerfile', ROOT/'src/linker/ci'], 'image-build.log', args.timeout)
        image = json.loads(subprocess.check_output(['docker', 'image', 'inspect', args.image], text=True))[0]
        report['image'] = {'requested': args.image, 'id': image['Id'],
                           'repo_digests': image.get('RepoDigests', [])}
        # Include uncommitted changes, but no host build products, .git or ignored
        # credentials. A complete manifest makes that precise snapshot reviewable.
        names = subprocess.check_output(['git', 'ls-files', '-z', '--cached', '--others',
                                         '--exclude-standard'], cwd=ROOT).split(b'\0')
        source = output/'source'
        source.mkdir()
        manifest = {}
        for raw in sorted(set(names)):
            if not raw:
                continue
            relative = Path(os.fsdecode(raw))
            path = ROOT/relative
            if not path.exists():
                continue
            if path.is_symlink():
                raise ValueError('source snapshot requires regular tracked files: '+str(relative))
            target = source/relative
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(path, target)
            manifest[str(relative)] = sha(target)
        (output/'source-manifest.json').write_text(json.dumps(manifest, indent=2)+'\n')
        report['source_file_count'] = len(manifest)
        report['source_manifest_sha256'] = sha(output/'source-manifest.json')
        for directory in ('home', 'results'):
            (output/directory).mkdir()
        # No host source, toolchain or home mounts. Ptrace is for GDB regressions.
        command = ['docker', 'create', '--name', name, '--user', f'{os.getuid()}:{os.getgid()}',
                   '--ulimit', 'nofile=65536:65536',
                   '--cap-add', 'SYS_PTRACE', '--mount', f'type=bind,src={output},dst=/qualification',
                   '--env', 'HOME=/qualification/home', '--env', 'RUNNER_TEMP=/qualification/results',
                   '--env', 'LC_ALL=C.UTF-8', '--env', 'FAS_SQLITE_EXTRAQUICK='+str(int(args.extraquick)),
                   '--workdir', '/qualification/source', report['image']['id'], 'bash', '-c',
                   'set -e; cat /etc/os-release; uname -a; '
                   'ocamlopt -version; dune --version; gcc --version; clang --version; '
                   'bash src/linker/ci/correctness.sh']
        run(command, 'container-create.log', 60)
        created = True
        report['container_name'] = name
        run(['docker', 'start', '--attach', name], 'qualification.log', args.timeout)
        inspect = json.loads(subprocess.check_output(['docker', 'inspect', name], text=True))[0]
        report['container_exit'] = inspect['State']['ExitCode']
        if report['container_exit']:
            raise RuntimeError('container exit '+str(report['container_exit']))
        summary = output/'results/qualification-summary.json'
        report['qualification_summary'] = json.loads(summary.read_text())
        report['qualification_summary_sha256'] = sha(summary)
        if report['qualification_summary']['status'] != 'passed':
            raise RuntimeError('correctness selection verification failed')
        changed = [name for name, expected in manifest.items() if sha(source/name) != expected]
        report['source_changes'] = changed
        if changed:
            raise RuntimeError('source snapshot changed during qualification')
        report['status'] = 'passed'
    except BaseException as error:
        report['status'] = 'failed'
        report['error'] = str(error)
        raise
    finally:
        if created:
            # Stop only this invocation's container; leave its filesystem/logs
            # available for diagnosis. Removing it is a separate user action.
            try:
                cleanup = subprocess.run(['docker', 'stop', '--time', '5', name], capture_output=True, timeout=30)
                if cleanup.returncode:
                    report['cleanup_error'] = cleanup.stderr.decode(errors='replace')
            except (OSError, subprocess.TimeoutExpired) as error:
                report['cleanup_error'] = str(error)
        report['finished'] = time.strftime('%Y-%m-%dT%H:%M:%S%z')
        save()
        print(report['status'].upper(), output, flush=True)


if __name__ == '__main__':
    main()
