"""Release tooling tests. Qualification JSON here is deliberately synthetic."""
import hashlib
import io
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tarfile
import tempfile
import unittest

PACKAGE = Path(sys.argv.pop(1)).resolve()
LINKER = Path(sys.argv.pop(1)).resolve()


def sha(data):
    return hashlib.sha256(data).hexdigest()


class ReleaseTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix='fas-release-test-')
        self.addCleanup(self.temp.cleanup)
        self.base = Path(self.temp.name)
        self.source = self.base/'source'
        self.source.mkdir()
        subprocess.run(['git', 'init', '-q', self.source], check=True)
        (self.source/'LICENSE').write_text('Synthetic test license, not a release notice.\n')
        (self.source/'uncommitted.c').write_text('int fixture;\n')
        (self.source/'build.sh').write_text('#!/bin/sh\nexit 0\n')
        (self.source/'build.sh').chmod(0o755)
        self.manifest = self.base/'manifest.json'
        self.run_tool('snapshot', '--source-root', self.source, '--output', self.manifest)
        self.qualified = {'status': 'passed', 'failures': [],
                          'linker_sha256': sha(LINKER.read_bytes()),
                          'installed_linker_sha256': sha(LINKER.read_bytes()),
                          'source_manifest_sha256': sha(self.manifest.read_bytes()),
                          'source_files_unchanged': True,
                          'scope': 'synthetic test fixture, not a real qualification'}
        self.qualification = self.base/'qualification.json'
        self.qualification.write_text(json.dumps(self.qualified))

    def run_tool(self, *args, success=True):
        result = subprocess.run([sys.executable, PACKAGE, *map(str, args)],
                                capture_output=True, text=True)
        if success:
            self.assertEqual(result.returncode, 0, result.stderr)
        else:
            self.assertEqual(result.returncode, 1, result.stderr)
            self.assertIn('fas-ld package:', result.stderr)
            self.assertNotIn('Traceback', result.stderr)
        return result

    def build(self, output, *extra, success=True):
        return self.run_tool('build', '--source-root', self.source,
                             '--manifest', self.manifest, '--qualification', self.qualification,
                             '--linker', LINKER, '--output', output, *extra, success=success)

    def test_reproducible_archives_and_relocated_linker(self):
        first, second = self.base/'one', self.base/'two'
        self.build(first)
        self.build(second)
        self.assertEqual(sorted(x.name for x in first.iterdir()), sorted(x.name for x in second.iterdir()))
        for path in first.iterdir():
            self.assertEqual(path.read_bytes(), (second/path.name).read_bytes())
        archives = sorted(first.glob('*.tar.gz'))
        self.assertEqual(len(archives), 2)
        for path in archives:
            self.run_tool('verify', path)
            self.assertEqual(path.read_bytes()[4:8], b'\0\0\0\0')
        sums = (first/'SHA256SUMS').read_text()
        for path in archives:
            self.assertIn(sha(path.read_bytes())+'  '+path.name, sums)
        binary = next(x for x in archives if not x.name.endswith('-source.tar.gz'))
        with tarfile.open(binary) as tar:
            files = {x.name.split('/', 1)[1]: (x, tar.extractfile(x).read()) for x in tar}
        self.assertEqual(files['bin/fas-ld'][0].mode, 0o755)
        self.assertEqual(files['LICENSE'][1], (self.source/'LICENSE').read_bytes())
        self.assertIn(b'GNU LESSER GENERAL PUBLIC LICENSE', files['licenses/OCaml-LICENSE'][1])
        info = json.loads(files['.fas-release/build-info.json'][1])
        self.assertEqual(info['linker_sha256'], sha(LINKER.read_bytes()))
        self.assertIn('libc.so.6', info['needed'])
        self.assertTrue(info['required_glibc_versions'])
        relocated = self.base/'relocated-fas-ld'
        relocated.write_bytes(files['bin/fas-ld'][1])
        relocated.chmod(0o755)
        asm = self.base/'start.s'
        asm.write_text('.globl _start\n.text\n_start:\n mov $60, %eax\n mov $42, %edi\n syscall\n.section .note.GNU-stack,"",@progbits\n')
        obj, program = self.base/'start.o', self.base/'program'
        subprocess.run(['as', '-o', obj, asm], check=True)
        environment = {**os.environ, 'PATH': '/nonexistent'}
        subprocess.run([relocated, '--version'], env=environment, capture_output=True, check=True)
        subprocess.run([relocated, '-o', program, obj], env=environment, capture_output=True, check=True)
        self.assertEqual(subprocess.run([program], env=environment).returncode, 42)
        source = next(x for x in archives if x.name.endswith('-source.tar.gz'))
        with tarfile.open(source) as tar:
            source_files = {x.name.split('/', 1)[1]: (x, tar.extractfile(x).read()) for x in tar}
        for name in ('LICENSE', 'uncommitted.c', 'build.sh'):
            self.assertEqual(source_files[name][1], (self.source/name).read_bytes())
        self.assertEqual(source_files['build.sh'][0].mode, 0o755)
        self.assertFalse(any('.git/' in name for name in source_files))

    def test_failed_or_mismatched_qualification_cannot_create_artifacts(self):
        for field, bad in [('status', 'failed'), ('failures', ['bad result']),
                           ('linker_sha256', '0'*64), ('installed_linker_sha256', '0'*64),
                           ('source_manifest_sha256', '0'*64), ('source_files_unchanged', False)]:
            with self.subTest(field=field):
                self.qualification.write_text(json.dumps({**self.qualified, field: bad}))
                output = self.base/field
                self.build(output, success=False)
                self.assertFalse(output.exists())
        self.qualification.write_text(json.dumps(self.qualified))
        self.build(self.base/'relabel', '--version', '1.0', success=False)
        self.assertFalse((self.base/'relabel').exists())
        output = self.base/'existing'
        output.mkdir()
        (output/'sentinel').write_text('keep')
        self.build(output, success=False)
        self.assertEqual(list(output.iterdir()), [output/'sentinel'])
        self.assertEqual((output/'sentinel').read_text(), 'keep')

    def test_source_content_modes_and_symlinks_are_checked(self):
        for change in ('content', 'mode', 'symlink'):
            with self.subTest(change=change):
                path = self.source/'uncommitted.c'
                if change == 'content':
                    path.write_text('changed\n')
                elif change == 'mode':
                    path.chmod(0o755)
                else:
                    path.unlink()
                    path.symlink_to(self.source/'LICENSE')
                self.build(self.base/change, success=False)
                self.assertFalse((self.base/change).exists())
                path.unlink()
                path.write_text('int fixture;\n')
        (self.source/'directory').mkdir()
        (self.source/'directory/file').write_text('data')
        (self.source/'link').symlink_to('directory', target_is_directory=True)
        self.run_tool('snapshot', '--source-root', self.source, '--output', self.base/'bad.json', success=False)
        self.assertFalse((self.base/'bad.json').exists())

    def test_archive_verifier_rejects_corruption_and_unsafe_members(self):
        output = self.base/'archives'
        self.build(output)
        binary = next(x for x in output.glob('*.tar.gz') if not x.name.endswith('-source.tar.gz'))
        with tarfile.open(binary) as tar:
            members = [(member, tar.extractfile(member).read()) for member in tar]
        for kind in ('payload', 'extra', 'duplicate', 'traversal', 'symlink', 'missing-inventory', 'bad-inventory'):
            with self.subTest(kind=kind):
                path = self.base/(kind+'.tar.gz')
                with tarfile.open(path, 'w:gz') as tar:
                    for member, data in members:
                        if kind == 'missing-inventory' and member.name.endswith('/archive.json'):
                            continue
                        if kind == 'payload' and member.name.endswith('/bin/fas-ld'):
                            data = b'X'+data[1:]
                        if kind == 'bad-inventory' and member.name.endswith('/archive.json'):
                            data = b'[]'
                            member.size = len(data)
                        tar.addfile(member, io.BytesIO(data))
                    if kind in ('extra', 'duplicate', 'traversal', 'symlink'):
                        name = {'extra': members[0][0].name.split('/')[0]+'/extra',
                                'duplicate': members[0][0].name,
                                'traversal': '../escaped', 'symlink': 'root/link'}[kind]
                        member = tarfile.TarInfo(name)
                        member.mode = 0o644
                        if kind == 'symlink':
                            member.type, member.linkname = tarfile.SYMTYPE, '/tmp/escaped'
                        tar.addfile(member, io.BytesIO(b''))
                self.run_tool('verify', path, success=False)
        self.assertFalse((self.base/'escaped').exists())

    def test_snapshot_matches_legacy_manifest_and_normalizes_executable_modes(self):
        first = json.loads(self.manifest.read_text())
        legacy = self.base/'legacy.json'
        legacy.write_text(json.dumps({name: row['sha256'] for name, row in first['files'].items()}))
        shutil.rmtree(self.source/'.git')
        # Copies without git metadata (the clean runner) use its input manifest.
        (self.source/'LICENSE').chmod(0o600)
        (self.source/'build.sh').chmod(0o700)
        second = self.base/'second.json'
        self.run_tool('snapshot', '--source-root', self.source, '--from-manifest', legacy, '--output', second)
        self.assertEqual(self.manifest.read_bytes(), second.read_bytes())
        (self.source/'LICENSE').write_text('changed')
        self.run_tool('snapshot', '--source-root', self.source, '--from-manifest', legacy,
                      '--output', self.base/'changed.json', success=False)


if __name__ == '__main__':
    unittest.main(verbosity=2)
