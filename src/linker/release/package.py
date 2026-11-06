#!/usr/bin/env python3
"""Snapshot, package and inspect qualified fas-ld development release archives."""
import argparse
import gzip
import hashlib
import io
import json
import os
from pathlib import Path, PurePosixPath
import re
import shutil
import stat
import struct
import subprocess
import tarfile

ROOT = Path(__file__).resolve().parents[3]
RESERVED = '.fas-release'


def digest(data):
    return hashlib.sha256(data).hexdigest()


def encode(value):
    return (json.dumps(value, indent=2, sort_keys=True)+'\n').encode()


def safe_name(name):
    path = PurePosixPath(name)
    if (not name or '\\' in name or path.is_absolute() or
            any(part in ('', '.', '..') for part in name.split('/'))):
        raise ValueError('unsafe archive/source path: '+repr(name))
    return path


def read_source(root, name):
    path = safe_name(name)
    current = root
    for part in path.parts:
        current = current/part
        if current.is_symlink():
            raise ValueError('source symlink is not supported: '+name)
    info = current.stat()
    if not stat.S_ISREG(info.st_mode):
        raise ValueError('source is not a regular file: '+name)
    return current.read_bytes(), 0o755 if info.st_mode & 0o111 else 0o644


def snapshot(root, original=None):
    if original:
        previous = json.loads(original.read_bytes())
        if not isinstance(previous, dict):
            raise ValueError('source manifest must be a JSON object')
        names = previous['files'] if previous.get('schema') == 1 else previous
    else:
        names = sorted(set(os.fsdecode(x) for x in subprocess.check_output(
            ['git', 'ls-files', '-z', '--cached', '--others', '--exclude-standard'],
            cwd=root).split(b'\0') if x))
    files = {}
    for name in sorted(names):
        if safe_name(name).parts[0] == RESERVED:
            raise ValueError('reserved source namespace: '+name)
        if not original and not (root/name).exists() and not (root/name).is_symlink():
            continue  # Tracked files removed in this working tree.
        data, mode = read_source(root, name)
        row = {'sha256': digest(data), 'mode': mode}
        if original:
            expected = names[name]
            if isinstance(expected, str):
                if row['sha256'] != expected:
                    raise ValueError('source changed: '+name)
            elif row != expected:
                raise ValueError('source changed: '+name)
        files[name] = row
    if not files:
        raise ValueError('empty source snapshot')
    return {'schema': 1, 'files': files}


def validate_source(root, manifest):
    if (not isinstance(manifest, dict) or manifest.get('schema') != 1 or
            not isinstance(manifest.get('files'), dict) or not manifest['files']):
        raise ValueError('unsupported or empty source manifest')
    for name, expected in manifest['files'].items():
        if safe_name(name).parts[0] == RESERVED:
            raise ValueError('reserved source namespace: '+name)
        data, mode = read_source(root, name)
        if {'sha256': digest(data), 'mode': mode} != expected:
            raise ValueError('source changed: '+name)


def archive(path, prefix, kind, files, epoch):
    inventory = {'schema': 1, 'kind': kind, 'prefix': prefix,
                 'files': {name: {'sha256': digest(data), 'mode': mode, 'size': len(data)}
                           for name, (data, mode) in sorted(files.items())}}
    members = dict(files)
    members[RESERVED+'/archive.json'] = (encode(inventory), 0o644)
    with path.open('xb') as raw:
        with gzip.GzipFile(filename='', mode='wb', fileobj=raw, mtime=epoch, compresslevel=9) as zipped:
            with tarfile.open(fileobj=zipped, mode='w', format=tarfile.PAX_FORMAT) as tar:
                for name, (data, mode) in sorted(members.items()):
                    safe_name(name)
                    info = tarfile.TarInfo(prefix+'/'+name)
                    info.size, info.mode, info.mtime = len(data), mode, epoch
                    tar.addfile(info, io.BytesIO(data))


def verify(path):
    # Inspection only: never extract or run anything supplied by an archive.
    entries = {}
    prefix = None
    with tarfile.open(path, 'r:gz') as tar:
        for member in tar:
            parts = safe_name(member.name).parts
            if len(parts) < 2 or not member.isfile():
                raise ValueError('only regular files below one archive root are allowed')
            if prefix is None:
                prefix = parts[0]
            if parts[0] != prefix or member.name in entries:
                raise ValueError('multiple roots or duplicate archive entry')
            if member.mode not in (0o644, 0o755) or member.uid or member.gid:
                raise ValueError('unexpected archive permissions/ownership')
            data = tar.extractfile(member).read()
            entries[member.name] = (digest(data), member.mode, len(data))
            if parts[1:] == (RESERVED, 'archive.json'):
                inventory = json.loads(data)
    manifest_name = str(prefix)+'/'+RESERVED+'/archive.json'
    if manifest_name not in entries:
        raise ValueError('missing archive inventory')
    if (not isinstance(inventory, dict) or inventory.get('schema') != 1 or
            inventory.get('prefix') != prefix or inventory.get('kind') not in ('binary', 'source') or
            not isinstance(inventory.get('files'), dict)):
        raise ValueError('unsupported archive inventory')
    actual = {name[len(prefix)+1:]: {'sha256': sha, 'mode': mode, 'size': size}
              for name, (sha, mode, size) in entries.items() if name != manifest_name}
    for name in inventory['files']:
        safe_name(name)
    if actual != inventory['files']:
        raise ValueError('archive contents do not match inventory')
    return {'status': 'passed', 'kind': inventory['kind'], 'files': len(actual),
            'sha256': digest(path.read_bytes())}


def command(argv):
    return subprocess.check_output(list(map(str, argv)), text=True,
                                   env={**os.environ, 'LC_ALL': 'C'}, timeout=60)


def build(args):
    root = args.source_root.resolve()
    manifest_bytes = args.manifest.read_bytes()
    manifest = json.loads(manifest_bytes)
    validate_source(root, manifest)
    qualification_bytes = args.qualification.read_bytes()
    qualification = json.loads(qualification_bytes)
    binary = args.linker.resolve()
    data = binary.read_bytes()
    if (len(data) < 64 or data[:6] != b'\x7fELF\x02\x01' or
            struct.unpack_from('<HH', data, 16) not in ((2, 62), (3, 62))):
        raise ValueError('linker must be a Linux x86-64 ELF executable')
    if (not isinstance(qualification, dict) or qualification.get('status') != 'passed' or qualification.get('failures') != [] or
            qualification.get('linker_sha256') != digest(data) or
            qualification.get('installed_linker_sha256') != digest(data) or
            qualification.get('source_manifest_sha256') != digest(manifest_bytes) or
            qualification.get('source_files_unchanged') is not True):
        raise ValueError('qualification does not match this binary and source snapshot')
    version_text = command([binary, '--version']).strip()
    match = re.match(r'^fas-ld ([A-Za-z0-9][A-Za-z0-9.+-]*) ', version_text)
    if not match:
        raise ValueError('unrecognized linker version: '+version_text)
    version = match[1]
    if args.version and args.version != version:
        raise ValueError('requested version differs from executable version')
    help_text = command([binary, '--help'])
    dependencies = command(['readelf', '-W', '-l', '-d', '-V', binary])
    glibc = sorted(set(re.findall(r'Name: (GLIBC_\d+(?:\.\d+)+)', dependencies)),
                   key=lambda x: tuple(map(int, x[6:].split('.'))))
    license_path = args.ocaml_license
    if license_path is None:
        stdlib = Path(command(['ocamlc', '-where']).strip())
        license_path = stdlib.parents[1]/'doc/ocaml/LICENSE'
    ocaml_license = license_path.read_bytes()
    if not ocaml_license.strip():
        raise ValueError('empty OCaml license')
    info = {'schema': 1, 'version': version, 'version_output': version_text,
            'target': 'linux-x86_64', 'epoch': args.epoch,
            'linker_sha256': digest(data), 'source_manifest_sha256': digest(manifest_bytes),
            'qualification_sha256': digest(qualification_bytes),
            'ocaml_license_sha256': digest(ocaml_license),
            'tool_versions': qualification.get('tool_versions', {}),
            'elf_interpreter': re.findall(r'Requesting program interpreter: ([^\]]+)', dependencies),
            'needed': re.findall(r'\(NEEDED\).*?\[([^\]]+)\]', dependencies),
            'required_glibc_versions': glibc,
            'max_required_glibc_version': glibc[-1] if glibc else None,
            'scope': 'shared correctness qualification; complete release acceptance is recorded separately; no reproducible compiler build claim'}
    metadata = {RESERVED+'/build-info.json': (encode(info), 0o644),
                RESERVED+'/source-manifest.json': (manifest_bytes, 0o644),
                RESERVED+'/qualification.json': (qualification_bytes, 0o644)}
    source_files = {}
    for name, expected in manifest['files'].items():
        content, mode = read_source(root, name)
        if digest(content) != expected['sha256'] or mode != expected['mode']:
            raise ValueError('source changed during packaging: '+name)
        source_files[name] = (content, mode)
    if 'LICENSE' not in source_files:
        raise ValueError('source snapshot must contain the project LICENSE')
    binary_files = {**metadata, 'bin/fas-ld': (data, 0o755),
                    'LICENSE': source_files['LICENSE'],
                    'licenses/OCaml-LICENSE': (ocaml_license, 0o644),
                    'README.md': ((Path(__file__).with_name('BINARY.md')).read_bytes(), 0o644),
                    'help.txt': (help_text.encode(), 0o644)}
    source_files.update(metadata)
    prefix = 'fas-ld-'+version+'-linux-x86_64'
    # Nothing is overwritten, including partial artifacts from earlier attempts.
    args.output.mkdir(parents=True, exist_ok=False)
    try:
        archives = []
        for suffix, kind, files in (('', 'binary', binary_files), ('-source', 'source', source_files)):
            path = args.output/(prefix+suffix+'.tar.gz')
            archive(path, prefix+suffix, kind, files, args.epoch)
            verify(path)
            archives.append(path)
        (args.output/'SHA256SUMS').write_text(''.join(
            digest(path.read_bytes())+'  '+path.name+'\n' for path in archives))
    except BaseException:
        shutil.rmtree(args.output)
        raise
    print(json.dumps(info, indent=2))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest='operation', required=True)
    take = commands.add_parser('snapshot', help='capture source hashes and executable modes before testing')
    take.add_argument('--source-root', type=Path, default=ROOT)
    take.add_argument('--from-manifest', type=Path)
    take.add_argument('--output', type=Path, required=True)
    pack = commands.add_parser('build', help='package a matching passing qualification')
    pack.add_argument('--source-root', type=Path, default=ROOT)
    pack.add_argument('--manifest', type=Path, required=True)
    pack.add_argument('--qualification', type=Path, required=True)
    pack.add_argument('--linker', type=Path, required=True)
    pack.add_argument('--ocaml-license', type=Path)
    pack.add_argument('--version', help='must equal the version already embedded in the executable')
    pack.add_argument('--epoch', type=int, default=os.environ.get('SOURCE_DATE_EPOCH', '0'))
    pack.add_argument('--output', type=Path, required=True)
    check = commands.add_parser('verify', help='inspect archive integrity without extracting or executing')
    check.add_argument('archive', type=Path)
    args = parser.parse_args()
    try:
        if args.operation == 'snapshot':
            payload = encode(snapshot(args.source_root.resolve(), args.from_manifest))
            with args.output.open('xb') as target:
                target.write(payload)
        elif args.operation == 'build':
            if not 0 <= args.epoch <= 0xffffffff:
                raise ValueError('--epoch must fit the gzip timestamp (0..4294967295)')
            build(args)
        else:
            print(json.dumps(verify(args.archive), indent=2))
    except (OSError, ValueError, KeyError, TypeError, EOFError, tarfile.TarError,
            subprocess.SubprocessError) as error:
        parser.exit(1, 'fas-ld package: '+str(error)+'\n')


if __name__ == '__main__':
    main()
