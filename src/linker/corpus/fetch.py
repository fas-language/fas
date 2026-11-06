#!/usr/bin/env python3
"""Download checksum-pinned corpus sources; optional isolated musl installation."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess
import tarfile
import urllib.request
import zipfile

SOURCES = {
    'lua': ('https://www.lua.org/ftp/lua-5.1.5.tar.gz',
               '2640fc56a795f29d28ef15e13c34a47e223960b0240e8cb0a82d9b0738695333',
               'lua-5.1.5'),
    'bzip2': ('https://sourceware.org/pub/bzip2/bzip2-1.0.8.tar.gz',
               'ab5a03176ee106d3f0fa90e381da478ddae405918153cca248e682cd0c4a2269',
               'bzip2-1.0.8'),
    'libpng': ('https://codeload.github.com/pnggroup/libpng/tar.gz/refs/tags/v1.6.47',
               '631a4c58ea6c10c81f160c4b21fa8495b715d251698ebc2552077e8450f30454',
               'libpng-1.6.47'),
    'libarchive': ('https://codeload.github.com/libarchive/libarchive/tar.gz/refs/tags/v3.8.11',
               '6e9560b2e0659ff688c5efb7717a68bb06b28ef65a69addcd6c424eaf9c319b1',
               'libarchive-3.8.11'),
    'curl': ('https://curl.se/download/curl-8.14.1.tar.xz',
               'f4619a1e2474c4bbfedc88a7c2191209c8334b48fa1f4e53fd584cc12e9120dd',
               'curl-8.14.1'),
    'redis': ('https://codeload.github.com/redis/redis/tar.gz/refs/tags/7.2.8',
               '66edcb676acf0f611e1cd8d69aa294c0256cb1658094c73bad529d1546eb89ac',
               'redis-7.2.8'),
    'sqlite': ('https://www.sqlite.org/2026/sqlite-src-3540100.zip',
               '387e7f6cad4797b17848c07d453dc6606917bb356a6cfe5a3abafd38c240c71b',
               'sqlite-src-3540100'),
    'chibicc': ('https://codeload.github.com/rui314/chibicc/tar.gz/90d1f7f199cc55b13c7fdb5839d1409806633fdb',
                'cadb44e82354002000f6d2bbf41d3155f47a9b3e9abd50a309fdcd9b5ef791e0',
                'chibicc-90d1f7f199cc55b13c7fdb5839d1409806633fdb'),
    'zlib': ('https://zlib.net/fossils/zlib-1.3.1.tar.gz',
             '9a93b2b7dfdac77ceba5a558a580e74667dd6fede4585b91eefb60f03b72df23',
             'zlib-1.3.1'),
    'musl': ('https://musl.libc.org/releases/musl-1.2.5.tar.gz',
             'a9a118bbe84d8764da0ea0d28b3ab3fae8477fc7e4085d90102b8596fc7c75e4',
             'musl-1.2.5'),
    'cjson': ('https://codeload.github.com/DaveGamble/cJSON/tar.gz/refs/tags/v1.7.19',
              '7fa616e3046edfa7a28a32d5f9eacfd23f92900fe1f8ccd988c1662f30454562',
              'cJSON-1.7.19'),
}

GCC_COMMIT = '5115c7e447fc07457443df874bf57840e8316d5f'


def fetch_gcc(root):
    source = root/'gcc-15.2.0'
    url = 'https://github.com/gcc-mirror/gcc.git'
    commands = [
        ['git', 'init', source],
        ['git', '-C', source, 'remote', 'add', 'origin', url],
        ['git', '-C', source, 'fetch', '--filter=blob:none', '--depth=1', 'origin', GCC_COMMIT],
        ['git', '-C', source, 'sparse-checkout', 'init', '--cone'],
        ['git', '-C', source, 'sparse-checkout', 'set', 'gcc/testsuite/gcc.c-torture',
         'gcc/testsuite/lib', 'gcc/testsuite/config', 'gcc/testsuite/gcc.dg/tree-ssa'],
        ['git', '-C', source, 'checkout', '--detach', 'FETCH_HEAD'],
    ]
    with (root/'fetch.log').open('wb') as log:
        for command in commands:
            subprocess.run(command, stdout=log, stderr=subprocess.STDOUT, check=True)
    observed = subprocess.check_output(['git', '-C', source, 'rev-parse', 'HEAD'], text=True).strip()
    if observed != GCC_COMMIT:
        raise ValueError('GCC source commit mismatch')
    (root/'source.json').write_text(json.dumps({'release': 'gcc-15.2.0',
        'source_url': url, 'commit': observed, 'source': str(source),
        'commands': [list(map(str, command)) for command in commands]}, indent=2)+'\n')
    print(source)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('project', choices=(*SOURCES, 'gcc-torture'))
    parser.add_argument('--output', type=Path, required=True, help='new directory outside source repository')
    parser.add_argument('--build-musl', action='store_true')
    args = parser.parse_args()
    root = args.output.resolve()
    if root.is_relative_to(Path(__file__).resolve().parents[3]):
        parser.error('output must be outside the source repository')
    if args.build_musl and args.project != 'musl':
        parser.error('--build-musl requires project musl')
    root.mkdir(parents=True, exist_ok=False)
    if args.project == 'gcc-torture':
        fetch_gcc(root)
        return
    url, checksum, directory = SOURCES[args.project]
    archive = root/('source.zip' if url.endswith('.zip') else 'source.tar.gz')
    with urllib.request.urlopen(url, timeout=60) as response, archive.open('wb') as out:
        total = 0
        while chunk := response.read(1024 * 1024):
            total += len(chunk)
            if total > 32 * 1024 * 1024:
                raise ValueError('source archive exceeds 32 MiB')
            out.write(chunk)
    if hashlib.sha256(archive.read_bytes()).hexdigest() != checksum:
        raise ValueError('source archive checksum mismatch')
    if zipfile.is_zipfile(archive):
        with zipfile.ZipFile(archive) as source:
            for entry in source.infolist():
                target = (root/entry.filename).resolve()
                if not target.is_relative_to(root):
                    raise ValueError('archive member escapes output directory')
            source.extractall(root)
            for entry in source.infolist():
                target = root/entry.filename
                if target.is_file():
                    target.chmod((entry.external_attr >> 16) & 0o777 or 0o644)
    else:
        with tarfile.open(archive) as source:
            source.extractall(root, filter='data')
    report = {'release': directory, 'source_url': url, 'sha256': checksum,
              'source': str(root/directory), 'commands': []}
    if args.project == 'sqlite':
        # Independently verified against SQLite's published release checksum.
        expected = '877c6f4996ce7adb368686bce21638de9e3d91534832ad3d9d119037d1bc22ad'
        if hashlib.sha3_256(archive.read_bytes()).hexdigest() != expected:
            raise ValueError('SQLite release SHA3-256 mismatch')
        report['sha3_256'] = expected
    if args.build_musl:
        prefix = root/'sysroot'
        commands = [['./configure', '--prefix='+str(prefix),
                     '--syslibdir='+str(prefix/'lib'), '--enable-debug'],
                    ['nice', '-n', '10', 'make', '-j2'], ['make', 'install']]
        with (root/'build.log').open('wb') as log:
            for command in commands:
                report['commands'].append(command)
                subprocess.run(command, cwd=root/directory, env=dict(os.environ, CC='gcc'),
                               stdout=log, stderr=subprocess.STDOUT, check=True)
        report['prefix'] = str(prefix)
    (root/'source.json').write_text(json.dumps(report, indent=2)+'\n')
    print(root/directory)


if __name__ == '__main__':
    main()
