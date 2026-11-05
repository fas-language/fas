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

SOURCES = {
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


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('project', choices=SOURCES)
    parser.add_argument('--output', type=Path, required=True, help='new directory outside source repository')
    parser.add_argument('--build-musl', action='store_true')
    args = parser.parse_args()
    root = args.output.resolve()
    if root.is_relative_to(Path(__file__).resolve().parents[3]):
        parser.error('output must be outside the source repository')
    if args.build_musl and args.project != 'musl':
        parser.error('--build-musl requires project musl')
    root.mkdir(parents=True, exist_ok=False)
    url, checksum, directory = SOURCES[args.project]
    archive = root/'source.tar.gz'
    with urllib.request.urlopen(url, timeout=60) as response, archive.open('wb') as out:
        total = 0
        while chunk := response.read(1024 * 1024):
            total += len(chunk)
            if total > 32 * 1024 * 1024:
                raise ValueError('source archive exceeds 32 MiB')
            out.write(chunk)
    if hashlib.sha256(archive.read_bytes()).hexdigest() != checksum:
        raise ValueError('source archive checksum mismatch')
    with tarfile.open(archive) as source:
        source.extractall(root, filter='data')
    report = {'release': directory, 'source_url': url, 'sha256': checksum,
              'source': str(root/directory), 'commands': []}
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
