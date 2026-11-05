#!/usr/bin/env python3
"""Record additional C/release audit probes, including known failing cases.

Observations, not a passing acceptance suite. Requires the documented Debian-style
glibc x86-64 sysroot, GCC, Clang and GNU tools. LLD is compared when available.
The output directory must be new; inputs and the Fas executable are snapshotted.
Exit zero means the probes completed, not that all observed behavior was correct.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import platform
import shutil
import subprocess


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('linker', type=Path)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    root = args.output.resolve()
    root.mkdir(parents=True, exist_ok=False)
    fas = root / 'fas-ld'
    shutil.copy2(args.linker.resolve(), fas)
    clang = shutil.which('clang-22') or shutil.which('clang')
    lld = shutil.which('ld.lld-22') or shutil.which('ld.lld')
    tools = [('fas', str(fas)), ('gnu', shutil.which('ld'))]
    if lld:
        tools.append(('lld', lld))
    if not clang or not tools[1][1]:
        raise RuntimeError('Clang and GNU ld are required')

    def run(command, restrictive_mask=False):
        command = [str(a) for a in command]
        process = subprocess.run(command, capture_output=True, timeout=20,
                                 preexec_fn=(lambda: os.umask(0o077)) if restrictive_mask else None)
        return dict(command=command, status=process.returncode,
                    stdout=process.stdout.decode(errors='replace'),
                    stderr=process.stderr.decode(errors='replace'))

    def checked(command):
        result = run(command)
        if result['status']:
            raise RuntimeError(result)
        return result

    def crt(name):
        return checked(['gcc', '-print-file-name=' + name])['stdout'].strip()

    report = dict(linker_sha256=hashlib.sha256(fas.read_bytes()).hexdigest(),
                  platform=platform.platform(),
                  compiler=checked([clang, '--version'])['stdout'].splitlines()[0],
                  gcc=checked(['gcc', '--version'])['stdout'].splitlines()[0],
                  tool_versions={label: checked([tool, '--version'])['stdout'].splitlines()[0]
                                 for label, tool in tools},
                  cases=[], predefined_exports=[], permissions=[])
    constructor = '''static volatile int initialized;
__attribute__((constructor)) static void initialize(void) { initialized = 42; }
int main(void) { return initialized == 42 ? 0 : 7; }
'''
    sources = {
        'modern-ctors': constructor,
        'legacy-ctors': constructor,
        'legacy-dtors': '''#include <stdio.h>
__attribute__((destructor)) static void finalize(void) { puts("done"); }
int main(void) { puts("main"); return 0; }
''',
        'section-boundaries': '''const int record __attribute__((section("registry"),used,retain)) = 42;
extern const int __start_registry[], __stop_registry[];
int main(void) { return __stop_registry-__start_registry == 1 && *__start_registry == 42 ? 0 : 8; }
''',
    }
    for name, source in sources.items():
        path = root / (name + '.c')
        path.write_text(source)
        obj = path.with_suffix('.o')
        flags = ['-fno-use-init-array'] if name.startswith('legacy-') else []
        compiled = checked([clang, '-c', '-O2', '-fPIC', *flags, path, '-o', obj])
        for gc in (False, True):
            for label, tool in tools:
                out = root / (name + '-' + label + ('-gc' if gc else ''))
                command = [tool, '-pie', '--dynamic-linker', '/lib64/ld-linux-x86-64.so.2',
                           *(['--gc-sections'] if gc else []), '-o', out,
                           crt('Scrt1.o'), crt('crti.o'), crt('crtbeginS.o'), obj,
                           '/lib/x86_64-linux-gnu/libc.so.6', crt('libc_nonshared.a'),
                           crt('crtendS.o'), crt('crtn.o')]
                linked = run(command)
                executed = run([out]) if linked['status'] == 0 else None
                report['cases'].append(dict(case=name, gc=gc, tool=label,
                                            compile=compiled, link=linked, execute=executed))

    lib_source = root / 'predefined-library.c'
    lib_source.write_text('extern char _end[]; char *get_end(void) { return _end; }\n')
    lib_obj = lib_source.with_suffix('.o')
    checked([clang, '-c', '-fPIC', lib_source, '-o', lib_obj])
    host_source = root / 'predefined-host.c'
    host_source.write_text('''#include <dlfcn.h>
#include <stdio.h>
int main(int argc, char **argv) {
  if (argc != 2) return 4;
  void *h = dlopen(argv[1], RTLD_NOW | RTLD_LOCAL);
  if (!h) { puts(dlerror()); return 1; }
  char *(*get_end)(void) = (char *(*)(void))dlsym(h, "get_end");
  void *end = dlsym(h, "_end");
  if (!get_end || !end) { puts("missing export"); return 2; }
  if (end != get_end()) { puts("export differs from internal address"); return 3; }
  puts("ok"); return 0;
}
''')
    host = host_source.with_suffix('')
    checked(['gcc', host_source, '-ldl', '-o', host])
    for label, tool in tools:
        library = root / ('predefined-' + label + '.so')
        linked = run([tool, '-shared', '-o', library, lib_obj])
        executed = run([host, library]) if linked['status'] == 0 else None
        report['predefined_exports'].append(dict(tool=label, link=linked, execute=executed,
            symbols=run(['readelf', '--dyn-syms', '-W', library]) if executed else None))

    start = root / 'permissions.s'
    start.write_text('.text\n.globl _start\n_start: xor %edi,%edi\nmov $60,%eax\nsyscall\n'
                     '.section .note.GNU-stack,"",@progbits\n')
    checked(['as', '-o', start.with_suffix('.o'), start])
    for label, tool in tools:
        for mode in ('executable', 'relocatable', 'shared', 'map'):
            out = root / ('permissions-' + label + '-' + mode)
            map_path = root / ('permissions-' + label + '.map')
            flags = ['-r'] if mode == 'relocatable' else ['-shared'] if mode == 'shared' else []
            command = [tool, *flags, *(['-Map', map_path] if mode == 'map' else []),
                       '-o', out, start.with_suffix('.o')]
            result = run(command, restrictive_mask=True)
            if result['status']:
                raise RuntimeError(result)
            report['permissions'].append(dict(tool=label, mode=mode, umask='077', link=result,
                permissions=oct(out.stat().st_mode & 0o777),
                map_permissions=oct(map_path.stat().st_mode & 0o777) if mode == 'map' else None))

    report['input_sha256'] = {path.name: hashlib.sha256(path.read_bytes()).hexdigest()
                             for path in root.iterdir() if path.suffix in ('.c', '.s', '.o')}
    sysroot_files = [crt(name) for name in ('Scrt1.o', 'crti.o', 'crtbeginS.o',
                                           'libc_nonshared.a', 'crtendS.o', 'crtn.o')]
    sysroot_files += ['/lib64/ld-linux-x86-64.so.2', '/lib/x86_64-linux-gnu/libc.so.6']
    report['sysroot_sha256'] = {path: hashlib.sha256(Path(path).read_bytes()).hexdigest()
                               for path in sysroot_files}
    (root / 'report.json').write_text(json.dumps(report, indent=2) + '\n')
    for case in report['cases']:
        executed = case['execute']
        print(case['case'], case['tool'], 'gc=' + str(case['gc']),
              'link=' + str(case['link']['status']),
              'run=' + str(executed['status'] if executed else None),
              'stdout=' + repr(executed['stdout'] if executed else None))
    for case in report['predefined_exports']:
        print('predefined export', case['tool'], case['execute'])
    for case in report['permissions']:
        print('umask', case['tool'], case['mode'], case['permissions'], case['map_permissions'])


if __name__ == '__main__':
    main()
