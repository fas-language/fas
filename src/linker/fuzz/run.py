#!/usr/bin/env python3
"""Build an instrumented linker and preserve a bounded AFL++ campaign."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import platform
import shutil
import subprocess
import tempfile
import time
import struct
import zlib

ROOT = Path(__file__).resolve().parents[3]
MODES = ('elf', 'shared', 'archive', 'script', 'link', 'version', 'shared-link', 'symver-link', 'zlib', 'metadata-link', 'partial-link', 'dependency', 'static-pie-link', 'copy-link')


def run(*args, **kwargs):
    return subprocess.run([str(arg) for arg in args], check=True, **kwargs)


def seeds(output):
    for mode in MODES:
        (output / 'seeds' / mode).mkdir(parents=True)
    with tempfile.TemporaryDirectory(prefix='fas-fuzz-seeds-') as temp:
        temp = Path(temp)
        asm = temp / 'start.s'
        asm.write_text('''.text
.globl _start
_start: mov $60,%eax; xor %edi,%edi; syscall
.section .rodata.constants,"aM",@progbits,8
.quad 42
.section .bss,"aw",@nobits
.zero 32
.section .note.GNU-stack,"",@progbits
''')
        obj = temp / 'start.o'
        run('as', '--64', asm, '-o', obj)
        source = temp / 'metadata.c'
        source.write_text('''__thread int local_tls=7;
int datum=35; int *pointer=&datum;
int answer(void){return datum+local_tls;}
__attribute__((constructor(101))) static void init(void){datum=35;}
''')
        metadata = temp / 'metadata.o'
        run('gcc', '-c', '-g', '-O1', '-fPIC', source, '-o', metadata)
        compressed = temp / 'compressed.o'
        run('gcc', '-c', '-g', '-gz=zlib', '-O1', '-fPIC', source, '-o', compressed)
        static_tls = temp / 'static-tls.o'
        run('gcc', '-c', '-g', '-O1', '-fPIC', '-ftls-model=initial-exec', source, '-o', static_tls)
        shutil.copyfile(static_tls, output / 'seeds' / 'link' / static_tls.name)
        descriptor = temp / 'tlsdesc.o'
        run('gcc', '-c', '-g', '-O1', '-fPIC', '-mtls-dialect=gnu2', source, '-o', descriptor)
        ifunc_source = temp / 'ifunc.c'
        ifunc_source.write_text('''static int implementation(void){return 42;}
static int (*volatile selected)(void)=implementation;
static void *resolve(void){return selected;}
static int local(void) __attribute__((ifunc("resolve")));
int answer(void) __attribute__((ifunc("resolve")));
static int (*volatile pointer)(void)=local;
int check(void){return pointer==local ? answer() : local();}
void _start(void){check();}
''')
        ifunc = temp / 'ifunc.o'
        run('gcc', '-c', '-g', '-O1', '-fPIC', ifunc_source, '-o', ifunc)
        shutil.copyfile(ifunc, output / 'seeds' / 'link' / ifunc.name)
        alias_source = temp / 'aliases.c'
        alias_source.write_text('''int old_impl(void){return 35;} int new_impl(void){return 42;}
__asm__(".symver old_impl,answer@FAS_1,remove");
__asm__(".symver new_impl,answer@@FAS_2,remove");
int answer(void); int check(void){return answer();}
''')
        aliases = temp / 'aliases.o'
        run('gcc', '-c', '-g', '-O1', '-fPIC', alias_source, '-o', aliases)
        unwind_source = temp / 'personality.s'
        unwind_source.write_text('''.text
.globl _start
_start:
.cfi_startproc
.cfi_personality 0x9b, personality_pointer
.cfi_lsda 0x1b, exception_table
mov $60,%eax; xor %edi,%edi; syscall
.cfi_endproc
.hidden personality
.type personality,@function
personality: xor %eax,%eax; ret
.section .data.rel.ro,"aw",@progbits
.p2align 3
personality_pointer: .quad personality
.section .gcc_except_table,"a",@progbits
exception_table: .byte 0xff,0xff,1,0
.section .note.GNU-stack,"",@progbits
''')
        unwind = temp / 'personality.o'
        run('as', '--64', unwind_source, '-o', unwind)
        shutil.copyfile(unwind, output / 'seeds' / 'link' / unwind.name)
        sdt_source = temp / 'sdt.s'
        sdt_source.write_text('''.text
.globl _start
_start: mov $60,%eax; xor %edi,%edi; syscall
.section .stapsdt.base,"aG",@progbits,.stapsdt.base,comdat
.weak _.stapsdt.base
.hidden _.stapsdt.base
_.stapsdt.base: .byte 0
.section .note.stapsdt,"",@note
.p2align 2
.long 8,3f-2f,3
.asciz "stapsdt"
2: .quad _start,_.stapsdt.base,0
.asciz "fas"
.asciz "start"
.asciz ""
3: .balign 4
.section .note.gnu.property,"a",@note
.p2align 3
.long 4,16,5
.asciz "GNU"
.long 0xc0000002,4,3,0
.long 4,16,5
.asciz "GNU"
.long 0xc0000002,4,3,0
.section .note.GNU-stack,"",@progbits
''')
        sdt=temp/'sdt.o'
        run('as','--64',sdt_source,'-o',sdt)
        shutil.copyfile(sdt, output / 'seeds' / 'link' / sdt.name)
        for seed in (obj, metadata, compressed, static_tls, descriptor, ifunc, aliases, unwind, sdt):
            shutil.copyfile(seed, output / 'seeds' / 'elf' / seed.name)
            shutil.copyfile(seed, output / 'seeds' / 'shared-link' / seed.name)
            shutil.copyfile(seed, output / 'seeds' / 'symver-link' / seed.name)
            shutil.copyfile(seed, output / 'seeds' / 'metadata-link' / seed.name)
            shutil.copyfile(seed, output / 'seeds' / 'partial-link' / seed.name)
            shutil.copyfile(seed, output / 'seeds' / 'static-pie-link' / seed.name)
        bootstrap_source = temp / 'bootstrap.s'
        bootstrap_source.write_text('''.text
.globl _start
_start:
mov answer@GOTPCREL(%rip),%r13
call *answer@GOTPCREL(%rip)
mov $60,%eax; xor %edi,%edi; syscall
.type answer,@function
answer: mov $42,%eax; ret
.data
.quad answer
.section .note.GNU-stack,"",@progbits
''')
        bootstrap = temp / 'bootstrap.o'
        run('as', '--64', bootstrap_source, '-o', bootstrap)
        shutil.copyfile(bootstrap, output / 'seeds' / 'static-pie-link' / bootstrap.name)
        shutil.copyfile(obj, output / 'seeds' / 'link' / obj.name)
        for style in ('sysv', 'gnu', 'both'):
            library = output / 'seeds' / 'shared' / ('lib-' + style)
            run('gcc', '-shared', '-nostdlib', '-Wl,--hash-style=' + style,
                '-Wl,-soname,libfuzz.so', metadata, '-o', library)
        versions = temp / 'versions.map'
        versions.write_text('FAS_1 { global: answer; datum; local_tls; local: *; };')
        run('gcc', '-shared', '-nostdlib', '-Wl,--version-script=' + str(versions),
            '-Wl,-soname,libfuzz.so', metadata, '-o',
            output / 'seeds' / 'shared' / 'lib-versioned')
        importing = temp / 'importing.c'
        importing.write_text('extern int answer(void); int through(void){return answer();}')
        run('gcc', '-fPIC', '-shared', '-nostdlib', '-Wl,-soname,libfuzz-parent.so',
            importing, output / 'seeds' / 'shared' / 'lib-versioned', '-o',
            output / 'seeds' / 'shared' / 'lib-requirements')
        other_source = temp / 'other-version.c'
        other_source.write_text('int other_answer(void){return 7;}')
        other_policy = temp / 'other-version.map'
        other_policy.write_text('OTHER_1 { global: other_answer; };')
        other = temp / 'libother.so'
        run('gcc', '-fPIC', '-shared', '-nostdlib', '-Wl,-soname,libother.so',
            '-Wl,--version-script='+str(other_policy), other_source, '-o', other)
        multiple_source = temp / 'multiple-requirements.c'
        multiple_source.write_text('extern int answer(void),other_answer(void); int both(void){return answer()+other_answer();}')
        multiple = temp / 'multiple-requirements.o'
        run('gcc', '-c', '-O1', '-fPIC', multiple_source, '-o', multiple)
        producers = ['ld']
        lld = shutil.which('ld.lld-22') or shutil.which('ld.lld')
        if lld:
            producers.append(lld)
        for n, producer in enumerate(producers):
            run(producer, '-shared', '-soname', 'libmultiple.so', multiple, other,
                output / 'seeds' / 'shared' / 'lib-versioned', '-o',
                output / 'seeds' / 'shared' / ('lib-multiple-'+str(n)))
        unique_source = temp / 'unique.s'
        unique_source.write_text('''
.data
.globl unique_data
.type unique_data,@gnu_unique_object
.size unique_data,4
unique_data: .long 35
.text
.globl unique_address
.type unique_address,@function
unique_address: mov unique_data@GOTPCREL(%rip),%rax; ret
.section .note.GNU-stack,"",@progbits
''')
        run('gcc', '-shared', '-nostdlib', '-Wl,-soname,libunique.so', unique_source,
            '-o', output / 'seeds' / 'shared' / 'lib-unique')
        copy_source = temp / 'copy.c'
        copy_source.write_text('''int datum=42; int zero_data[31];
__attribute__((aligned(8192))) long aligned_data=17;
const int constants[3]={1,2,3}; int *const pointer=&datum;
int old_data=19,new_data=23;
__asm__(".symver old_data,versioned@FAS_1,remove");
__asm__(".symver new_data,versioned@@FAS_2,remove");
''')
        copy_policy = temp / 'copy.map'
        copy_policy.write_text('FAS_1 {}; FAS_2 {} FAS_1;')
        run('gcc', '-fPIC', '-shared', '-nostdlib', '-Wl,-soname,libcopy.so',
            '-Wl,--version-script='+str(copy_policy), copy_source,
            '-o', output / 'seeds' / 'shared' / 'lib-copy')
        for seed in (output / 'seeds' / 'shared').iterdir():
            shutil.copyfile(seed, output / 'seeds' / 'dependency' / seed.name)
            shutil.copyfile(seed, output / 'seeds' / 'copy-link' / seed.name)
        run('ar', 'rcs', output / 'seeds' / 'archive' / 'indexed', obj, metadata)
        run('ar', 'rcS', output / 'seeds' / 'archive' / 'unindexed', obj)
        run('ar', 'rcsT', output / 'seeds' / 'archive' / 'thin', obj, metadata)
        regular = temp / 'nested.a'
        run('ar', 'rcs', regular, obj, metadata)
        run('ar', 'rcsT', output / 'seeds' / 'archive' / 'thin-nested', regular)
        (output / 'seeds' / 'archive' / 'empty').write_bytes(b'!<arch>\n')
    for n, payload in enumerate((b'', b'A'*65536, bytes(range(256))*256)):
        for level in (0, 1, 9):
            (output / 'seeds' / 'zlib' / f'{n}-{level}').write_bytes(
                struct.pack('<I', len(payload)) + zlib.compress(payload, level))
    scripts = ['INPUT(start.o)', 'GROUP ( /lib/libc.so.6 AS_NEEDED ( -lother ) )',
               'OUTPUT_FORMAT(elf64-x86-64) INPUT("a b.o", c.o);',
               '/* comment */ INPUT ( \'quoted.o\' ) # end\n']
    for index, text in enumerate(scripts):
        (output / 'seeds' / 'script' / str(index)).write_text(text)
    for index, text in enumerate(('{ global: answer; local: *; };',
                                 'FAS_1 { global: answer; local: private*; }; FAS_2 { global: datum; } FAS_1;',
                                 'EMPTY {};')):
        (output / 'seeds' / 'version' / str(index)).write_text(text)


def import_corpora(output, modes, roots):
    """Reuse saved queues against a fresh instrumented binary, retaining hashes."""
    imported = []
    for root in roots:
        root = root.resolve()
        found = False
        for mode in modes:
            queue = root/mode/'default'/'queue'
            if not queue.is_dir():
                continue
            found = True
            hashes = []
            for path in sorted(queue.iterdir()):
                if not path.name.startswith('id:') or not path.is_file():
                    continue
                with path.open('rb') as stream:
                    data = stream.read(1024*1024+1)
                if not data or len(data)>1024*1024:
                    continue
                digest = hashlib.sha256(data).hexdigest()
                destination = output/'seeds'/mode/('import-'+digest)
                if destination.exists():
                    continue
                destination.write_bytes(data)
                hashes.append(digest)
            imported.append({'root': str(root), 'mode': mode,
                             'count': len(hashes), 'sha256': hashes})
        if not found:
            raise SystemExit('No selected target queues found under '+str(root))
    return imported


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output', required=True, type=Path,
                        help='new directory; preserves binary, seeds, queues and report')
    parser.add_argument('--seconds', type=int, default=180,
                        help='seconds per target (default 180)')
    parser.add_argument('--modes', nargs='+', choices=MODES, default=list(MODES))
    parser.add_argument('--import-corpus', action='append', type=Path, default=[],
                        help='reuse queues from a previous campaign directory (repeatable)')
    args = parser.parse_args()
    if args.seconds < 1:
        parser.error('--seconds must be positive')
    output = args.output.resolve()
    if output.is_relative_to(ROOT):
        parser.error('--output must be outside the source tree')
    output.mkdir(parents=True, exist_ok=False)
    target = output / 'target.exe'
    with tempfile.TemporaryDirectory(prefix='fas-fuzz-build-') as build:
        run('dune', 'build', '--build-dir=' + build, '--profile', 'fuzz',
            'src/linker/fuzz/target.exe', cwd=ROOT)
        shutil.copy2(Path(build) / 'default/src/linker/fuzz/target.exe', target)
    seeds(output)
    imported = import_corpora(output, args.modes, args.import_corpus)
    report = {'seconds_per_target': args.seconds, 'modes': args.modes,
              'imported_corpora': imported,
              'started': time.strftime('%Y-%m-%dT%H:%M:%S%z'),
              'platform': platform.platform(),
              'target_sha256': hashlib.sha256(target.read_bytes()).hexdigest(),
              'ocaml': run('ocamlopt', '-version', capture_output=True, text=True).stdout.strip(),
              'git_head': run('git', 'rev-parse', 'HEAD', cwd=ROOT,
                              capture_output=True, text=True).stdout.strip(),
              'git_status': run('git', 'status', '--short', cwd=ROOT,
                                capture_output=True, text=True).stdout,
              'targets': {}}
    (output / 'source.diff').write_bytes(run('git', 'diff', cwd=ROOT, capture_output=True).stdout)
    # Preserve new, untracked linker sources too; git diff alone omits them.
    shutil.copytree(ROOT / 'src/linker', output / 'source',
                    ignore=shutil.ignore_patterns('__pycache__', '*.pyc'))
    env = dict(os.environ, AFL_NO_UI='1', AFL_SKIP_CPUFREQ='1', AFL_NO_AFFINITY='1',
               AFL_I_DONT_CARE_ABOUT_MISSING_CRASHES='1', AFL_CRASH_EXITCODE='2')
    report['afl_environment'] = {key: value for key, value in env.items()
                                  if key.startswith('AFL_')}
    report['core_pattern'] = Path('/proc/sys/kernel/core_pattern').read_text().strip()
    for mode in args.modes:
        command = ['nice', '-n', '10', 'afl-fuzz', '-i', str(output / 'seeds' / mode),
                   '-o', str(output / mode), '-m', 'none', '-t', '2000',
                   '-V', str(args.seconds), '--', str(target), mode, '@@']
        print('Fuzzing ' + mode + ' for ' + str(args.seconds) + ' seconds', flush=True)
        start = time.monotonic()
        with (output / (mode + '.log')).open('wb') as log:
            result = subprocess.run(command, env=env, stdout=log, stderr=subprocess.STDOUT)
        stats_file = output / mode / 'default/fuzzer_stats'
        stats = {}
        if stats_file.exists():
            for line in stats_file.read_text().splitlines():
                key, _, value = line.partition(':')
                stats[key.strip()] = value.strip()
        report['targets'][mode] = {'exit_code': result.returncode,
                                   'elapsed_seconds': time.monotonic() - start,
                                   'command': command, 'stats': stats}
        (output / 'report.json').write_text(json.dumps(report, indent=2) + '\n')
        if result.returncode or not stats:
            raise SystemExit('AFL failed; inspect ' + str(output / (mode + '.log')))
    print('Report: ' + str(output / 'report.json'), flush=True)
    if any(int(target['stats'].get(key, '0')) for target in report['targets'].values()
           for key in ('saved_crashes', 'saved_hangs')):
        raise SystemExit('Campaign saved findings; replay and triage them before accepting the run')


if __name__ == '__main__':
    main()
