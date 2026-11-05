#!/usr/bin/env python3
"""Reproducible large-input correctness checks; deliberately no speed scores."""
import argparse
import hashlib
import json
import mmap
from pathlib import Path
import shutil
import struct
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[3]


def digest(path):
    with path.open('rb') as source:
        return hashlib.file_digest(source, 'sha256').hexdigest()


class Qualification:
    def __init__(self, output, linker):
        self.output = output
        self.commands = []
        self.results = []
        self.linker = output / 'fas-ld'
        shutil.copy2(linker, self.linker)
        self.report = {
            'purpose': 'large-input correctness; no performance qualification',
            'linker_sha256': digest(self.linker),
            'profile': None,
            'source_sha256': {
                str(p.relative_to(ROOT)): digest(p)
                for p in sorted((ROOT / 'src/linker').rglob('*'))
                if p.is_file() and p.suffix in ('.ml', '.mli', '.py', '.c')
            },
            'tools': {}, 'commands': self.commands, 'results': self.results,
        }
        self.save()

    def save(self):
        self.report['passed'] = sum(r['status'] == 'passed' for r in self.results)
        self.report['failed'] = sum(r['status'] == 'failed' for r in self.results)
        self.report['files'] = {
            str(p.relative_to(self.output)): {'size': p.stat().st_size, 'sha256': digest(p)}
            for p in sorted(self.output.iterdir())
            if p.is_file() and p.name != 'report.json'
        }
        (self.output / 'report.json').write_text(json.dumps(self.report, indent=2) + '\n')

    def run(self, *args, expected=0):
        argv = [str(a) for a in args]
        number = len(self.commands)
        stdout = self.output / f'command-{number:03d}.stdout'
        stderr = self.output / f'command-{number:03d}.stderr'
        row = {'argv': argv, 'expected_exit': expected,
               'stdout': stdout.name, 'stderr': stderr.name}
        self.commands.append(row)
        with stdout.open('wb') as out, stderr.open('wb') as err:
            try:
                result = subprocess.run(argv, stdout=out, stderr=err, timeout=180)
            except subprocess.TimeoutExpired:
                row['timeout'] = True
                raise AssertionError(f'command timed out: {argv}') from None
        row['exit'] = result.returncode
        if result.returncode != expected:
            raise AssertionError(f'{argv}: exit {result.returncode}, expected {expected}: '
                                 + stderr.read_text(errors='replace')[:2000])
        return stdout.read_bytes(), stderr.read_bytes()

    def assemble(self, name, text):
        source = self.output / (name + '.s')
        source.write_text(text + '\n.section .note.GNU-stack,"",@progbits\n')
        obj = source.with_suffix('.o')
        self.run('as', '--64', source, '-o', obj)
        return obj

    def case(self, name, check):
        row = {'name': name, 'status': 'running'}
        self.results.append(row)
        print(name, flush=True)
        try:
            row.update(check() or {})
            row['status'] = 'passed'
        except Exception as error:
            row['status'] = 'failed'
            row['error'] = f'{type(error).__name__}: {error}'
        self.save()
        print('  ' + row['status'], flush=True)

    def link(self, tool, name, *inputs, options=()):
        out = self.output / name
        self.run(tool, *options, '-o', out, *inputs)
        _, stderr = self.run('readelf', '-hW', out)
        assert not stderr, stderr
        return out

    def reject_preserving(self, name, *inputs, diagnostic, options=()):
        output = self.output / name
        output.write_bytes(b'existing executable must survive\n')
        output.chmod(0o640)
        before = output.stat()
        _, stderr = self.run(self.linker, *options, '-o', output, *inputs, expected=1)
        assert diagnostic.encode() in stderr, stderr
        assert b'Fatal error' not in stderr, stderr
        assert output.read_bytes() == b'existing executable must survive\n'
        after = output.stat()
        assert (before.st_ino, before.st_mode) == (after.st_ino, after.st_mode)
        assert not list(self.output.glob('.fas-ld-*.tmp'))

    def files(self):
        entry = self.assemble('file-entry',
                              '.text\n.global _start\n_start: mov $60,%eax; mov $42,%edi; syscall')
        empty = self.assemble('empty', '')
        inputs = [entry]
        for number in range(1023):
            copy = self.output / f'empty-{number:04d}.o'
            shutil.copyfile(empty, copy)
            inputs.append(copy)
        response = self.output / '1024-files.rsp'
        # Quote paths so staging directories with whitespace also work.
        response.write_text('\n'.join('"' + str(p).replace('\\', '\\\\').replace('"', '\\"') + '"'
                                      for p in inputs))
        out = self.link(self.linker, '1024-files', '@' + str(response))
        self.run(out, expected=42)
        self.reject_preserving('1025-files', *inputs, empty,
                               diagnostic='input count exceeds 1024 file limit')
        return {'accepted_files': 1024, 'rejected_files': 1025}

    def sections(self, count):
        text = '.text\n.global _start\n_start: mov $60,%eax; mov $42,%edi; syscall\n'
        text += ''.join(f'.section .rodata.scale{n},"a",@progbits\n.byte {n % 256}\n'
                        for n in range(count))
        obj = self.assemble('many-sections', text)
        for tag, tool in [('fas', self.linker), ('gnu', 'ld')]:
            out = self.link(tool, 'many-sections-' + tag, obj)
            self.run(out, expected=42)
            with out.open('rb') as stream, mmap.mmap(stream.fileno(), 0, access=mmap.ACCESS_READ) as image:
                headers = sections(image)
                payload = [(name, image[offset:offset+size])
                           for name, _, offset, size in headers
                           if name.startswith('.rodata.scale')]
                # GNU combines .rodata.*, while Fas preserves these distinct names.
                if tag == 'fas':
                    assert len(payload) == count, len(payload)
                    assert all(data == bytes([int(name.removeprefix('.rodata.scale')) % 256])
                               for name, data in payload)
                else:
                    rodata = next((offset, size) for name, _, offset, size in headers
                                  if name == '.rodata')
                    offset, size = rodata
                    assert size == count, size
                    assert image[offset:offset+size] == bytes(n % 256 for n in range(count))
        # A second object with distinct section names crosses the final-image
        # section-index boundary without exceeding either input's own count.
        extra = self.assemble('more-sections', ''.join(
            f'.section .rodata.extra{n},"a",@progbits\n.byte 1\n'
            for n in range(0xff00 - count)))
        self.reject_preserving('output-section-overflow', obj, extra,
                               diagnostic='unsupported extended section numbering')
        self.reject_preserving('partial-section-overflow', obj, extra,
                               diagnostic='partial link: too many output sections', options=('-r',))
        return {'retained_payload_sections': count, 'extended_output_numbering': 'rejected'}

    def dense(self, count):
        payload = self.output / 'dense-values.bin'
        payload.write_bytes(bytes(range(256)) * 16)
        definitions = self.assemble('dense-definitions',
            '.data\n.globl values\nvalues:\n.incbin "' + str(payload) + '"\n' +
            ''.join(f'.globl value{n}\n.set value{n},values+{n % 4096}\n' for n in range(count)))
        pointers = self.assemble('dense-pointers',
            '.data\n.p2align 3\n.globl pointers\npointers:\n' +
            ''.join(f'.quad value{n}\n' for n in range(count)))
        entry = self.assemble('dense-entry', f'''
.text
.globl _start
_start:
lea pointers(%rip),%rsi
lea values(%rip),%rdi
xor %ecx,%ecx
1: mov %ecx,%eax
and $4095,%eax
add %rdi,%rax
cmp %rax,(%rsi)
jne 2f
mov (%rax),%al
cmp %cl,%al
jne 2f
add $8,%rsi
inc %ecx
cmp ${count},%ecx
jne 1b
mov $42,%edi
jmp 3f
2: mov $1,%edi
3: mov $60,%eax
syscall
''')
        outputs = []
        for tag, tool in [('fas', self.linker), ('gnu', 'ld')]:
            out = self.link(tool, 'dense-' + tag, entry, pointers, definitions)
            self.run(out, expected=42)
            outputs.append(out)
        partial = self.link(self.linker, 'dense-partial.o', pointers, definitions, options=('-r',))
        for tag, tool in [('fas', self.linker), ('gnu', 'ld')]:
            out = self.link(tool, 'dense-partial-' + tag, entry, partial)
            self.run(out, expected=42)
        repeat = self.link(self.linker, 'dense-fas-repeat', entry, pointers, definitions)
        assert digest(repeat) == digest(outputs[0]), 'nondeterministic dense link'
        return {'named_definitions': count, 'pointer_relocations': count,
                'partial_consumers': ['fas', 'gnu'], 'repeat_byte_identical': True}

    def backed(self, mebibytes):
        size = mebibytes * 1024 * 1024
        payload = self.output / 'large-payload.bin'
        block = bytes(range(256)) * 4096
        with payload.open('wb') as stream:
            for _ in range(mebibytes):
                stream.write(block)
        obj = self.assemble('file-backed', f'''
.text
.global _start
_start:
lea payload(%rip),%rsi
cmpb $0,(%rsi)
jne 1f
cmpb $255,{size-1}(%rsi)
jne 1f
lea payload+{size//2}(%rip),%rax
cmp %rax,middle(%rip)
jne 1f
mov $42,%edi
jmp 2f
1: mov $1,%edi
2: mov $60,%eax
syscall
.data
.p2align 3
middle: .quad payload+{size//2}
.section .data.payload,"aw",@progbits
.p2align 12
payload: .incbin "{payload}"
''')
        for tag, tool in [('fas', self.linker), ('gnu', 'ld')]:
            out = self.link(tool, 'file-backed-' + tag, obj)
            self.run(out, expected=42)
            with out.open('rb') as stream, mmap.mmap(stream.fileno(), 0, access=mmap.ACCESS_READ) as image:
                candidates = [(offset, length) for name, _, offset, length in sections(image)
                              if name in ('.data', '.data.payload') and length >= size]
                assert len(candidates) == 1, candidates
                offset, length = candidates[0]
                start = offset + length - size
                # Hash in bounded chunks; avoid a second whole-image copy in the inspector.
                actual = hashlib.sha256()
                for at in range(start, start + size, len(block)):
                    actual.update(image[at:at+len(block)])
                assert actual.hexdigest() == digest(payload), 'file-backed payload mismatch'
        oversized = self.output / 'too-large-input.o'
        with oversized.open('wb') as stream:
            stream.truncate(128 * 1024 * 1024 + 1)
        self.reject_preserving('oversized-input', oversized,
                               diagnostic='input exceeds 134217728 byte limit')
        return {'payload_bytes': size, 'all_payload_bytes_hashed': True,
                'oversized_input_bytes': oversized.stat().st_size}


def sections(image):
    offset = struct.unpack_from('<Q', image, 40)[0]
    count, names_index = struct.unpack_from('<HH', image, 60)
    name_offset, name_size = struct.unpack_from('<QQ', image, offset + names_index * 64 + 24)
    names = image[name_offset:name_offset + name_size]
    result = []
    for at in range(offset, offset + count * 64, 64):
        name, kind, _, _, position, size = struct.unpack_from('<IIQQQQ', image, at)
        end = names.index(b'\0', name)
        result.append((names[name:end].decode(), kind, position, size))
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('linker', type=Path)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--profile', choices=('quick', 'full'), default='quick')
    args = parser.parse_args()
    for tool in ('as', 'ld', 'readelf'):
        if not shutil.which(tool):
            parser.error(f'required tool unavailable: {tool}')
    if not args.linker.is_file():
        parser.error(f'linker is not a file: {args.linker}')
    args.output = args.output.resolve()
    args.output.mkdir(parents=True, exist_ok=False)
    checks = Qualification(args.output, args.linker.resolve())
    checks.report['profile'] = args.profile
    for tool in ('as', 'ld', 'readelf'):
        stdout, _ = checks.run(tool, '--version')
        checks.report['tools'][tool] = stdout.decode().splitlines()[0]
    checks.case('physical-file-count-boundary', checks.files)
    checks.case('section-count-boundaries', lambda: checks.sections(65260 if args.profile == 'full' else 4096))
    checks.case('dense-symbols-and-relocations', lambda: checks.dense(200000 if args.profile == 'full' else 20000))
    checks.case('large-file-backed-data', lambda: checks.backed(64 if args.profile == 'full' else 8))
    checks.save()
    print(f"{checks.report['passed']} passed, {checks.report['failed']} failed; {args.output / 'report.json'}")
    return 1 if checks.report['failed'] else 0


if __name__ == '__main__':
    sys.exit(main())
