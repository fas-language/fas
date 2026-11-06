#!/usr/bin/env python3
"""Generated valid-program oracles across direct, archive and mixed partial links."""
import argparse
import hashlib
import json
import random
from pathlib import Path
import shutil
import struct
import subprocess
import time

ROOT = Path(__file__).resolve().parents[3]
MASK = (1 << 64)-1
PIPELINES = ('direct', 'archive', 'fas-partial', 'gnu-partial')
MODES = ('static', 'static-gc', 'static-pie', 'static-pie-gc')


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def rotate(value, shift):
    return ((value << shift) | (value >> (64-shift))) & MASK


def program(seed):
    rng = random.Random(seed)
    count = rng.randrange(4, 10)
    nodes = []
    for i in range(count):
        children = list(range(i+1, count))
        edges = [] if not children else [(children[0], rng.randrange(1, 64))]
        if len(children) > 1:
            edges += [(child, rng.randrange(1, 64)) for child in
                      rng.sample(children[1:], rng.randrange(min(2, len(children)-1)+1))]
        nodes.append({'bias': rng.getrandbits(64), 'datum': rng.getrandbits(64), 'edges': edges,
                      'alignment': 1 << rng.randrange(3, 9), 'got': bool(rng.getrandbits(1))})
    values = [0]*count
    for i in reversed(range(count)):
        node = nodes[i]
        value = (node['bias']+node['datum']) & MASK
        for child, shift in node['edges']:
            value ^= rotate(values[child], shift)
        values[i] = value
    strong, weak = rng.getrandbits(64), rng.getrandbits(64)
    order = list(range(count+2))
    rng.shuffle(order)
    return {'seed': seed, 'nodes': nodes, 'strong': strong, 'weak': weak, 'order': order,
            'expected': (values[0]+strong+42) & MASK}


def sources(model):
    nodes = model['nodes']
    output = {}
    for i, node in enumerate(nodes):
        lines = [f'.section .text.fn{i},"ax",@progbits', f'.globl fn{i}',
                 f'.type fn{i},@function', f'fn{i}:', '.cfi_startproc',
                 'push %rbx', '.cfi_def_cfa_offset 16', '.cfi_offset 3,-16',
                 f'movabs ${node["bias"]},%rbx']
        if node['got']:
            lines += [f'mov datum{i}@GOTPCREL(%rip),%rax', 'add (%rax),%rbx']
        else:
            lines += [f'lea pointer{i}(%rip),%rax', 'add (%rax),%rax', 'add (%rax),%rbx']
        for child, shift in node['edges']:
            lines += [f'call fn{child}', f'rol ${shift},%rax', 'xor %rax,%rbx']
        lines += ['mov %rbx,%rax', 'pop %rbx', '.cfi_def_cfa_offset 8',
                  '.cfi_restore 3', 'ret', '.cfi_endproc', f'.size fn{i},.-fn{i}',
                  f'.section .data.datum{i},"aw",@progbits', f'.balign {node["alignment"]}',
                  f'.hidden datum{i}', f'.globl datum{i}', f'datum{i}: .quad {node["datum"]}',
                  f'.section .rodata.pointer{i},"a",@progbits', '.p2align 3',
                  f'pointer{i}: .quad datum{i} - .', f'.comm common,{8*(i+1)},{node["alignment"]}',
                  f'.section .text.dead{i},"ax",@progbits', f'.globl dead{i}',
                  f'dead{i}: ret']
        output[f'node{i}'] = '\n'.join(lines)+'\n'
    for name, binding, value in [('weak', '.weak', model['weak']), ('strong', '.globl', model['strong'])]:
        output[name] = (f'.section .text.chosen_{name},"ax",@progbits\n'
                        f'{binding} chosen\n.type chosen,@function\nchosen: movabs ${value},%rax; ret\n'
                        f'.globl anchor_{name}\nanchor_{name}: xor %eax,%eax; ret\n')
    output['start'] = f'''.section .text.start,"ax",@progbits
.globl _start
_start:
.cfi_startproc
cmpq $0,result(%rip)
jne failed
cmpq $0,common+{8*(len(nodes)-1)}(%rip)
jne failed
movq $42,common+{8*(len(nodes)-1)}(%rip)
call fn0
mov %rax,result(%rip)
call anchor_weak
call anchor_strong
call chosen
add %rax,result(%rip)
mov common+{8*(len(nodes)-1)}(%rip),%rax
add %rax,result(%rip)
lea result_pointer(%rip),%rsi
add (%rsi),%rsi
mov $1,%eax
mov $1,%edi
mov $8,%edx
syscall
cmp $8,%rax
jne failed
xor %edi,%edi
jmp finish
failed: mov $99,%edi
finish: mov $60,%eax
syscall
.cfi_endproc
.section .bss.result,"aw",@nobits
.p2align 3
result: .zero 8
.section .rodata.result_pointer,"a",@progbits
.p2align 3
result_pointer: .quad result - .
'''
    output['retained'] = ('.section .text.retained,"axR",@progbits\n'
                          '.globl retained_root\nretained_root: ret\n')
    return {name: text+'.section .note.GNU-stack,"",@progbits\n' for name, text in output.items()}


def inspect(path):
    # Independent ELF inspection, without importing linker implementation code.
    data = path.read_bytes()
    kind, machine = struct.unpack_from('<HH', data, 16)
    if data[:6] != b'\x7fELF\x02\x01' or machine != 62:
        raise ValueError('unexpected output target')
    offset = struct.unpack_from('<Q', data, 40)[0]
    count = struct.unpack_from('<H', data, 60)[0]
    sections = [struct.unpack_from('<IIQQQQIIQQ', data, offset+64*i) for i in range(count)]
    table = next(s for s in sections if s[1] == 2)
    strings = sections[table[6]]
    names = data[strings[4]:strings[4]+strings[5]]
    symbols = {}
    for at in range(table[4], table[4]+table[5], 24):
        name, info, other, section, value, size = struct.unpack_from('<IBBHQQ', data, at)
        name = names[name:].split(b'\0', 1)[0].decode()
        if name:
            symbols[name] = {'binding': info >> 4, 'section': section, 'value': value, 'size': size}
    phoff = struct.unpack_from('<Q', data, 32)[0]
    phnum = struct.unpack_from('<H', data, 56)[0]
    segments = [struct.unpack_from('<IIQQQQQQ', data, phoff+56*i) for i in range(phnum)]
    if any(p[0] == 3 or (p[0] == 1 and p[1] & 3 == 3) for p in segments):
        raise ValueError('unexpected interpreter or writable executable segment')
    return kind, symbols


class Campaign:
    def __init__(self, output, linker, gnu='ld', lld=None):
        self.root = output.resolve()
        if self.root.is_relative_to(ROOT):
            raise ValueError('output must be outside the repository')
        self.root.mkdir(parents=True, exist_ok=False)
        self.tools = {'fas': self.root/'fas-ld', 'gnu': Path(shutil.which(str(gnu)) or gnu).absolute()}
        shutil.copy2(linker, self.tools['fas'])
        if lld:
            self.tools['lld'] = Path(shutil.which(str(lld)) or lld).absolute()
        shutil.copy2(Path(__file__), self.root/'semantic-run.py')
        self.report = {'status': 'running', 'timing_comparison': False,
                       'started': time.strftime('%Y-%m-%dT%H:%M:%S%z'),
                       'linker_sha256': sha(self.tools['fas']), 'harness_sha256': sha(Path(__file__)),
                       'tools': {name: {'path': str(path), 'version': subprocess.check_output(
                           [path, '--version'], text=True).splitlines()[0]} for name, path in self.tools.items()},
                       'cases': [], 'final_links': 0, 'executions': 0, 'partial_links': 0}
        self.save()

    def save(self):
        (self.root/'report.json').write_text(json.dumps(self.report, indent=2)+'\n')

    def run(self, args, expected=0):
        p = subprocess.run(list(map(str, args)), capture_output=True, timeout=30)
        with (self.root/'commands.jsonl').open('a') as log:
            log.write(json.dumps({'argv': list(map(str, args)), 'exit': p.returncode,
                                  'stdout_hex': p.stdout.hex(), 'stderr': p.stderr.decode(errors='replace')})+'\n')
        if p.returncode != expected:
            raise ValueError(f'{args}: exit {p.returncode}, expected {expected}: '+p.stderr.decode(errors='replace'))
        return p

    def case(self, seed, modes=MODES, pipelines=PIPELINES):
        folder = self.root/('seed-'+str(seed))
        folder.mkdir()
        model = program(seed)
        (folder/'model.json').write_text(json.dumps(model, indent=2)+'\n')
        objects = {}
        for name, text in sources(model).items():
            path = folder/(name+'.s')
            path.write_text(text)
            objects[name] = folder/(name+'.o')
            self.run(['as', '--64', '-o', objects[name], path])
        providers = [objects['node'+str(i)] for i in range(len(model['nodes']))]+[objects['weak'], objects['strong']]
        providers = [providers[i] for i in model['order']]
        retained = folder/'retained.a'
        self.run(['ar', 'rcs', retained, objects['retained']])
        inputs = {'direct': [objects['start'], *providers, retained]}
        if 'archive' in pipelines:
            archive = folder/'providers.a'
            self.run(['ar', 'rcs', archive, *providers])
            inputs['archive'] = [objects['start'], archive, retained]
        for pipeline, first, second in [('fas-partial', 'fas', 'gnu'), ('gnu-partial', 'gnu', 'fas')]:
            if pipeline not in pipelines:
                continue
            one, two = folder/(pipeline+'-one.o'), folder/(pipeline+'-two.o')
            half = len(providers)//2
            self.run([self.tools[first], '-r', '-u', 'retained_root', '-o', one, *providers[:half]])
            self.run([self.tools[second], '-r', '-o', two, objects['start'], one, *providers[half:]])
            self.report['partial_links'] += 2
            kind, symbols = inspect(two)
            if kind != 1 or symbols['retained_root']['section'] != 0 or symbols['retained_root']['binding'] != 1:
                raise ValueError('partial pipeline lost the forced undefined root')
            inputs[pipeline] = [two, retained]
        row = {'seed': seed, 'model_sha256': sha(folder/'model.json'),
               'expected_hex': model['expected'].to_bytes(8, 'little').hex(), 'runs': []}
        for pipeline in pipelines:
            for mode in modes:
                flags = ['--gc-sections'] if mode.endswith('-gc') else []
                if 'pie' in mode:
                    flags += ['-pie', '--no-dynamic-linker']
                roots = ['-u', 'retained_root'] if pipeline in ('direct', 'archive') else []
                for tool, linker in self.tools.items():
                    output = folder/(pipeline+'-'+mode+'-'+tool)
                    self.run([linker, *flags, *roots, '-o', output, *inputs[pipeline]])
                    self.report['final_links'] += 1
                    result = self.run([output])
                    self.report['executions'] += 1
                    if result.stdout != model['expected'].to_bytes(8, 'little') or result.stderr:
                        raise ValueError(f'seed {seed}: {pipeline}/{mode}/{tool}: runtime value differs from Python oracle')
                    kind, symbols = inspect(output)
                    if kind != (3 if 'pie' in mode else 2) or symbols['retained_root']['section'] == 0:
                        raise ValueError('incorrect output kind or later archive provider missing')
                    if symbols['common']['size'] != len(model['nodes'])*8:
                        raise ValueError('common size does not match independent model')
                    for i in range(len(model['nodes'])):
                        if (('dead'+str(i)) in symbols) == mode.endswith('-gc'):
                            raise ValueError('dead-section retention differs from GC policy')
                    row['runs'].append({'pipeline': pipeline, 'mode': mode, 'tool': tool,
                                        'output_sha256': sha(output), 'status': 'passed'})
        row['status'] = 'passed'
        self.report['cases'].append(row)
        self.save()
        print('PASSED seed', seed, flush=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--linker', type=Path, default=ROOT/'_build/default/src/linker/bin/main.exe')
    parser.add_argument('--gnu', default='ld')
    parser.add_argument('--lld', help='optional independent LLD consumer')
    parser.add_argument('--seed', type=int, default=0xFA500)
    parser.add_argument('--cases', type=int, default=64)
    parser.add_argument('--modes', nargs='+', choices=MODES, default=MODES)
    parser.add_argument('--pipelines', nargs='+', choices=PIPELINES, default=PIPELINES)
    args = parser.parse_args()
    if args.cases <= 0:
        parser.error('--cases must be positive')
    campaign = Campaign(args.output, args.linker.resolve(), args.gnu, args.lld)
    try:
        for seed in range(args.seed, args.seed+args.cases):
            campaign.case(seed, args.modes, args.pipelines)
        campaign.report['status'] = 'passed'
    except Exception as error:
        campaign.report['status'] = 'failed'
        campaign.report['error'] = str(error)
        raise
    finally:
        campaign.save()


if __name__ == '__main__':
    main()
