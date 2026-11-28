#!/usr/bin/env python3
"""Parallel differential corpus tester for fas-as against llvm-mc-22.

Compiles C files with clang-22 -S and gcc -S across flag sets, assembles each .s
with both fas-as and llvm-mc-22, and compares section contents, non-local symbols,
and relocations.
"""

import concurrent.futures, glob, os, struct, subprocess, sys, tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "tools/refcache"))
from compiler import assemble
from refcache import run as cached_run
from collections import Counter

def parse_elf(data, strict=False):
    e_shoff = struct.unpack_from('<Q', data, 40)[0]
    e_shentsize, e_shnum, e_shstrndx = struct.unpack_from('<HHH', data, 58)
    shdrs = [struct.unpack_from('<IIQQQQIIQQ', data, e_shoff + i * e_shentsize) for i in range(e_shnum)]
    shstr_off = shdrs[e_shstrndx][4]
    def get_str(base, idx):
        end = data.index(b'\x00', base + idx)
        return data[base + idx:end].decode('utf-8', 'replace')

    secs = {}
    symtab = None
    strtab_off = 0
    relas_raw = {}
    for i, sh in enumerate(shdrs):
        name = get_str(shstr_off, sh[0])
        sh_type, sh_flags, sh_off, sh_size = sh[1], sh[2], sh[4], sh[5]
        if name == '.symtab':
            symtab = sh
        elif name == '.strtab':
            strtab_off = sh_off
        elif sh_type == 4:
            target = get_str(shstr_off, shdrs[sh[7]][0])
            relas_raw[target] = sh
        elif name and sh_type != 0 and i != e_shstrndx and (strict or
                (name != '.llvm_addrsig' and not name.startswith('.debug'))):
            attributes = (sh_type, sh_flags, sh[8], sh[9], sh_size)
            payload = b'' if sh_type == 8 else data[sh_off:sh_off + sh_size]
            secs[name] = (attributes, payload)

    # Extract symbols (non-temporary)
    syms = {}
    sym_names = {}
    if symtab is not None:
        s_off, s_sz = symtab[4], symtab[5]
        for idx in range(s_sz // 24):
            st_name, st_info, st_other, st_shndx, st_val, st_size = struct.unpack_from('<IBBHQQ', data, s_off + idx * 24)
            bind, stype = st_info >> 4, st_info & 0xf
            if stype == 3 and st_shndx < len(shdrs):
                sname = get_str(shstr_off, shdrs[st_shndx][0])
            else:
                sname = get_str(strtab_off, st_name)
            sym_names[idx] = (sname, bind, stype, st_shndx, st_val)
            if sname and stype != 3 and (bind != 0 or strict):
                sec_name = get_str(shstr_off, shdrs[st_shndx][0]) if 0 < st_shndx < len(shdrs) else str(st_shndx)
                syms[sname] = (bind, stype, st_other, sec_name, st_val, st_size)

    relas = {}
    for target, sh in relas_raw.items():
        if not strict and target.startswith('.debug'):
            continue
        r_off, r_sz = sh[4], sh[5]
        lst = []
        for off in range(r_off, r_off + r_sz, 24):
            r_offset, r_info, r_addend = struct.unpack_from('<QQq', data, off)
            sym_idx = r_info >> 32
            rtype = r_info & 0xffffffff
            sname, bind, stype, st_shndx, st_val = sym_names.get(sym_idx, ('?', 0, 0, 0, 0))
            # The older corpus allows equivalent local symbol and section targets.
            if not strict and bind == 0 and stype != 3 and 0 < st_shndx < len(shdrs):
                sname = get_str(shstr_off, shdrs[st_shndx][0])
                r_addend += st_val
            lst.append((r_offset, rtype, sname, r_addend))
        relas[target] = lst
    return secs, syms, relas

def compare_objs(mc_data, fas_data, strict=False):
    ms, msy, mr = parse_elf(mc_data, strict)
    fs, fsy, fr = parse_elf(fas_data, strict)
    diffs = []
    # Compare all code/data/eh_frame sections present in mc
    for name, (meta, mb) in ms.items():
        if not strict and name in ('.note.GNU-stack', '.comment'):
            continue
        if name not in fs:
            diffs.append(f"missing section {name}")
            continue
        actual_meta, fb = fs[name]
        if strict and meta != actual_meta:
            diffs.append(f"section {name} (type, flags, align, entsize, size): ref={meta} fas={actual_meta}")
        elif not strict and meta[0] == 8 and meta[4] != actual_meta[4]:
            diffs.append(f"section {name} size differs (ref={meta[4]} fas={actual_meta[4]})")
        if mb != fb:
            # Find first diff byte
            k = 0
            while k < len(mb) and k < len(fb) and mb[k] == fb[k]: k += 1
            diffs.append(f"section {name} bytes differ at 0x{k:x} (mc len={len(mb)} fas len={len(fb)}, mc={mb[k:k+8].hex()} fas={fb[k:k+8].hex()})")
    targets = sorted(mr.keys() | fr.keys()) if strict else mr
    for target in targets:
        if not strict and target == '.comment':
            continue
        ml = mr.get(target, [])
        fl = fr.get(target, [])
        if ml != fl:
            n = 0
            while n < min(len(ml), len(fl)) and ml[n] == fl[n]:
                n += 1
            diffs.append(f"relas {target} differ at entry {n} (ref={len(ml)} fas={len(fl)}, ref={ml[n:n+2]} fas={fl[n:n+2]})")
    for sname, mval in msy.items():
        if sname not in fsy:
            diffs.append(f"missing symbol {sname}")
        else:
            fval = fsy[sname]
            if (mval != fval if strict else mval[:5] != fval[:5]):
                diffs.append(f"sym {sname} differs: mc={mval} fas={fval}")
    if strict:
        for name in sorted(fs.keys() - ms.keys()):
            diffs.append(f"extra section {name}")
        for name in sorted(fsy.keys() - msy.keys()):
            diffs.append(f"extra symbol {name}")
    return diffs

def test_one(args):
    fas_as, cc, flags, cfile = args
    with tempfile.TemporaryDirectory() as td:
        s_path = os.path.join(td, 't.s')
        mc_o = os.path.join(td, 'mc.o')
        fas_o = os.path.join(td, 'fas.o')
        inc = ['-I', os.path.dirname(cfile)]
        p = cached_run([cc, '-S'] + flags + inc + [cfile, '-o', s_path],
                       ns="fas-as/corpus-c", deps=[cfile], outs=[s_path], scratch=td, c=True)
        if p.returncode != 0:
            return ('SKIP', cfile, '')
        p_mc = assemble(['llvm-mc-22', '-triple=x86_64-unknown-linux-gnu', '-filetype=obj', s_path, '-o', mc_o],
                        ns="fas-as/corpus-mc", source=s_path, output=mc_o, scratch=td)
        if p_mc.returncode != 0:
            return ('SKIP', cfile, '')
        p_fas = subprocess.run([fas_as, s_path, '-o', fas_o], capture_output=True)
        if p_fas.returncode != 0:
            err = p_fas.stderr.decode('utf-8', 'replace').strip().splitlines()
            first = err[0] if err else 'unknown error'
            return ('ERR', f"{os.path.basename(cfile)} ({cc} {' '.join(flags)})", first)
        with open(mc_o, 'rb') as f: mc_d = f.read()
        with open(fas_o, 'rb') as f: fas_d = f.read()
        diffs = compare_objs(mc_d, fas_d)
        if diffs:
            return ('DIFF', f"{os.path.basename(cfile)} ({cc} {' '.join(flags)})", diffs[0])
        return ('PASS', cfile, '')

def main():
    if len(sys.argv) == 4 and sys.argv[1] == '--compare':
        reference, actual = map(Path, sys.argv[2:])
        diffs = compare_objs(reference.read_bytes(), actual.read_bytes(), strict=True)
        for diff in diffs:
            print(diff)
        return int(bool(diffs))

    fas_as = sys.argv[1]
    upstream = sys.argv[2]
    mode = sys.argv[3] if len(sys.argv) > 3 else 'quick'

    files = []
    files.extend(sorted(glob.glob(os.path.join(upstream, 'c-testsuite/tests/single-exec/*.c'))))
    if mode in ('full', 'libs'):
        files.extend(sorted(glob.glob(os.path.join(upstream, 'zlib/*.c'))))
        files.extend(sorted(glob.glob(os.path.join(upstream, 'lua/src/*.c'))))
        sq = os.path.join(upstream, 'sqlite/sqlite3.c')
        if os.path.exists(sq): files.append(sq)
    if mode == 'full':
        files.extend(sorted(glob.glob(os.path.join(upstream, 'gcc/gcc/testsuite/gcc.c-torture/execute/*.c'))))

    flag_sets = [
        ('clang-22', ['-O0']),
        ('clang-22', ['-O2']),
        ('clang-22', ['-O2', '-fPIC']),
        ('gcc', ['-O0']),
        ('gcc', ['-O2']),
    ]
    if mode == 'quick':
        flag_sets = [('clang-22', ['-O0']), ('clang-22', ['-O2']), ('gcc', ['-O2'])]

    jobs = [(fas_as, cc, fl, f) for f in files for cc, fl in flag_sets]
    counts = Counter()
    err_reasons = Counter()
    samples = {}

    jobs_limit = int(os.environ.get('FAS_AS_JOBS', os.cpu_count() or 8))
    with concurrent.futures.ThreadPoolExecutor(max_workers=jobs_limit) as ex:
        for status, label, msg in ex.map(test_one, jobs):
            counts[status] += 1
            if status in ('ERR', 'DIFF'):
                # Normalize error reason (strip filename:line)
                reason = msg.split(': error: ', 1)[-1] if ': error: ' in msg else msg[:80]
                err_reasons[reason] += 1
                if reason not in samples:
                    samples[reason] = f"{label}: {msg}"

    print(f"corpus ({mode}): PASS={counts['PASS']} ERR={counts['ERR']} DIFF={counts['DIFF']} SKIP={counts['SKIP']}")
    for reason, cnt in err_reasons.most_common(25):
        print(f"  [{cnt:4d}] {reason}")
        print(f"         e.g. {samples[reason]}")
    return 0 if counts['ERR'] == 0 and counts['DIFF'] == 0 else 1

if __name__ == '__main__':
    sys.exit(main())
