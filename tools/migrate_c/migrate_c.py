#!/usr/bin/env python3
"""Move Fas sources to C namespaces (roadmap v0.2): alias every `use "C"`
as `c` (or `libc` when `c` is taken), then prefix the bare C names the
compiler reports under FAS_MIGRATE_C=1.

usage: migrate_c.py [-I dir | -DNAME ...] file.fas ...
Rewrites the files in place; keep a copy (or a clean git tree) first.
FAS names the compiler (default: the dune build next to this script).
Remove this tool and the FAS_MIGRATE_C hook once no old code is left."""
import os, re, subprocess, sys

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
F = os.environ.get("FAS", os.path.join(ROOT, "_build/default/bin/main.exe"))

def add_alias(src, alias):
    lines = src.split("\n")
    in_c = None
    out = []
    for line in lines:
        if in_c is not None:
            out.append(line)
            if line.strip() == in_c:
                in_c = None
            continue
        m = re.match(r'^(\s*use\s+"C")\s+<<(\w+)\s*$', line)
        if m:
            out.append(f'{m.group(1)} as {alias} <<{m.group(2)}')
            in_c = m.group(2)
            continue
        m = re.match(r'^(\s*use\s+"C"\s+(?:<[^>]+>|"[^"]+"))(\s*)$', line)
        if m:
            out.append(m.group(1) + " as " + alias)
            continue
        # other heredoc blocks (asm etc.) must be skipped too
        m = re.match(r'^.*<<(\w+)\s*$', line)
        if m and line.lstrip().startswith("use"):
            in_c = m.group(1)
        out.append(line)
    return "\n".join(out)

def edits_for(path, extra):
    env = dict(os.environ, FAS_MIGRATE_C="1")
    r = subprocess.run([F, "check", *extra, os.path.basename(path)], cwd=os.path.dirname(path) or ".",
                       env=env, capture_output=True, text=True)
    edits = set()
    for line in r.stderr.splitlines():
        if line.startswith("FAS-MIGRATE\t"):
            _, file, s, e, name = line.split("\t")
            if os.path.basename(file) == os.path.basename(path):
                edits.add((int(s), name))
    return edits, r

def migrate(path, extra=()):
    src = open(path).read()
    r = migrate_as(path, src, "c", extra)
    if "may not reuse the name" in r.stderr or "has the name of a Fas top-level" in r.stderr:
        r = migrate_as(path, src, "libc", extra)
    return r

def migrate_as(path, src, alias, extra):
    pre = (alias + ".").encode()
    new = add_alias(src, alias)
    for _ in range(5):
        open(path, "w").write(new)
        edits, r = edits_for(path, extra)
        if not edits:
            break
        b = new.encode()
        for off, name in sorted(edits, reverse=True):
            if b[off:off + len(name)].decode(errors="replace") != name:
                print(f"{path}: offset {off} is not `{name}`", file=sys.stderr)
                continue
            if b[max(0, off - len(pre)):off] == pre:
                continue
            b = b[:off] + pre + b[off:]
        new = b.decode()
    open(path, "w").write(new)
    return r

if __name__ == "__main__":
    args, extra = sys.argv[1:], []
    while args and args[0].startswith(("-I", "-D")):
        flag = args.pop(0)
        extra += [flag, os.path.abspath(args.pop(0))] if flag == "-I" else [flag]
    for p in args:
        if not os.path.isfile(p):
            print("FAIL " + p + "  no such file"); continue
        r = migrate(p, extra)
        errs = [l for l in r.stderr.splitlines() if "error" in l and not l.startswith("FAS-MIGRATE")]
        print(("OK   " if r.returncode == 0 else "FAIL ") + p + ("" if r.returncode == 0 else "  " + (errs[0] if errs else r.stderr.strip()[:200])))
