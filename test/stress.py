#!/usr/bin/env python3
# Large-input stress test (roadmap v0.7 hardening; not part of dune test).
#
#   stress.py FAS_FIR FAS [FAMILY...]
#
# Each family generates a program at 1x and at 10x its size, compiles it
# natively, runs it and checks its output against the value this script
# computes. A family passes when both sizes are correct, nothing crashes
# (an OCaml stack overflow is a bug), and the 10x compile takes at most
# RATIO (default 25) times the 1x one: roughly linear, with slack for noise
# and for the fixed cost of a run. The 10x sizes are the roadmap's:
#
#   insts    one function of 1M instructions (straight-line arithmetic)
#   blocks   one function of 100k blocks (a chain of branches)
#   switch   a 10k-case switch, called for every case and some misses
#   funcs    100k functions, each called once
#   globals  a 10 MB initialized global and a 640 MB zero one
#   lanes    4096-lane vector arithmetic and reductions
#   nesting  Fas source nested 100 deep (must run) and 10k, 100k deep
#            (must run or be refused cleanly): parentheses, blocks,
#            ifs, `1 + 1 + ...` chains, `argv + 1 + ...` address chains
#
# Times go to stdout; STRESS_RATIO overrides the ratio, STRESS_KEEP keeps
# the generated files in that directory.
import os, shutil, subprocess, sys, tempfile, time

M = (1 << 64) - 1
fir_tool, fas_tool = sys.argv[1], sys.argv[2]
families = sys.argv[3:] or ["insts", "blocks", "switch", "funcs", "globals", "lanes", "nesting"]
ratio = float(os.environ.get("STRESS_RATIO", "25"))
here = os.path.dirname(os.path.abspath(__file__))
putnum = os.path.join(here, "fas_fir", "putnum.fir")
HEAD = 'triple "x86_64-unknown-linux-gnu"\n'
PUT = "declare @putnum (i64) -> () global\n"


def main_tail(var):
    return f"  call (i64) -> () @putnum({var})\n  ret i32 0\n}}\n"


# ---------- Fir families: (text, expected stdout) at size n ----------

def insts(n):
    # x = (x * K_i + c_i) ^ (x >> 7), three instructions per step
    out = [HEAD, PUT, "func @main () -> (i32) global {\n^e:\n  %x0 = add i64 i64 0, i64 12345\n"]
    x = 12345
    for i in range(n // 3):
        k = 2 * i + 3
        out.append(f"  %m{i} = mul i64 %x{i}, i64 {k}\n  %s{i} = lshr i64 %x{i}, i64 7\n"
                   f"  %x{i + 1} = xor i64 %m{i}, %s{i}\n")
        x = ((x * k) & M) ^ (x >> 7)
    out.append(main_tail(f"%x{n // 3}"))
    return "".join(out), f"{x}\n"


def blocks(n):
    # ^b_i(%x): y = x * 3 + i; on y's low bit go on with y or y ^ i
    out = [HEAD, PUT, "func @main () -> (i32) global {\n^e:\n  jump ^b0(i64 7)\n"]
    x = 7
    for i in range(n):
        nxt = f"^b{i + 1}" if i + 1 < n else "^out"
        out.append(f"^b{i}(%x{i}: i64):\n  %t{i} = mul i64 %x{i}, i64 3\n  %y{i} = add i64 %t{i}, i64 {i}\n"
                   f"  %z{i} = xor i64 %y{i}, i64 {i}\n  %l{i} = and i64 %y{i}, i64 1\n"
                   f"  %c{i} = icmp ne i1 %l{i}, i64 0\n  br %c{i}, {nxt}(%y{i}), {nxt}(%z{i})\n")
        y = (x * 3 + i) & M
        x = y if y & 1 else y ^ i
    out.append("^out(%r: i64):\n" + main_tail("%r"))
    return "".join(out), f"{x}\n"


def switch(n):
    # f(i) = i * 7 + 1 for the cases 0..n-1 (sparse: case k is 3k), 5 otherwise;
    # main sums f over 0..3n+2
    out = [HEAD, PUT, "func @f (i64) -> (i64) {\n^e(%i: i64):\n  switch %i, ^d, ["]
    out.append(", ".join(f"{3 * k}: ^c{k}" for k in range(n)))
    out.append("]\n^d:\n  ret i64 5\n")
    for k in range(n):
        out.append(f"^c{k}:\n  ret i64 {3 * k * 7 + 1}\n")
    out.append("}\n")
    out.append("func @main () -> (i32) global {\n^e:\n  jump ^l(i64 0, i64 0)\n"
               "^l(%i: i64, %acc: i64):\n  %v = call (i64) -> (i64) @f(%i)\n  %a = add i64 %acc, %v\n"
               f"  %j = add i64 %i, i64 1\n  %c = icmp ult i1 %j, i64 {3 * n + 3}\n  br %c, ^l(%j, %a), ^out(%a)\n"
               "^out(%r: i64):\n" + main_tail("%r"))
    acc = sum((i * 7 + 1) if i % 3 == 0 and i // 3 < n else 5 for i in range(3 * n + 3))
    return "".join(out), f"{acc & M}\n"


def funcs(n):
    # f_i(x) = x * 5 + i, called in turn from main
    out = [HEAD, PUT]
    for i in range(n):
        out.append(f"func @f{i} (i64) -> (i64) {{\n^e(%x: i64):\n  %m = mul i64 %x, i64 5\n"
                   f"  %r = add i64 %m, i64 {i}\n  ret %r\n}}\n")
    out.append("func @main () -> (i32) global {\n^e:\n  %x0 = add i64 i64 0, i64 1\n")
    x = 1
    for i in range(n):
        out.append(f"  %x{i + 1} = call (i64) -> (i64) @f{i}(%x{i})\n")
        x = (x * 5 + i) & M
    out.append(main_tail(f"%x{n}"))
    return "".join(out), f"{x}\n"


def globals_(n):
    # @g: n bytes, byte k = (k * 31 + k >> 8) & 255; @z: 64n zero bytes, of
    # which main writes and reads back a few; main sums @g at stride 4093
    g = bytes(((k * 31) + (k >> 8)) & 255 for k in range(n))
    hexs = "".join("\\%02x" % b for b in g)
    z = 64 * n
    out = [HEAD, PUT, f'global @g size {n} align 16 local readonly = {{bytes "{hexs}"}}\n',
           f"global @z size {z} align 16 local = zero\n",
           "func @main () -> (i32) global {\n^e:\n"
           f"  %zl = ptradd ptr ptr @z, i64 {z - 8}, 1, 0\n  store i64 77, %zl, align 8\n"
           "  jump ^l(i64 0, i64 0)\n"
           "^l(%k: i64, %acc: i64):\n  %p = ptradd ptr ptr @g, %k, 1, 0\n  %b = load i8 %p, align 1\n"
           "  %w = zext i64 %b\n  %a = add i64 %acc, %w\n  %k1 = add i64 %k, i64 4093\n"
           f"  %c = icmp ult i1 %k1, i64 {n}\n  br %c, ^l(%k1, %a), ^out(%a)\n"
           "^out(%r: i64):\n  %zm = ptradd ptr ptr @z, i64 4096, 1, 0\n  %z0 = load i64 %zm, align 8\n"
           "  %zv = load i64 %zl, align 8\n  %r1 = add i64 %r, %zv\n  %r2 = add i64 %r1, %z0\n" + main_tail("%r2")]
    acc = sum(g[k] for k in range(0, n, 4093)) + 77
    return "".join(out), f"{acc}\n"


def lanes(n):
    # vectors of n i32 lanes: v = <0, 1, ..., n-1> (built by a loop of
    # inserts); w = v * 3 + v; print reduce add and reduce xor of w
    t = f"<{n} x i32>"
    out = [HEAD, PUT, "func @main () -> (i32) global {\n^e:\n",
           f"  jump ^f({t} zero, i32 0)\n",
           f"^f(%v: {t}, %i: i32):\n  %v1 = insert {t} %v, %i, %i\n  %i1 = add i32 %i, i32 1\n"
           f"  %c = icmp ult i1 %i1, i32 {n}\n  br %c, ^f(%v1, %i1), ^g(%v1)\n",
           f"^g(%u: {t}):\n  %three = splat {t} i32 3\n  %m = mul {t} %u, %three\n  %w = add {t} %m, %u\n"
           "  %ra = reduce add i32 %w\n  %rx = reduce xor i32 %w\n  %a = zext i64 %ra\n  %x = zext i64 %rx\n"
           "  %s = shl i64 %a, i64 32\n  %r = or i64 %s, %x\n" + main_tail("%r")]
    w = [(4 * i) & 0xFFFFFFFF for i in range(n)]
    ra, rx = sum(w) & 0xFFFFFFFF, 0
    for v in w:
        rx ^= v
    return "".join(out), f"{(ra << 32) | rx}\n"


# ---------- Fas nesting: (source, expected exit code) ----------

NEST_KINDS = ["parens", "blocks", "ifs", "chain", "argchain", "indexchain", "addrchain"]


def nesting(kind, n):
    # main nests one construct n deep and returns 8 when argc > 0. Fas has a
    # nesting limit (Limits.max_nesting): within it the program must run,
    # past it the compiler must refuse it with a diagnostic
    body = {
        "parens": "z i32 = " + "(" * n + "4 + 4" + ")" * n + "\n    y = z",
        "blocks": "{ " * n + "y = 8" + " }" * n,
        "ifs": "if x > 0 { " * n + "y = 8" + " }" * n,
        "chain": "z i32 = " + "1 + " * (n - 1) + "1\n    y = z - " + str(n - 8),
        # the chain as a call argument (its shape cache was once built only
        # for top-level expressions: O(n^2))
        "argchain": "y = id(" + "1 + " * (n - 1) + "1) - " + str(n - 8),
        # an index chain in a generic call argument: the parser's lookahead
        # for `g[...](` and constant folding were each O(n^2) on it
        "indexchain": "y = pick(g[" + "0 + " * (n - 1) + "0], 0, true) + 8",
        # an address offset chain: sema's address facts re-walked the chain
        # below at every level, O(n^2) (a fas_refuse timeout)
        "addrchain": "q addr = argv + " + "1 + " * (n - 1) + "1\n    y = if q == argv { 0 } else { 8 }",
    }[kind]
    src = ("var g arr[2,i32]\n\nfn pick[T](a T, b T, choose bool) T {\n    return if choose { a } else { b }\n}\n\n"
           f"fn id(v i32) i32 {{\n    return v\n}}\n\nfn main(argc i32, argv addr) i32 {{\n    x i32 = argc\n    y i32 = 0\n    {body}\n    return y\n}}\n")
    return src, 8


FIR = {"insts": (insts, 100_000), "blocks": (blocks, 10_000), "switch": (switch, 1_000),
       "funcs": (funcs, 10_000), "globals": (globals_, 1 << 20), "lanes": (lanes, 4096)}


def run_fir(tmp, name, n):
    text, want = FIR[name][0](n)
    src = os.path.join(tmp, f"{name}_{n}.fir")
    open(src, "w").write(text)
    exe = os.path.join(tmp, f"{name}_{n}")
    t = time.monotonic()
    r = subprocess.run([fir_tool, src, "-o", exe, putnum_o], capture_output=True, text=True, timeout=3600)
    dt = time.monotonic() - t
    if r.returncode != 0:
        return dt, f"compile exit {r.returncode}: {(r.stderr.strip().splitlines() or ['?'])[-1][:300]}"
    r = subprocess.run([exe], capture_output=True, text=True, timeout=600)
    if r.returncode != 0 or r.stdout != want:
        return dt, f"run exit {r.returncode}, got {r.stdout.strip()!r}, want {want.strip()!r}"
    return dt, None


def run_fas(tmp, n):
    # every kind at depth n: success (and the right exit code when run) or
    # a clean refusal (exit 1, a diagnostic, no internal error or exception)
    total = 0.0
    for kind in NEST_KINDS:
        src_text, want = nesting(kind, n)
        d = os.path.join(tmp, f"nest_{kind}_{n}")
        os.makedirs(d, exist_ok=True)
        open(os.path.join(d, "m.fas"), "w").write(src_text)
        t = time.monotonic()
        r = subprocess.run([fas_tool, "m.fas", "-o", "m"], capture_output=True, text=True, timeout=3600, cwd=d,
                           env=dict(os.environ, OCAMLRUNPARAM="b"))
        total += time.monotonic() - t
        low = r.stderr.lower()
        if r.returncode == 1 and not any(w in low for w in ["internal error", "fatal error", "exception", "stack overflow"]):
            if n <= 100:
                return total, f"{kind}: refused within the nesting limit: {r.stderr.strip()[:200]}"
            continue
        if r.returncode != 0:
            return total, f"{kind}: compile exit {r.returncode}: {(r.stderr.strip().splitlines() or ['?'])[-1][:300]}"
        r = subprocess.run([os.path.join(d, "m"), "x"], capture_output=True, timeout=600)
        if r.returncode != want:
            return total, f"{kind}: exit {r.returncode}, want {want}"
    return total, None


keep = os.environ.get("STRESS_KEEP")
tmp = keep or tempfile.mkdtemp()
fail = False
putnum_o = os.path.join(tmp, "putnum.o")
subprocess.run([fir_tool, "-c", putnum, "-o", putnum_o], check=True)
try:
    for name in families:
        base = 10_000 if name == "nesting" else FIR[name][1]
        if name == "nesting":
            # within the limit (must compile and run), then 10k and 100k deep
            dt, err = run_fas(tmp, 100)
            print(f"{name:8} n={100:<9} compile {dt:8.2f}s {'ok' if not err else 'FAIL: ' + err}", flush=True)
            fail = fail or err is not None
        if name == "lanes":
            sizes = [512, 4096]  # the lane count is the 10x size; 1x is 512 lanes
        else:
            sizes = [base, 10 * base]
        times = []
        for n in sizes:
            dt, err = run_fas(tmp, n) if name == "nesting" else run_fir(tmp, name, n)
            times.append(dt)
            print(f"{name:8} n={n:<9} compile {dt:8.2f}s {'ok' if not err else 'FAIL: ' + err}", flush=True)
            if err:
                fail = True
                break
        if len(times) == 2 and times[1] > ratio * max(times[0], 0.05) * (sizes[1] / sizes[0]) / 10:
            print(f"{name:8} FAIL: {times[1] / max(times[0], 0.05):.1f}x the time for {sizes[1] // sizes[0]}x the size")
            fail = True
finally:
    if not keep:
        shutil.rmtree(tmp, ignore_errors=True)
print("stress: " + ("FAIL" if fail else "ok"))
sys.exit(1 if fail else 0)
