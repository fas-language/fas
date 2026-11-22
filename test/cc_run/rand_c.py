#!/usr/bin/env python3
"""UB-free random C differential fuzzer: compares fcc vs clang-22 on generated programs."""

import argparse
from concurrent.futures import ThreadPoolExecutor, as_completed
import os
from pathlib import Path
import random
import shutil
import subprocess
import sys
import tempfile

ROOT = Path(__file__).resolve().parents[2]
FCC = str(Path(os.environ.get("FCC", str(ROOT / "_build/fcc"))).resolve())
CLANG = os.environ.get("CLANG", "clang-22")
ENV = {
    **os.environ,
    "FCC_FIR": str(Path(os.environ.get("FCC_FIR", str(ROOT / "_build/default/tools/fas_fir/fas_fir.exe"))).resolve()),
    "FCC_LD": str(Path(os.environ.get("FCC_LD", str(ROOT / "_build/fas-ld"))).resolve()),
    "FCC_INCLUDE": str(Path(os.environ.get("FCC_INCLUDE", str(ROOT / "cc/include"))).resolve()),
}

SAFE_HEADER = r"""#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdarg.h>

static uint32_t safe_div_u32(uint32_t a, uint32_t b) { return b ? a / b : a; }
static uint32_t safe_mod_u32(uint32_t a, uint32_t b) { return b ? a % b : a; }
static int32_t  safe_div_i32(int32_t a, int32_t b) { return (b == 0 || (a == (-2147483647 - 1) && b == -1)) ? a : a / b; }
static uint32_t safe_shl_u32(uint32_t a, uint32_t b) { return a << (b & 31u); }
static uint32_t safe_shr_u32(uint32_t a, uint32_t b) { return a >> (b & 31u); }
static uint64_t safe_shl_u64(uint64_t a, uint64_t b) { return a << (b & 63u); }
static uint64_t safe_shr_u64(uint64_t a, uint64_t b) { return a >> (b & 63u); }
"""


def gen_expr_u32(rng: random.Random, vars_u32: list[str], depth: int) -> str:
    if depth <= 0 or rng.random() < 0.35:
        if rng.random() < 0.65:
            return rng.choice(vars_u32)
        return f"{rng.randint(0, 65535)}u"
    kind = rng.choice(["bin", "bin", "cmp", "cond", "shift", "div", "unary"])
    if kind == "bin":
        op = rng.choice(["+", "-", "*", "&", "|", "^"])
        return f"({gen_expr_u32(rng, vars_u32, depth-1)} {op} {gen_expr_u32(rng, vars_u32, depth-1)})"
    if kind == "cmp":
        op = rng.choice(["==", "!=", "<", "<=", ">", ">="])
        return f"(uint32_t)({gen_expr_u32(rng, vars_u32, depth-1)} {op} {gen_expr_u32(rng, vars_u32, depth-1)})"
    if kind == "cond":
        return f"({gen_expr_u32(rng, vars_u32, depth-1)} ? {gen_expr_u32(rng, vars_u32, depth-1)} : {gen_expr_u32(rng, vars_u32, depth-1)})"
    if kind == "shift":
        fn = rng.choice(["safe_shl_u32", "safe_shr_u32"])
        return f"{fn}({gen_expr_u32(rng, vars_u32, depth-1)}, {gen_expr_u32(rng, vars_u32, depth-1)})"
    if kind == "div":
        fn = rng.choice(["safe_div_u32", "safe_mod_u32"])
        return f"{fn}({gen_expr_u32(rng, vars_u32, depth-1)}, {gen_expr_u32(rng, vars_u32, depth-1)})"
    op = rng.choice(["~", "-", "!"])
    return f"({op}({gen_expr_u32(rng, vars_u32, depth-1)}))"


def gen_program(seed: int) -> str:
    rng = random.Random(seed)
    lines = [SAFE_HEADER]

    # Struct with bitfields & nested struct
    bf1 = rng.randint(1, 7)
    bf2 = rng.randint(2, 11)
    bf3 = rng.randint(1, 13)
    lines.append(f"""
struct S {{
    uint32_t x;
    int16_t y;
    unsigned a : {bf1};
    int b : {bf2};
    unsigned c : {bf3};
    double d;
}};
static struct S g_s = {{ {rng.randint(0, 1000)}u, {rng.randint(-500, 500)}, {rng.randint(0, (1<<bf1)-1)}u, {rng.randint(-(1<<(bf2-1)), (1<<(bf2-1))-1)}, {rng.randint(0, (1<<bf3)-1)}u, {rng.randint(-100, 100)}.25 }};
static uint32_t g_arr[8] = {{ {', '.join(str(rng.randint(0, 1000)) + 'u' for _ in range(8))} }};
""")

    # Helper taking and returning struct by value
    lines.append(f"""
static struct S step_s(struct S s, uint32_t k) {{
    s.x = (s.x * 33u) ^ k;
    s.y = (int16_t)(s.y + (int16_t)(k & 31u));
    s.a = (s.a + k) & {(1<<bf1)-1}u;
    s.c = (s.c ^ k) & {(1<<bf3)-1}u;
    s.d = s.d + (double)(k & 7u) * 0.5;
    return s;
}}

static uint32_t var_mix(int n, ...) {{
    va_list ap;
    va_start(ap, n);
    uint32_t h = 2166136261u;
    for (int i = 0; i < n; i++) {{
        uint32_t v = va_arg(ap, uint32_t);
        h = (h ^ v) * 16777619u;
    }}
    va_end(ap);
    return h;
}}
""")

    # Main with local vars, loops, switch, pointers, bitfields, floats, varargs
    vars_u32 = ["v0", "v1", "v2", "v3", "g_arr[0]", "g_arr[3]", "s.x"]
    lines.append("int main(void) {")
    for i in range(4):
        lines.append(f"    uint32_t v{i} = {rng.randint(1, 100000)}u;")
    lines.append("    struct S s = g_s;")
    lines.append("    uint32_t *p = g_arr;")

    for _ in range(rng.randint(5, 10)):
        stmt_kind = rng.choice(["assign", "if", "for", "switch", "call", "ptr"])
        if stmt_kind == "assign":
            lhs = rng.choice(["v0", "v1", "v2", "v3"])
            op = rng.choice(["=", "+=", "-=", "^=", "|="])
            lines.append(f"    {lhs} {op} {gen_expr_u32(rng, vars_u32, 3)};")
        elif stmt_kind == "if":
            cond = gen_expr_u32(rng, vars_u32, 2)
            l1, l2 = rng.choice(["v0", "v1"]), rng.choice(["v2", "v3"])
            lines.append(f"    if ({cond} & 1u) {{ {l1} ^= {gen_expr_u32(rng, vars_u32, 2)}; }} else {{ {l2} += {gen_expr_u32(rng, vars_u32, 2)}; }}")
        elif stmt_kind == "for":
            n = rng.randint(2, 6)
            lhs = rng.choice(["v0", "v1", "v2", "v3"])
            lines.append(f"    for (int i = 0; i < {n}; i++) {{ {lhs} = ({lhs} * 3u) + g_arr[i & 7]; g_arr[(i + 1) & 7] ^= {lhs}; }}")
        elif stmt_kind == "switch":
            lhs = rng.choice(["v0", "v1", "v2", "v3"])
            lines.append(f"    switch ({lhs} & 7u) {{")
            for c in range(4):
                lines.append(f"        case {c}: {lhs} += {rng.randint(1, 99)}u; break;")
            lines.append(f"        default: {lhs} ^= {rng.randint(1, 999)}u; break;")
            lines.append("    }")
        elif stmt_kind == "call":
            lines.append(f"    s = step_s(s, {gen_expr_u32(rng, vars_u32, 2)});")
        elif stmt_kind == "ptr":
            idx = rng.randint(0, 7)
            lines.append(f"    *(p + {idx}) ^= {gen_expr_u32(rng, vars_u32, 2)};")

    lines.append("    uint32_t vm = var_mix(4, v0, v1, v2, v3);")
    lines.append('    printf("%u %u %u %u %u %d %u %d %u %.2f %u\\n", v0, v1, v2, v3, s.x, (int)s.y, s.a, s.b, s.c, s.d, vm);')
    lines.append("    return 0;")
    lines.append("}")
    return "\n".join(lines)


def run_seed(seed: int, tmp_root: Path) -> tuple[int, bool, str]:
    work = Path(tempfile.mkdtemp(dir=tmp_root))
    try:
        src = work / f"r_{seed}.c"
        src.write_text(gen_program(seed))
        b_fcc = work / "fcc_prog"
        b_ref = work / "ref_prog"
        c1 = subprocess.run([FCC, "-std=gnu23", str(src), "-o", str(b_fcc)], env=ENV,
                            stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=15)
        if c1.returncode != 0:
            return (seed, False, f"fcc compile failed: {c1.stderr.decode('utf-8', 'replace')[:120]}")
        c2 = subprocess.run([CLANG, "-std=gnu23", "-O0", "-w", str(src), "-o", str(b_ref)],
                            stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=15)
        if c2.returncode != 0:
            return (seed, False, f"clang compile failed: {c2.stderr.decode('utf-8', 'replace')[:120]}")
        r1 = subprocess.run([str(b_fcc)], stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=5)
        r2 = subprocess.run([str(b_ref)], stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=5)
        if r1.returncode != r2.returncode or r1.stdout != r2.stdout:
            return (seed, False, f"mismatch: fcc=({r1.returncode}, {r1.stdout!r}) vs clang=({r2.returncode}, {r2.stdout!r})")
        return (seed, True, "")
    finally:
        shutil.rmtree(work, ignore_errors=True)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--count", type=int, default=100)
    ap.add_argument("--seed", type=int, default=1)
    ap.add_argument("--jobs", type=int, default=min(16, os.cpu_count() or 4))
    args = ap.parse_args()

    tmp_root = Path(tempfile.mkdtemp(prefix="fcc_rand_"))
    passed = 0
    fails = []
    try:
        with ThreadPoolExecutor(max_workers=args.jobs) as ex:
            futs = [ex.submit(run_seed, args.seed + i, tmp_root) for i in range(args.count)]
            for fut in as_completed(futs):
                seed, ok, detail = fut.result()
                if ok:
                    passed += 1
                else:
                    fails.append((seed, detail))
    finally:
        shutil.rmtree(tmp_root, ignore_errors=True)

    print(f"Random C differential: {passed}/{args.count} passed ({100.0 * passed / args.count:.1f}%)")
    if fails:
        for seed, d in sorted(fails)[:10]:
            print(f"  FAIL seed={seed}: {d}")
        sys.exit(1)


if __name__ == "__main__":
    main()
