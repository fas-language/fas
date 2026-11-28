#!/usr/bin/env python3
"""Check front-end acceptance against the frozen corpus and live Clang."""
import argparse
from collections import Counter, defaultdict
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass
import json
import os
from pathlib import Path
import random
import re
import runpy
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[2]
TESTS = Path(__file__).resolve().parent
CLANG = os.environ.get("FAS_CC", "clang-22")


@dataclass(frozen=True)
class Case:
    group: str
    name: str
    path: str
    expect: int | None
    source: bytes | None = None


def run(exe, path, source=None, extra=()):
    return subprocess.run([exe, "-std=gnu23", "-fsyntax-only", *extra, path],
                          input=source, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                          cwd=ROOT, timeout=40)


def cases(corpus, suites):
    if "directed" in suites:
        for path in sorted(TESTS.glob("*.c")):
            yield Case("directed", path.name, str(path), int(path.name.startswith("reject_")))
    if "manifest" in suites:
        for row in json.loads((corpus / "manifest.json").read_text())["files"]:
            if row["role"] != "test":
                continue
            kind, name = row["category"], row["path"]
            if kind == "diagnostic-only" and all(row["oracles"][cc]["compile_exit"] == 1 for cc in ("gcc", "clang")):
                expect = 1
            elif kind in {"compile-only", "run"} and not row["needs_non_c23"] and all(
                row["oracles"][cc]["compile_ok"] for cc in ("gcc", "clang")
            ):
                expect = 0
            else:
                continue
            yield Case(kind, name, str(corpus / "upstream" / name), expect)
    if "headers" in suites:
        system = Path("/usr/include")
        for path in sorted(system.rglob("*.h")):
            if path.is_file():
                yield Case("headers", str(path.relative_to(system)), "-", None,
                           f'#include "{path}"\n'.encode())
    if "projects" in suites:
        for group, pattern in (("sqlite", "sqlite3.c"), ("lua", "src/*.c"), ("zlib", "*.c")):
            for path in sorted((corpus / "upstream" / group).glob(pattern)):
                yield Case(group, path.name, str(path), None)


def check(fcc, case):
    checking_oracle = case.expect is None
    try:
        if checking_oracle:
            flags = ("-ferror-limit=1",)
            if case.source is not None:
                flags += ("-x", "c")
            ref = run(CLANG, case.path, case.source, flags)
            if ref.returncode != 0:
                return "clang-skip", ref.stderr.decode(errors="replace")
            checking_oracle = False
        got = run(fcc, case.path, case.source)
        if got.returncode == 0:
            return ("accepted" if not case.expect else "unexpected-accept"), ""
        detail = got.stderr.decode(errors="replace")
        if got.returncode != 1 or "error:" not in detail:
            return "process-error", f"exit {got.returncode}\n{detail}"
        return ("rejected" if case.expect == 1 else "unexpected-reject"), detail
    except subprocess.TimeoutExpired as timeout:
        return ("oracle-timeout" if checking_oracle else "timeout"), str(timeout.cmd[0])


def constant_cases(seed, count):
    rng = random.Random(seed)
    for i in range(count):
        a = rng.randrange(2**32)
        b = rng.randrange(2**32)
        c = rng.randrange(1, 2**32)
        shift = rng.randrange(32)
        op = rng.choice(("+", "-", "*", "^", "|", "&", "/", "%", "<<", ">>"))
        if op == "+": value = a + b
        elif op == "-": value = a - b
        elif op == "*": value = a * b
        elif op == "^": value = a ^ b
        elif op == "|": value = a | b
        elif op == "&": value = a & b
        elif op == "/": value = a // c; b = c
        elif op == "%": value = a % c; b = c
        elif op == "<<": value = a << shift; b = shift
        else: value = a >> shift; b = shift
        value &= 0xffffffff
        # Both sides of the outer comparison are compile-time constants.
        # The dead branches deliberately contain an invalid integer operation.
        yield f"static_assert(((({a}u {op} {b}u) == {value}u) ? 1 : (1 / 0)));"
        if value < 2**31:
            yield f"enum {{ enum_{i} = {a}u {op} {b}u }}; static_assert(enum_{i} == {value}u);"

    for i in range(max(1, count // 4)):
        width = rng.randrange(65, 194)
        mask = (1 << width) - 1
        a = rng.getrandbits(width)
        b = rng.getrandbits(width - 9) or 1
        shift = rng.randrange(width)
        yield f"typedef unsigned _BitInt({width}) W{i};"
        for operator, rhs, result in (
            ("+", b, a + b), ("-", b, a - b), ("*", b, a * b),
            ("/", b, a // b), ("%", b, a % b),
            ("<<", shift, a << shift), (">>", shift, a >> shift),
        ):
            yield f"static_assert(((W{i}){a}uwb {operator} (W{i}){rhs}uwb) == {result & mask}uwb);"
        # Stay within range on the signed side, then exercise sign extension.
        a = rng.randrange(-(1 << 61), 1 << 61)
        b = rng.randrange(-(1 << 61), 1 << 61)
        yield f"static_assert(((_BitInt({width})){a}LL + {b}LL) == {a + b}LL);"


def extra_constants(fcc, seed, count):
    lines = list(constant_cases(seed, count))
    source = ("\n".join(lines) + "\n").encode()
    ref = run(CLANG, "-", source, ("-x", "c"))
    got = run(fcc, "-", source)
    if ref.returncode or got.returncode:
        raise SystemExit(f"integer constant differential failed with seed {seed}\n"
                         f"clang: {ref.stderr.decode()}\nfcc: {got.stderr.decode()}")
    assertions = sum("static_assert(" in line for line in lines)
    print(f"Integer constant differential: {assertions} assertions passed (seed {seed})", flush=True)


def extra_driver(fcc):
    with tempfile.TemporaryDirectory() as scratch:
        work = Path(scratch)
        header = work / "forced.h"
        source = work / "input.c"
        dump = work / "ast.txt"
        header.write_text("typedef int Item;\n")
        source.write_text("Item pending[];\nint fun(int n) { Item a[n]; return sizeof a; }\n")
        result = run(fcc, str(source), extra=("-include", str(header), "-Xfcc-dump-ast", "-o", str(dump)))
        if result.returncode:
            raise SystemExit(result.stderr.decode())
        roots = [line for line in dump.read_text().splitlines() if not line.startswith(" ")]
        if len(roots) != 2 or not roots[0].endswith(" fun") or not roots[1].endswith(" pending"):
            raise SystemExit(f"unexpected AST callback order: {roots}")
        if not any(line.lstrip().startswith("size ") and " runtime" in line for line in dump.read_text().splitlines()):
            raise SystemExit("AST dump is missing the runtime VLA size")
        result = run(fcc, str(TESTS / "floating_literals.c"), extra=("-Xfcc-dump-ast", "-o", str(dump)))
        if result.returncode:
            raise SystemExit(result.stderr.decode())
        bits = [line.rsplit(" bits=0x", 1)[1] for line in dump.read_text().splitlines() if " bits=0x" in line]
        if bits != ["7ffeffffffffffffffff", "00000000000000000001", "3fff8000000000000001"]:
            raise SystemExit(f"incorrect long double literal bits: {bits}")
    print("Driver checks: forced include, function/object callbacks, runtime VLA size, x87 literal bits", flush=True)


def extra_layout(fcc, seed, count):
    # Compare through the parser as well as through the standalone layout probe.
    # Clang supplies constants in a tiny host program that includes the declarations.
    layout = runpy.run_path(str(ROOT / "test/cc_types/layout_fuzz.py"))
    records = layout["gen_cases"](random.Random(seed), count)
    with tempfile.TemporaryDirectory() as scratch:
        work = Path(scratch)
        source = work / "layout.c"
        executable = work / "layout"
        header = work / "records.h"
        declarations, checks = render_layout(records)
        header.write_text(declarations + """\
#include <time.h>
#include <netinet/in.h>
#include <sys/epoll.h>
#include <sys/stat.h>
#include <sys/uio.h>
""")
        for record, field in (
            ("timespec", "tv_nsec"), ("sockaddr_in6", "sin6_addr"),
            ("epoll_event", "data"), ("stat", "st_mtim"), ("iovec", "iov_len"),
        ):
            checks.extend((f"sizeof(struct {record})", f"alignof(struct {record})",
                           f"offsetof(struct {record}, {field})"))
        source.write_text('#include "records.h"\n#include <stdio.h>\nint main(void) {\n' +
                          "\n".join(f'printf("%zu\\n", (size_t)({c}));' for c in checks) + "\n}\n")
        subprocess.run([CLANG, "-std=gnu23", str(source), "-o", str(executable)],
                       check=True, capture_output=True, timeout=40)
        values = subprocess.check_output([executable], timeout=10).decode().splitlines()
        source.write_text('#include "records.h"\n' + "\n".join(
            f"static_assert({c} == {value});" for c, value in zip(checks, values, strict=True)) + "\n")
        actual = run(fcc, str(source))
        if actual.returncode:
            raise SystemExit(f"layout differential failed with seed {seed}\n{actual.stderr.decode()}")
    print(f"Layout differential: {len(checks)} assertions passed across {count} generated and 5 system records (seed {seed})", flush=True)


def render_layout(records):
    parts = ["#include <stddef.h>", "typedef typeof(nullptr) nullptr_t;"]
    checks = []
    for i, rec in enumerate(records):
        kind = "union" if rec["is_union"] else "struct"
        ty = f"{kind} R{i}"
        attrs = (["packed"] if rec["packed"] else []) + (
            [f'aligned({rec["align"]})'] if rec["align"] else [])
        if rec["pragma_pack"]:
            parts.append(f'#pragma pack(push, {rec["pragma_pack"]})')
        attribute = f' __attribute__(({", ".join(attrs)}))' if attrs else ""
        parts.append(f"typedef {kind}{attribute} R{i} {{")
        for j, f in enumerate(rec["fields"]):
            attrs = (["packed"] if f["packed"] else []) + (
                [f'aligned({f["align"]})'] if f["align"] else [])
            attribute = f' __attribute__(({", ".join(attrs)}))' if attrs else ""
            name = f"f{j}"
            if f["is_bf"]:
                if f["bw"] == 0:
                    parts.append(f'    {f["c_ty"]} : 0;')
                else:
                    parts.append(f'    {f["c_ty"]} {name} : {f["bw"]}{attribute};')
            else:
                if f["is_flex"]:
                    suffix = "[]"
                elif f["is_arr"]:
                    suffix = f'[{f["arr_len"]}]'
                else:
                    suffix = ""
                parts.append(f'    {f["c_ty"]} {name}{suffix}{attribute};')
                checks.append(f"offsetof({ty}, {name})")
        parts.append(f"}} R{i};")
        if rec["pragma_pack"]:
            parts.append("#pragma pack(pop)")
        checks.extend((f"sizeof({ty})", f"alignof({ty})"))
    return "\n".join(parts) + "\n", checks


def expectations(path):
    result = {}
    for number, line in enumerate(path.read_text().splitlines(), 1):
        if not line.strip() or line.lstrip().startswith("#"):
            continue
        status, case, reason = line.split(maxsplit=2)
        if status not in {"unexpected-reject", "unexpected-accept", "oracle-timeout"} or case in result:
            raise ValueError(f"{path}:{number}: invalid or duplicate expectation")
        result[case] = status, reason
    return result


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("fcc")
    ap.add_argument("--cc-tests", type=Path, default=ROOT.parent / "cc-tests")
    ap.add_argument("--suite", action="append", choices=("directed", "manifest", "headers", "projects", "constants", "layout"))
    ap.add_argument("--jobs", type=int, default=min(12, os.cpu_count() or 1))
    ap.add_argument("--filter", default="")
    ap.add_argument("--json", type=Path)
    ap.add_argument("--seed", type=int, default=23109)
    ap.add_argument("--count", type=int, default=500)
    ap.add_argument("--expected", type=Path, default=TESTS / "expected_diffs.txt")
    opt = ap.parse_args()
    suites = opt.suite or ("directed", "manifest", "headers", "projects", "constants", "layout")
    expected = expectations(opt.expected)
    selected = [c for c in cases(opt.cc_tests.resolve(), suites) if re.search(opt.filter, c.name)]
    counts = defaultdict(Counter)
    records = []
    unexpected = 0
    with ThreadPoolExecutor(max_workers=opt.jobs) as pool:
        for i, (case, (status, detail)) in enumerate(zip(selected, pool.map(lambda c: check(opt.fcc, c), selected)), 1):
            key = f"{case.group}/{case.name}"
            entry = expected.get(key)
            reason = entry[1] if entry and entry[0] == status else None
            counts[case.group][status] += 1
            if status not in {"accepted", "rejected", "clang-skip"} and not reason:
                unexpected += 1
                if counts[case.group][status] <= 8:
                    print(f"{key}: {status}: {detail.splitlines()[0] if detail else ''}", flush=True)
            record = dict(group=case.group, name=case.name, status=status)
            if detail and status not in {"accepted", "rejected", "clang-skip"}:
                record["diagnostic"] = detail
            if reason:
                record["expected_reason"] = reason
            records.append(record)
            if i % 1000 == 0:
                print(f"Checked {i}/{len(selected)} inputs", flush=True)
    for group, count in counts.items():
        print(f"{group}: {dict(count)}", flush=True)
    print(f"Unexpected differences: {unexpected}", flush=True)
    if opt.json:
        opt.json.write_text(json.dumps(dict(counts=counts, cases=records), indent=2) + "\n")
    if "directed" in suites and not opt.filter:
        extra_driver(opt.fcc)
    if "constants" in suites:
        extra_constants(opt.fcc, opt.seed, opt.count)
    if "layout" in suites:
        extra_layout(opt.fcc, opt.seed, opt.count)
    return bool(unexpected)


if __name__ == "__main__":
    raise SystemExit(main())
