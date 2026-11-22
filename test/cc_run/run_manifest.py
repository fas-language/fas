#!/usr/bin/env python3
"""Differential execution runner for fcc against clang-22 over cc-tests/manifest.json."""

import argparse
from collections import Counter
from concurrent.futures import ThreadPoolExecutor, as_completed
import json
import os
from pathlib import Path
import resource
import shutil
import subprocess
import sys
import tempfile

MEM_LIMIT_BYTES = 2 * 1024 * 1024 * 1024  # 2 GiB cap per compiler child


def _limit_as():
    resource.setrlimit(resource.RLIMIT_AS, (MEM_LIMIT_BYTES, MEM_LIMIT_BYTES))

ROOT = Path(__file__).resolve().parents[2]
CC_TESTS = ROOT.parent / "cc-tests"
UPSTREAM = CC_TESTS / "upstream"
MANIFEST = CC_TESTS / "manifest.json"
EXPECTED = Path(__file__).resolve().parent / "expected_failures.txt"

FCC = Path(os.environ.get("FCC", str(ROOT / "_build/fcc")))
CLANG = os.environ.get("CLANG", "clang-22")


def load_expected(path: Path) -> dict[str, str]:
    out = {}
    if not path.exists():
        return out
    for line in path.read_text().splitlines():
        s = line.strip()
        if not s or s.startswith("#"):
            continue
        parts = s.split(None, 1)
        out[parts[0]] = parts[1] if len(parts) > 1 else ""
    return out


def resolve_sources(entry: dict) -> list[Path]:
    res = []
    for s in entry.get("sources", []):
        if s.startswith("/upstream/"):
            res.append(UPSTREAM / s[len("/upstream/"):])
        else:
            res.append(UPSTREAM / s.lstrip("/"))
    return res


def run_one(entry: dict, tmp_root: Path, live_clang: bool, timeout: float) -> tuple[str, str, str]:
    """Returns (path, status, detail). status is 'pass' or failure category."""
    rel = entry["path"]
    srcs = resolve_sources(entry)
    if not srcs or any(not p.exists() for p in srcs):
        return (rel, "missing-source", str(srcs))

    work = Path(tempfile.mkdtemp(dir=tmp_root))
    try:
        out_bin = work / "prog"
        # Build command: pass -lm (many torture/dg tests use math functions)
        cmd = [str(FCC), "-std=gnu23", *[str(p) for p in srcs], "-lm", "-o", str(out_bin)]
        env = {
            **os.environ,
            "FCC_FIR": os.environ.get("FCC_FIR", str(ROOT / "_build/default/tools/fas_fir/fas_fir.exe")),
            "FCC_LD": os.environ.get("FCC_LD", str(ROOT / "_build/fas-ld")),
            "FCC_INCLUDE": os.environ.get("FCC_INCLUDE", str(ROOT / "cc/include")),
        }
        try:
            cp = subprocess.run(cmd, cwd=str(srcs[0].parent), env=env,
                                stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                timeout=timeout, preexec_fn=_limit_as)
        except subprocess.TimeoutExpired:
            return (rel, "compile-timeout", "fcc timed out")

        if cp.returncode != 0:
            err = (cp.stderr or cp.stdout).decode("utf-8", errors="replace").strip()
            err_lo = err.lower()
            first = err.splitlines()[0] if err else f"exit {cp.returncode}"
            if ("out of memory" in err_lo or "out_of_memory" in err_lo
                    or "cannot allocate memory" in err_lo or "bad_alloc" in err_lo
                    or cp.returncode == -9):
                cat = "compile-oom"
                first = f"OOM ({cp.returncode}): {first}"
            elif "fas-fir:" in err:
                cat = "fir-error"
                lines = [l.strip() for l in err.splitlines() if l.strip()]
                for i, line in enumerate(lines):
                    if "verifier:" in line and i + 1 < len(lines):
                        msg = lines[i + 1]
                        if ": " in msg:
                            msg = msg.split(": ", 2)[-1]
                        first = f"verifier: {msg}"
                        break
                    if "fas-fir:" in line:
                        first = line.split(":", 2)[-1].strip()
                        break
            elif "fas-ld" in err or "fcc: link failed" in err:
                cat = "link-error"
            elif cp.returncode < 0 or cp.returncode >= 128:
                cat = "fcc-crash"
                first = f"signal/exit {cp.returncode}: {first}"
            else:
                cat = "fcc-error"
            return (rel, cat, first[:180])

        try:
            rp = subprocess.run([str(out_bin)], cwd=str(work),
                                stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=timeout)
        except subprocess.TimeoutExpired:
            return (rel, "run-timeout", "program timed out")

        # Reference result: from manifest or live clang-22
        oracle = entry.get("oracles", {}).get("clang", {}).get("run")
        ref_exit = None
        ref_stdout = None
        if oracle and not live_clang and isinstance(oracle.get("stdout"), dict) and "text" in oracle["stdout"]:
            ref_exit = oracle["exit"]
            ref_stdout = oracle["stdout"]["text"].encode("utf-8")
        else:
            ref_bin = work / "ref"
            c_cmd = [CLANG, "-std=gnu23", "-O0", "-w", *[str(p) for p in srcs], "-lm", "-o", str(ref_bin)]
            rcp = subprocess.run(c_cmd, cwd=str(srcs[0].parent),
                                 stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=timeout)
            if rcp.returncode != 0:
                return (rel, "clang-fail", "reference clang failed")
            rrp = subprocess.run([str(ref_bin)], cwd=str(work),
                                 stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=timeout)
            ref_exit = rrp.returncode
            ref_stdout = rrp.stdout

        got_exit = rp.returncode if rp.returncode >= 0 else 128 + (-rp.returncode)
        if ref_exit < 0:
            ref_exit = 128 + (-ref_exit)
        if got_exit != ref_exit:
            return (rel, "exit-mismatch", f"fcc exit={got_exit}, clang exit={ref_exit}")
        if rp.stdout != ref_stdout:
            return (rel, "stdout-mismatch", f"fcc={rp.stdout[:80]!r}, clang={ref_stdout[:80]!r}")
        return (rel, "pass", "")
    finally:
        shutil.rmtree(work, ignore_errors=True)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--suite", action="append", help="Filter to suite(s): c-testsuite, gcc-torture-execute, gcc-dg")
    ap.add_argument("--jobs", type=int, default=min(16, os.cpu_count() or 4))
    ap.add_argument("--timeout", type=float, default=30.0)
    ap.add_argument("--live-clang", action="store_true", help="Always compile & run clang-22 live instead of manifest oracle")
    ap.add_argument("--show", type=int, default=25, help="Max unexpected failures to print")
    ap.add_argument("--strict", action="store_true", help="Exit non-zero on unexpected failures")
    args = ap.parse_args()

    data = json.loads(MANIFEST.read_text())
    entries = [f for f in data["files"] if f.get("agreement") == "run-agree"]
    if args.suite:
        want = set(args.suite)
        entries = [f for f in entries if f["suite"] in want]

    expected = load_expected(EXPECTED)
    tmp_root = Path(tempfile.mkdtemp(prefix="fcc_run_"))
    counts = Counter()
    by_suite = {}
    unexpected = []
    xpass = []

    try:
        with ThreadPoolExecutor(max_workers=args.jobs) as ex:
            futs = {ex.submit(run_one, e, tmp_root, args.live_clang, args.timeout): e for e in entries}
            for fut in as_completed(futs):
                e = futs[fut]
                suite = e["suite"]
                rel, status, detail = fut.result()
                by_suite.setdefault(suite, Counter())[status] += 1
                counts[status] += 1
                if status == "pass":
                    if rel in expected:
                        xpass.append(rel)
                else:
                    if rel not in expected:
                        unexpected.append((rel, status, detail))
    finally:
        shutil.rmtree(tmp_root, ignore_errors=True)

    total = len(entries)
    passed = counts["pass"]
    print(f"Total: {passed}/{total} passed ({100.0 * passed / total:.1f}%)" if total else "Total: 0")
    for suite in sorted(by_suite):
        sc = by_suite[suite]
        st = sum(sc.values())
        sp = sc["pass"]
        print(f"  {suite:22s}: {sp:4d}/{st:4d} ({100.0 * sp / st:5.1f}%)  {dict((k, v) for k, v in sc.most_common() if k != 'pass')}")
    if xpass:
        print(f"XPASS ({len(xpass)}): {xpass[:10]}")
    if unexpected:
        unexpected.sort()
        print(f"\nUnexpected failures ({len(unexpected)}), showing first {min(len(unexpected), args.show)}:")
        for rel, status, detail in unexpected[:args.show]:
            print(f"  [{status}] {rel}: {detail}")
        # Also print top failure reasons
        reason_counts = Counter((st, d[:60]) for _, st, d in unexpected)
        print("\nTop failure clusters:")
        for (st, d), n in reason_counts.most_common(15):
            print(f"  {n:4d}x [{st}] {d}")

    if args.strict and unexpected:
        sys.exit(1)


if __name__ == "__main__":
    main()
