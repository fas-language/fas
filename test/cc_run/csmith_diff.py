#!/usr/bin/env python3
"""Run the pinned cc-tests Csmith against fcc and Clang at -O0."""

import argparse
from collections import Counter
from concurrent.futures import ThreadPoolExecutor
import os
from pathlib import Path
import resource
import shlex
import shutil
import signal
import subprocess
import tempfile
import time

from rand_c import CLANG, ENV, FCC, ROOT


def run_case(seed, csmith, include, scratch, failures):
    with tempfile.TemporaryDirectory(prefix="fcc_csmith_", dir=scratch) as tmp:
        work = Path(tmp)
        src = work / "case.c"
        commands = [
            ("generate", [csmith, "--seed", str(seed), "--concise", "-o", str(src)], 10),
            ("clang-link", [CLANG, "-std=gnu23", "-O0", "-w", "-I", include, src.name, "-lm", "-o", "clang.out"], 30),
            ("clang-run", ["./clang.out"], 5),
            ("fcc-link", [FCC, "-std=gnu23", "-O0", "-w", "-I", include, src.name, "-lm", "-o", "fcc.out"], 30),
            ("fcc-run", ["./fcc.out"], 5),
        ]
        status = "same"
        results = {}
        for name, cmd, limit in commands:
            try:
                with (work / (name + ".stdout")).open("wb") as stdout, (work / (name + ".stderr")).open("wb") as stderr:
                    with subprocess.Popen(cmd, cwd=work, env=ENV, stdout=stdout, stderr=stderr, start_new_session=True) as process:
                        try:
                            results[name] = process.wait(timeout=limit)
                        except subprocess.TimeoutExpired:
                            os.killpg(process.pid, signal.SIGKILL)
                            raise
            except subprocess.TimeoutExpired:
                results[name] = "timeout"
                if name == "clang-run":
                    continue
                if name == "fcc-run" and results["clang-run"] == "timeout":
                    status = "both-run-timeout"
                    break
                status = name + "-timeout"
                break
            if results[name]:
                status = name + "-failure"
                break
        if status == "same":
            if results["clang-run"] == "timeout":
                status = "clang-run-timeout"
            elif (work / "clang-run.stdout").read_bytes() != (work / "fcc-run.stdout").read_bytes():
                status = "stdout"
            elif (work / "clang-run.stderr").read_bytes() != (work / "fcc-run.stderr").read_bytes():
                status = "stderr"
        if status != "same":
            dest = failures / str(seed)
            dest.mkdir()
            if src.exists():
                shutil.copyfile(src, dest / src.name)
            for p in (*work.glob("*.stdout"), *work.glob("*.stderr")):
                shutil.copyfile(p, dest / p.name)
            log = [f"{seed}: {status}"]
            log.extend(f"{name}: {shlex.join(cmd).replace(str(work) + '/', '')} (exit {results.get(name)})"
                       for name, cmd, _ in commands)
            (dest / "commands.txt").write_text("\n".join(log) + "\n")
        return status


def main():
    pinned = ROOT.parent / "cc-tests" / "tools"
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--csmith", type=Path, default=pinned / "bin/csmith")
    ap.add_argument("--include", type=Path, default=pinned / "include")
    ap.add_argument("--count", type=int, default=1000)
    ap.add_argument("--seed", type=int, default=1)
    ap.add_argument("--jobs", type=int, default=min(6, os.cpu_count() or 1))
    ap.add_argument("--seconds", type=int, default=1800, help="stop starting cases after this many seconds")
    ap.add_argument("--failures", type=Path, required=True, help="new directory for failing case reproducers")
    args = ap.parse_args()
    if min(args.count, args.jobs, args.seconds) <= 0:
        ap.error("count, jobs and seconds must be positive")
    csmith, include = str(args.csmith.resolve()), str(args.include.resolve())
    if not Path(csmith).is_file() or not (Path(include) / "csmith.h").is_file():
        ap.error("Csmith and csmith.h are required (see ../cc-tests/fetch.sh)")
    maximum = 256 * 1024 * 1024
    soft, hard = resource.getrlimit(resource.RLIMIT_FSIZE)
    if soft == resource.RLIM_INFINITY or soft > maximum:
        resource.setrlimit(resource.RLIMIT_FSIZE, (maximum, hard))
    resource.setrlimit(resource.RLIMIT_CORE, (0, 0))
    args.failures.mkdir(parents=True)
    deadline = time.monotonic() + args.seconds
    seeds = iter(range(args.seed, args.seed + args.count))

    def worker():
        counts = Counter()
        while time.monotonic() < deadline:
            seed = next(seeds, None)
            if seed is None:
                break
            status = run_case(seed, csmith, include, args.failures.parent, args.failures)
            counts[status] += 1
            if status != "same":
                print(f"seed={seed}: {status}", flush=True)
        return counts

    with ThreadPoolExecutor(max_workers=args.jobs) as pool:
        workers = [pool.submit(worker) for _ in range(args.jobs)]
        counts = sum((future.result() for future in workers), Counter())
    total = counts.total()
    print(f"Csmith differential: {counts['same']}/{total} passed ({args.count - total} unrun within budget)")
    for status, count in sorted(counts.items()):
        print(f"  {status}: {count}")
    return int(total != counts["same"] + counts["both-run-timeout"] or counts["same"] == 0)


if __name__ == "__main__":
    raise SystemExit(main())
