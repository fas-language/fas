#!/usr/bin/env python3
"""Record reference build and run timings for later Fas comparisons."""

import argparse
from dataclasses import asdict, dataclass
import glob
import json
import os
from pathlib import Path
import platform
import statistics
import subprocess
import tempfile
import time

from refcache import Cache, Invocation, dependency_words, digest, encode, fingerprint, relevant_env

CALIBRATION = b"""#include <stdint.h>
int main(void) {
    volatile uint64_t x = 1;
    for (unsigned i = 0; i < 10000000; ++i) x = x * 6364136223846793005ULL + 1;
    return !x;
}
"""


def read_optional(path):
    try:
        return Path(path).read_text().strip()
    except OSError:
        return None


def machine():
    cpuinfo = Path("/proc/cpuinfo").read_text().split("\n\n")
    cpus = []
    for cpu in cpuinfo:
        fields = dict(line.split(":", 1) for line in cpu.splitlines() if ":" in line)
        keys = {key.strip(): value.strip() for key, value in fields.items()}
        if "processor" in keys:
            cpus.append([keys.get(k) for k in ("model name", "microcode")])
    governor = "/sys/devices/system/cpu/cpufreq/policy*/scaling_governor"
    return dict(cpu=cpus, affinity=sorted(os.sched_getaffinity(0)),
                governor={path: read_optional(path) for path in sorted(glob.glob(governor))},
                kernel=platform.release(), glibc=os.confstr("CS_GNU_LIBC_VERSION"),
                smt=read_optional("/sys/devices/system/cpu/smt/active"))


@dataclass
class Timing:
    median: float
    mad: float


def measure(command):
    start = time.perf_counter_ns()
    subprocess.run(command, check=True, stdin=subprocess.DEVNULL,
                   stdout=subprocess.DEVNULL, stderr=subprocess.PIPE, timeout=600)
    return (time.perf_counter_ns() - start) / 1e9


def summary(samples):
    median = statistics.median(samples)
    return asdict(Timing(median, statistics.median(abs(x - median) for x in samples)))


def calibration(compiler, scratch, repeats):
    source = scratch / "calibration.c"
    output = scratch / "calibration"
    source.write_bytes(CALIBRATION)
    measure([compiler, "-O2", str(source), "-o", str(output)])
    return summary([measure([str(output)]) for _ in range(repeats)])


def record(cache, program, compiler, flags, repeats, scratch):
    program = str(Path(program).resolve())
    invocation = Invocation("bench", (compiler,), cwd=os.getcwd(), c=True)
    identity = cache.identity(invocation, os.environ)
    dep = subprocess.check_output([compiler, *flags, "-M", "-MT", "refcache", program])
    paths = sorted(set(dependency_words(os.fsdecode(dep))))
    inputs = {p: fingerprint(Path(p))["sha"] for p in paths}
    hardware = machine()
    key = digest(encode([program, inputs, compiler, flags, hardware, identity,
                         relevant_env(os.environ), digest(CALIBRATION)]))
    entry = cache.root / "bench" / key
    try:
        return cache.read_json(entry)
    except (OSError, ValueError):
        pass
    output = str(scratch / "program")
    builds, runs = [], []
    for _ in range(repeats):
        Path(output).unlink(missing_ok=True)
        builds.append(measure([compiler, *flags, program, "-o", output]))
        runs.append(measure([output]))
    record = dict(key=key, program=program, compiler=compiler, flags=flags,
                  repeats=repeats, machine=hardware, identity=identity,
                  build=summary(builds), run=summary(runs),
                  calibration=calibration(compiler, scratch, repeats))
    cache.write_json(entry, record)
    return record


def compare(cache, fresh, scratch, threshold, repeats):
    report = []
    calibrations = {}
    current_machine = machine()
    for result in fresh:
        key = result["reference_key"]
        if len(key) != 64 or any(c not in "0123456789abcdef" for c in key):
            raise ValueError("invalid reference key")
        entry = cache.read_json(cache.root / "bench" / key)
        compiler = entry["compiler"]
        inv = Invocation("bench", (compiler,), cwd=os.getcwd(), c=True)
        if entry["machine"] != current_machine or entry["identity"] != cache.identity(inv, os.environ):
            raise ValueError(f"{entry['program']}: reference machine or compiler changed")
        if compiler not in calibrations:
            calibrations[compiler] = calibration(compiler, scratch, repeats)
        now = calibrations[compiler]
        old = entry["calibration"]
        difference = abs(now["median"] - old["median"])
        margin = max(old["median"] * threshold, 3 * (old["mad"] + now["mad"]))
        report.append(dict(reference_key=key, program=entry["program"], compiler=compiler,
                           flags=entry["flags"], reference_build=entry["build"], reference_run=entry["run"],
                           fas_build=result["build"], fas_run=result["run"],
                           build_ratio=result["build"]["median"] / entry["build"]["median"],
                           run_ratio=result["run"]["median"] / entry["run"]["median"],
                           calibration_reference=old, calibration_current=now,
                           drift=difference > margin))
    return report


def self_test(cache, scratch, compiler):
    programs = []
    for i in range(2):
        path = scratch / f"toy{i}.c"
        path.write_text(f"int main(void) {{ return {i} - {i}; }}\n")
        first = record(cache, path, compiler, ["-O1"], 2, scratch)
        second = record(cache, path, compiler, ["-O1"], 2, scratch)
        assert first == second and first["repeats"] == 2
        assert first["build"]["median"] > 0 and first["run"]["median"] > 0
        programs.append(dict(reference_key=first["key"], build=first["build"], run=first["run"]))
    assert programs[0]["reference_key"] != programs[1]["reference_key"]
    results = compare(cache, programs, scratch, 10, 2)
    assert [row["run_ratio"] for row in results] == [1, 1]
    print("benchmark self-test: 2 programs, 2 repeats, cache reuse and comparison passed")


def main():
    p = argparse.ArgumentParser(description=__doc__)
    sub = p.add_subparsers(dest="action", required=True)
    rec = sub.add_parser("record")
    rec.add_argument("program", nargs="+")
    rec.add_argument("--compiler", action="append")
    rec.add_argument("--opt", action="append", choices=("1", "2", "3"))
    rec.add_argument("--flag", action="append", default=[])
    rec.add_argument("--repeats", type=int, default=5)
    comp = sub.add_parser("compare")
    comp.add_argument("fresh", type=Path)
    comp.add_argument("--drift", type=float, default=0.15)
    comp.add_argument("--repeats", type=int, default=5)
    test = sub.add_parser("self-test")
    test.add_argument("--compiler", default="gcc")
    args = p.parse_args()
    if getattr(args, "repeats", 1) < 1:
        p.error("--repeats must be positive")
    if getattr(args, "drift", 0) < 0:
        p.error("--drift must be nonnegative")
    cache = Cache()
    if cache.root is None:
        p.error("benchmark storage requires FAS_REFCACHE")
    cache.root.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix="bench-", dir=cache.root) as d:
        scratch = Path(d)
        if args.action == "self-test":
            isolated = Cache(scratch / "test-cache")
            with isolated.lock(exclusive=True):
                self_test(isolated, scratch, args.compiler)
            return 0
        try:
            with cache.lock(exclusive=args.action == "record"):
                if args.action == "record":
                    result = [record(cache, program, compiler, ["-O" + level, *args.flag], args.repeats, scratch)
                              for program in args.program
                              for compiler in args.compiler or ["gcc", "clang-22"]
                              for level in args.opt or ["1", "2", "3"]]
                else:
                    result = compare(cache, json.loads(args.fresh.read_text()), scratch, args.drift, args.repeats)
        except (OSError, KeyError, ValueError, subprocess.SubprocessError) as exc:
            p.error(str(exc))
        print(json.dumps(result, indent=2))
        return int(args.action == "compare" and any(row["drift"] for row in result))


if __name__ == "__main__":
    raise SystemExit(main())
