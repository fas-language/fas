#!/usr/bin/env python3
"""Warm-cache synthetic static-link baseline; compilers are outside timed runs.

Uses wait4 for CPU time and perf_counter for caller wall time through exit.
GNU time measures peak RSS in a separate run to avoid the Python spawn parent's
inherited RSS floor. Requires Linux, GNU as/ld/time, and Python's standard library.
"""
import argparse
import datetime
import hashlib
import json
import os
from pathlib import Path
import platform
import shutil
import statistics
import subprocess
import tempfile
import time


def version(tool):
    p = subprocess.run([tool, "--version"], capture_output=True, text=True, check=True)
    return p.stdout.splitlines()[0]


def measure(command):
    with tempfile.TemporaryFile() as diagnostics:
        start = time.perf_counter()
        pid = os.posix_spawn(command[0], command, os.environ,
                             file_actions=[(os.POSIX_SPAWN_DUP2, diagnostics.fileno(), 1),
                                           (os.POSIX_SPAWN_DUP2, diagnostics.fileno(), 2)])
        _, status, usage = os.wait4(pid, 0)
        wall = time.perf_counter() - start
        if os.waitstatus_to_exitcode(status) != 0:
            diagnostics.seek(0)
            raise RuntimeError(diagnostics.read().decode(errors="replace"))
    return dict(wall_ms=wall * 1000, user_ms=usage.ru_utime * 1000,
                system_ms=usage.ru_stime * 1000)


def measure_memory(command):
    with tempfile.NamedTemporaryFile() as result:
        subprocess.run(["/usr/bin/time", "-f", "%M", "-o", result.name, *command],
                       capture_output=True, check=True, timeout=30)
        return int(Path(result.name).read_text().strip())


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("linker", type=Path, help="fas-ld executable built with dune --profile release")
    parser.add_argument("--lld", type=Path, help="explicit LLD executable for versioned installations")
    parser.add_argument("--objects", type=int, nargs="+", default=[1, 32, 256])
    parser.add_argument("--functions-per-object", type=int, default=64)
    parser.add_argument("--runs", type=int, default=7)
    parser.add_argument("--warmups", type=int, default=1)
    parser.add_argument("--archive", choices=["none", "all", "lazy", "whole"], default="none",
                        help="archive corpus: all members needed, one needed, or forced whole-archive")
    parser.add_argument("--statistics", action="store_true",
                        help="record fas-ld phase/allocation counters in a separate untimed run")
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    if min(args.objects + [args.functions_per_object, args.runs, args.warmups]) < 1:
        parser.error("counts must all be positive")
    if max(args.objects) >= 1024 and args.archive == "none":
        parser.error("initial fas-ld input limit includes one additional startup object")
    tools = [("fas-ld", str(args.linker.resolve())), ("GNU ld", shutil.which("ld"))]
    lld = str(args.lld.absolute()) if args.lld else shutil.which("ld.lld") or next((str(p) for p in Path("/usr/lib").glob("llvm-*/bin/ld.lld") if p.is_file()), None)
    if lld:
        tools.append(("LLD", lld))
    mold = shutil.which("mold")
    if mold:
        tools.append(("mold", mold))
    cpu = next((line.partition(":")[2].strip() for line in Path("/proc/cpuinfo").read_text().splitlines()
                if line.startswith("model name")), "unknown")
    report = dict(timestamp_utc=datetime.datetime.now(datetime.timezone.utc).isoformat(),
                  harness_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
                  platform=platform.platform(), cpu=cpu, available_cpus=len(os.sched_getaffinity(0)),
                  filesystem_cache="warm; no cache eviction attempted", runs=args.runs,
                  warmups=args.warmups, tool_versions={name: version(path) for name, path in tools},
                  tool_sha256={name: hashlib.sha256(Path(path).read_bytes()).hexdigest() for name, path in tools},
                  assembler_version=version(shutil.which("as")),
                  methodology="wall time includes spawn and exit; CPU from wait4; peak RSS from one separate untimed GNU time run per tool/corpus; tool default worker counts; no GC, ICF, or build ID requested; outputs are verified by execution",
                  note="Synthetic static-link corpus, not a general linker ranking. Output layout and size differ. Statistics, when requested, come from a separate untimed fas-ld run; phases can overlap.",
                  archive_mode=args.archive,
                  corpora=[])
    with tempfile.TemporaryDirectory(prefix="fas-linker-bench-") as temporary:
        root = Path(temporary)
        start = root / "start.s"
        start.write_text('.text\n.globl _start\n_start: call fn_0_0\ncmp $42,%eax\nsetne %dil\nmovzbl %dil,%edi\nmov $60,%eax\nsyscall\n.section .note.GNU-stack,"",@progbits\n')
        subprocess.run(["as", "--64", "-o", str(start.with_suffix(".o")), str(start)], check=True)
        objects = []
        for index in range(max(args.objects)):
            source = root / f"object-{index}.s"
            lines = [".text"]
            for fn in range(args.functions_per_object):
                name = f"fn_{index}_{fn}"
                lines += [f".globl {name}", f".type {name},@function", f"{name}: mov $42,%eax", "ret", f".size {name},.-{name}"]
            lines += [".data"]
            lines += [f".quad fn_{index}_{fn}+1" for fn in range(args.functions_per_object)]
            lines += ['.section .note.GNU-stack,"",@progbits']
            source.write_text("\n".join(lines) + "\n")
            obj = source.with_suffix(".o")
            subprocess.run(["as", "--64", "-o", str(obj), str(source)], check=True)
            objects.append(obj)
        report["filesystem"] = subprocess.run(["stat", "-f", "-c", "%T", str(root)], capture_output=True, text=True, check=True).stdout.strip()
        for count in args.objects:
            inputs = [start.with_suffix(".o"), *objects[:count]]
            extra_flags = []
            if args.archive != "none":
                archive = root / f"corpus-{count}.a"
                subprocess.run(["ar", "rcs", str(archive), *(str(p) for p in objects[:count])], check=True)
                if args.archive == "all":
                    roots = root / f"roots-{count}.s"
                    roots.write_text(start.read_text() + '\n.data\n' +
                                     '\n'.join(f'.quad fn_{n}_0' for n in range(count)) + '\n')
                    subprocess.run(["as", "--64", "-o", str(roots.with_suffix(".o")), str(roots)], check=True)
                    inputs = [roots.with_suffix(".o"), archive]
                else:
                    inputs = [start.with_suffix(".o"), archive]
                if args.archive == "whole":
                    extra_flags = ["--whole-archive"]
            results = {name: [] for name, _ in tools}
            commands = {name: [path, "-o", str(root / f"out-{index}"), *extra_flags, *(str(p) for p in inputs)]
                        for index, (name, path) in enumerate(tools)}
            for name, _ in tools:
                for _ in range(args.warmups):
                    measure(commands[name])
                subprocess.run([commands[name][2]], check=True, timeout=10)
            for iteration in range(args.runs):
                # Rotate tool order to reduce systematic scheduling/cache drift.
                order = tools[iteration % len(tools):] + tools[:iteration % len(tools)]
                for name, _ in order:
                    results[name].append(measure(commands[name]))
            corpus = dict(input_objects=count + 1, input_files=len(inputs),
                          expected_linked_objects=2 if args.archive == "lazy" else count + 1,
                          generated_functions=count * args.functions_per_object,
                          generated_relocations=count * args.functions_per_object + 1 +
                            (count if args.archive == "all" else 0),
                          input_bytes=sum(p.stat().st_size for p in inputs), results={})
            digest = hashlib.sha256()
            for path in inputs:
                data = path.read_bytes()
                digest.update(len(data).to_bytes(8, "little"))
                digest.update(data)
            corpus["ordered_input_sha256"] = digest.hexdigest()
            if args.statistics:
                process = subprocess.run([*commands["fas-ld"], "--stats"],
                                         capture_output=True, text=True, check=True, timeout=30)
                corpus["fas_statistics"] = json.loads(process.stderr)
            for name, samples in results.items():
                wall = [s["wall_ms"] for s in samples]
                corpus["results"][name] = dict(median_wall_ms=statistics.median(wall),
                                                min_wall_ms=min(wall), max_wall_ms=max(wall),
                                                peak_rss_kib=measure_memory(commands[name]),
                                                output_bytes=Path(commands[name][2]).stat().st_size,
                                                output_sha256=hashlib.sha256(Path(commands[name][2]).read_bytes()).hexdigest(),
                                                samples=samples)
            report["corpora"].append(corpus)
    text = json.dumps(report, indent=2) + "\n"
    if args.output:
        args.output.write_text(text)
    else:
        print(text, end="")


if __name__ == "__main__":
    main()
