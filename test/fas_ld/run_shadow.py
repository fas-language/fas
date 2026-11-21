#!/usr/bin/env python3
"""Run all OCaml linker integration tests through the fas-ld shadow wrapper.

Derives the test command list directly from src/linker/test/dune so new
integration tests are picked up automatically.
"""

from collections import Counter
import os
from pathlib import Path
import re
import subprocess
import sys


def tokenize_sexp(text: str):
    tokens = []
    i = 0
    n = len(text)
    while i < n:
        c = text[i]
        if c.isspace():
            i += 1
        elif c == ";":
            while i < n and text[i] != "\n":
                i += 1
        elif c in "()":
            tokens.append(c)
            i += 1
        elif c == '"':
            j = i + 1
            buf = []
            while j < n and text[j] != '"':
                if text[j] == "\\" and j + 1 < n:
                    j += 1
                buf.append(text[j])
                j += 1
            tokens.append("".join(buf))
            i = j + 1
        else:
            j = i
            while j < n and not text[j].isspace() and text[j] not in "();":
                j += 1
            tokens.append(text[i:j])
            i = j
    return tokens


def parse_sexps(tokens):
    pos = 0

    def parse_one():
        nonlocal pos
        tok = tokens[pos]
        if tok == "(":
            pos += 1
            items = []
            while tokens[pos] != ")":
                items.append(parse_one())
            pos += 1
            return items
        pos += 1
        return tok

    out = []
    while pos < len(tokens):
        out.append(parse_one())
    return out


def extract_runtest_commands(dune_path: Path):
    sexps = parse_sexps(tokenize_sexp(dune_path.read_text()))
    commands = []
    for form in sexps:
        if not isinstance(form, list) or not form or form[0] != "rule":
            continue
        is_runtest = False
        action = None
        for field in form[1:]:
            if isinstance(field, list) and field:
                if field[0] == "alias" and len(field) >= 2 and field[1] == "runtest":
                    is_runtest = True
                elif field[0] == "action" and len(field) >= 2:
                    action = field[1]
        if is_runtest and isinstance(action, list) and len(action) >= 2 and action[0] == "run":
            if action[1] == "python3":
                commands.append(action[1:])
    return commands


def resolve_arg(arg: str, test_src: Path, test_build: Path, shadow_ld: Path) -> str:
    if arg == "%{exe:../bin/main.exe}":
        return str(shadow_ld)
    m = re.fullmatch(r"%\{dep:([^}]+)\}", arg)
    if m:
        return str(test_src / m.group(1))
    m = re.fullmatch(r"%\{exe:([^}]+)\}", arg)
    if m:
        return str((test_build / m.group(1)).resolve())
    return arg


def main() -> int:
    if len(sys.argv) != 4:
        print("usage: run_shadow.py ROOT WORKDIR FAS_LD", file=sys.stderr)
        return 2

    root = Path(sys.argv[1]).resolve()
    workdir = Path(sys.argv[2]).resolve()
    fas_ld = Path(sys.argv[3]).resolve()
    test_src = root / "src/linker/test"
    test_build = root / "_build/default/src/linker/test"
    ocaml_ld = root / "_build/default/src/linker/bin/main.exe"
    script_dir = Path(__file__).resolve().parent
    shadow_ld = script_dir / "shadow_ld"
    norm_stderr = script_dir / "norm_stderr.py"

    if not ocaml_ld.is_file():
        print(f"fas-ld shadow: missing OCaml linker at {ocaml_ld} (run dune build)", file=sys.stderr)
        return 2

    workdir.mkdir(parents=True, exist_ok=True)
    tmp_dir = workdir / "tmp"
    tmp_dir.mkdir(exist_ok=True)
    log_path = workdir / "log"
    if log_path.exists():
        log_path.unlink()

    commands = extract_runtest_commands(test_src / "dune")
    if not commands:
        print(f"fas-ld shadow: no runtest commands found in {test_src / 'dune'}", file=sys.stderr)
        return 2

    env = dict(os.environ)
    env.pop("FAS_ORACLE", None)
    env["SHADOW_OCAML"] = str(ocaml_ld)
    env["SHADOW_FAS"] = str(fas_ld)
    env["SHADOW_LOG"] = str(log_path)
    env["SHADOW_TMP"] = str(tmp_dir)
    env["SHADOW_NORM"] = str(norm_stderr)
    env.setdefault("FAS_CC", "clang-22")
    env.setdefault("CC", "clang-22")
    env.setdefault("FAS_OPT", "opt-22")
    env.setdefault("FAS_LLC", "llc-22")
    env.setdefault("LLVM_OPT", "opt-22")
    env.setdefault("LLVM_LLC", "llc-22")

    failed_suites = []
    for raw_cmd in commands:
        cmd = [resolve_arg(a, test_src, test_build, shadow_ld) for a in raw_cmd]
        name = Path(cmd[1]).name if len(cmd) > 1 else "test"
        with open(log_path, "a") as lf:
            lf.write(f"== {name}\n")
        out_file = workdir / f"out.{name}"
        with open(out_file, "wb") as of:
            proc = subprocess.run(cmd, cwd=test_src, env=env, stdout=of, stderr=subprocess.STDOUT, timeout=900)
        if proc.returncode != 0:
            failed_suites.append((name, proc.returncode, out_file))

    counts = Counter()
    problems = []
    if log_path.exists():
        for line in log_path.read_text(errors="replace").splitlines():
            for tag in ("SAME", "DIFF", "BYTES", "REFUSED"):
                if line.startswith(tag + " "):
                    counts[tag] += 1
                    if tag != "SAME" and len(problems) < 20:
                        problems.append(line)
                    break

    print(
        f"shadow: suites={len(commands)} failed_suites={len(failed_suites)} "
        f"SAME={counts['SAME']} DIFF={counts['DIFF']} BYTES={counts['BYTES']} REFUSED={counts['REFUSED']}"
    )
    for name, rc, out_file in failed_suites:
        print(f"  suite failed: {name} (exit {rc}, see {out_file})", file=sys.stderr)
    for p in problems:
        print(f"  {p}", file=sys.stderr)

    if failed_suites or counts["DIFF"] or counts["BYTES"] or counts["REFUSED"]:
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
