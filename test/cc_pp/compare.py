#!/usr/bin/env python3
"""Compare the spelling of C preprocessing tokens, including line pragmas."""

import argparse
from collections import Counter
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import tempfile

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "tools/refcache"))
from refcache import run as cached_run

TESTS = Path(__file__).resolve().parent
ORACLE = os.environ.get("CLANG", "clang-22")
ENV = {**os.environ, "LC_ALL": "C", "SOURCE_DATE_EPOCH": "0"}
UCN = rb"\\(?:u[0-9a-fA-F]{4}|U[0-9a-fA-F]{8})"
ID = rb"(?:[a-zA-Z_$\x80-\xff]|" + UCN + rb")"
TOKEN = re.compile(
    rb"(?://[^\n]*|/\*[\s\S]*?\*/)|"
    rb"((?:u8|[LuU])?(?:\"(?:[^\"\\\n]|\\.)*\"|'(?:[^'\\\n]|\\.)*')|"
    rb"(?:[0-9]|\.[0-9])(?:[eEpP][+-]|[0-9.]|'[0-9a-zA-Z_]|" + ID + rb")*|"
    + ID + rb"(?:" + ID + rb"|[0-9])*|"
    rb"%:%:|\.\.\.|<<=|>>=|->|\+\+|--|<<|>>|&&|\|\||"
    rb"[+*/%<>=!&|^-]=|##|::|<:|:>|<%|%>|%:|\S)"
)
UCN_RE = re.compile(UCN)
DIRECTIVES = re.compile(rb"^[ \t]*#[ \t]*(?:pragma\b|ident\b).*\n?", re.M)


def decode_ucn(match):
    cp = int(match[0][2:], 16)
    if cp > 0x10FFFF or 0xD800 <= cp <= 0xDFFF:
        return match[0]
    return chr(cp).encode()


def identifier_pieces(word):
    """Separate Unicode characters outside C23's identifier classes."""
    text = word.decode("utf-8", errors="surrogateescape")
    start = 0
    while start < len(text):
        end = start + 1
        if text[start].isidentifier() or text[start] == "$":
            while end < len(text) and (("_" + text[end]).isidentifier() or text[end] == "$"):
                end += 1
        yield text[start:end].encode("utf-8", errors="surrogateescape")
        start = end


def tokens(source):
    # A pragma's line boundary matters even though normal C whitespace does not.
    source = DIRECTIVES.sub(lambda m: m[0].rstrip() + b"\x01\n", source)
    # Clang canonicalizes universal characters in identifiers in its text output.
    # Limit that canonicalization to identifiers so string spellings stay exact.
    result = []
    for match in TOKEN.finditer(source):
        word = match[1]
        if word is None:
            continue
        if any(c in word for c in (b'"', b"'")):
            result.append(word)
            continue
        if b"\\" in word:
            word = UCN_RE.sub(decode_ucn, word)
        if not word.isascii() and not word[:1].isdigit() and not word.startswith(b"."):
            result.extend(identifier_pieces(word))
        else:
            result.append(word)
    return result


@dataclass(frozen=True)
class Case:
    category: str
    name: str
    path: str
    args: tuple = ()
    source: bytes | None = None


def run(command, source=None):
    return subprocess.run(
        command, input=source, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
        cwd=ROOT, env=ENV, timeout=45, check=False,
    )


def preprocess(command, case, *, deps=(), scratch=None):
    inputs = (*deps, case.path) if case.path != "-" else deps
    return cached_run(command, ns="cc-pp", deps=inputs, scratch=scratch,
                      c=True, input=case.source, cwd=ROOT, env=ENV, timeout=45)


def test_case(fcc, case, normalized, shared_headers, defs):
    args = ["-std=gnu23", "-E", "-P", *case.args, case.path]
    try:
        expected = preprocess([ORACLE, *args], case)
        if expected.returncode:
            return case, "clang-skip", expected.stderr.decode(errors="replace")[:1000]
        reference = tokens(expected.stdout)
        actual = run([fcc, *args], case.source)
        if actual.returncode:
            status = "fcc-error"
            detail = actual.stderr.decode(errors="replace")[:1000]
        else:
            observed = tokens(actual.stdout)
            if observed == reference:
                return case, "same", ""
            i = next((i for i, (a, b) in enumerate(zip(observed, reference)) if a != b),
                     min(len(observed), len(reference)))
            detail = f"token {i}: fcc {observed[i:i+8]!r}, clang {reference[i:i+8]!r}"
            status = "diff"
        if normalized and actual.returncode == 0 and case.category != "regression":
            baseline = preprocess([ORACLE, *normalized, *args], case, deps=[defs], scratch=defs.parent)
            if baseline.returncode == 0 and observed == tokens(baseline.stdout):
                return case, "normalized-same", detail
            baseline = preprocess([ORACLE, *normalized, *shared_headers, *args], case, deps=[defs], scratch=defs.parent)
            if baseline.returncode == 0 and observed == tokens(baseline.stdout):
                return case, "shared-headers-same", detail
        if case.category == "manifest" and status == "diff":
            gcc = preprocess(["gcc", *args], case)
            if gcc.returncode == 0 and reference != tokens(gcc.stdout):
                detail += " (GCC also differs from Clang)"
        return case, status, detail
    except subprocess.TimeoutExpired as timeout:
        return case, "timeout", str(timeout.cmd[0])


def make_cases(root, suites):
    if "regression" in suites:
        includes = (f"-I{TESTS / 'first'}", f"-I{TESTS / 'second'}")
        yield from (Case("regression", path.name, str(path), includes)
                    for path in sorted(TESTS.glob("*.c")))
    if "manifest" in suites:
        rows = json.loads((root / "manifest.json").read_text())["files"]
        for row in rows:
            if row["category"] == "preprocessor" and all(
                row["oracles"][cc]["compile_ok"] for cc in ("gcc", "clang")
            ):
                name = row["path"]
                yield Case("manifest", name, str(root / "upstream" / name))
    if "headers" in suites:
        # Use the actual physical name, so a file shadowed by a similarly
        # named multiarch or resource header is still exercised individually.
        system = Path("/usr/include")
        for path in sorted(system.rglob("*.h")):
            if path.is_file():
                yield Case("headers", str(path.relative_to(system)), "-", ("-x", "c"),
                           f'#include "{path}"\n'.encode())
    if "projects" in suites:
        for name, pattern in (("sqlite", "sqlite3.c"), ("lua", "src/*.c"), ("zlib", "*.c")):
            for path in sorted((root / "upstream" / name).glob(pattern)):
                yield Case(name, path.name, str(path))


def extra_checks(fcc):
    cases = [
        (b"#error fcc deliberate diagnostic\n", b"<stdin>:1:1: error: fcc deliberate diagnostic"),
        (b"ok\n  /* unclosed\n", b"<stdin>:2:3: error: unterminated block comment"),
        (b"  \"unclosed\n", b"<stdin>:1:3: error: unterminated literal"),
        (b"#if 1\nready\n", b"<stdin>:1:1: error: unterminated conditional"),
        (b"#define F(x) x\nF(\n", b"<stdin>:2:1: error: unterminated macro invocation"),
    ]
    for source, diagnostic in cases:
        got = run([fcc, "-E", "-P", "-"], source)
        if got.returncode != 1 or diagnostic not in got.stderr:
            raise SystemExit(f"diagnostic regression for {source!r}: {got.returncode}, {got.stderr!r}")
    cli_errors = [
        (("-E", "-std=gnu26", "-"), b"unsupported C standard"),
        (("-E", "-", "extra.c"), b"expected one input file, got extra.c"),
        (("-fsyntax-only", "-", "extra.c"), b"expected one input file, got extra.c"),
    ]
    for args, diagnostic in cli_errors:
        got = run([fcc, *args], b"int x;\n")
        if got.returncode != 1 or diagnostic not in got.stderr:
            raise SystemExit(f"CLI diagnostic regression for {args!r}: {got.returncode}, {got.stderr!r}")
    nfc = run([fcc, "-E", "-P", "-std=c23", "-"], "int e\u0301;\n".encode())
    if nfc.returncode != 1 or b"identifier is not in Unicode NFC" not in nfc.stderr:
        raise SystemExit("strict identifier normalization regression")
    with tempfile.TemporaryDirectory() as temp:
        p = Path(temp)
        (p / "inc.h").write_text("#define Y 12\ny\n")
        (p / "mac.h").write_text("#define MAC 23\nno_text\n")
        (p / "a.c").write_text("D MAC Y DROPPED __STDC_VERSION__ __STRICT_ANSI__\n")
        output = p / "a.i"
        dep = p / "a.d"
        got = run([fcc, "-E", "-std=c23", "-D", "D=5", "-DDROPPED", "-UDROPPED",
                   "-imacros", str(p / "mac.h"), "-include", str(p / "inc.h"), "-MD",
                   "-MF", str(dep), "-MT", "custom.o", "-MP", "-o", str(output), str(p / "a.c")])
        if got.returncode:
            raise SystemExit(got.stderr.decode())
        if b"no_text" in output.read_bytes() or b"5 23 12 DROPPED 202311L 1" not in output.read_bytes():
            raise SystemExit("forced include, macro, or output-file regression")
        if not dep.read_text().startswith("custom.o:") or f"\n{p / 'inc.h'}:\n" not in dep.read_text():
            raise SystemExit("dependency file regression")
        main = p / "main.c"
        main.write_text('#include <stdio.h>\n#include "inc.h"\nY\n')
        marked = run([fcc, "-E", str(main)])
        if b' 1 3\n' not in marked.stdout or b' 2\n' not in marked.stdout:
            raise SystemExit("include line marker regression")
        deps = run([fcc, "-MM", str(main)])
        if deps.returncode or b"stdio.h" in deps.stdout or b"inc.h" not in deps.stdout:
            raise SystemExit("system-header dependency filtering regression")
        quoted = p / "quoted"
        after = p / "after"
        quoted.mkdir()
        after.mkdir()
        (quoted / "test.h").write_text("quote_dir\n")
        (after / "test.h").write_text("after_dir\n")
        search = run([fcc, "-E", "-P", "-iquote", str(quoted), "-idirafter", str(after), "-"],
                     b'#include "test.h"\n#include <test.h>\n')
        if search.returncode or tokens(search.stdout) != [b"quote_dir", b"after_dir"]:
            raise SystemExit("quote versus system include order regression")
        dump = run([fcc, "-E", "-dM", "-std=c23", str(main)])
        if dump.returncode or b"#define __fcc__ 1\n" not in dump.stdout or b"#define linux " in dump.stdout:
            raise SystemExit("strict macro dump regression")
    print(f"directed CLI and diagnostics: {len(cases) + len(cli_errors) + 7} passed", flush=True)


def normalized_features():
    # A Clang -undef invocation still has its magic __has_* operators. Hold
    # our advertised features fixed for the target-normalized comparison.
    source = (ROOT / "cc/features.fas").read_text()
    out = ["#define FCC_TEST_SECOND(_0, _1, ...) _1",
           "#define FCC_TEST_PROBE(...) FCC_TEST_SECOND(__VA_ARGS__, 0)"]
    for category, operators in (
        ("FEATURES", ("__has_feature", "__has_extension")),
        ("ATTRIBUTES", ("__has_attribute",)),
        ("BUILTINS", ("__has_builtin",)),
    ):
        names = re.search(r'const C_' + category + r' addr = c"([^"]+)"', source)[1].split()
        prefix = "FCC_TEST_" + category + "_"
        for name in names:
            out.append(f"#define {prefix}{name} ~, 1")
            if category != "BUILTINS":
                out.append(f"#define {prefix}__{name}__ ~, 1")
        for operator in operators:
            out.append(f"#undef {operator}")
            out.append(f"#define {operator}(x) FCC_TEST_PROBE({prefix}##x)")
    return ("\n".join(out) + "\n").encode()


def read_expected(path):
    entries = {}
    for number, line in enumerate(path.read_text().splitlines(), 1):
        if not line.strip() or line.lstrip().startswith("#"):
            continue
        status, case, reason = line.split(maxsplit=2)
        if status not in {"diff", "fcc-error", "clang-skip", "timeout"} or case in entries:
            raise ValueError(f"{path}:{number}: invalid or duplicate expectation")
        entries[case] = (status, reason)
    return entries


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("fcc")
    ap.add_argument("--cc-tests", type=Path, default=ROOT.parent / "cc-tests")
    ap.add_argument("--suite", action="append", choices=("regression", "manifest", "headers", "projects"))
    ap.add_argument("--jobs", type=int, default=min(os.cpu_count() or 1, 8))
    ap.add_argument("--filter", default="", help="regular expression on case names")
    ap.add_argument("--limit", type=int)
    ap.add_argument("--json", type=Path, help="record case statuses and first mismatches")
    ap.add_argument("--expected", type=Path, default=Path(__file__).with_name("expected_diffs.txt"))
    opt = ap.parse_args()
    expected = read_expected(opt.expected)
    unexpected = 0
    suites = opt.suite or ["regression", "manifest", "headers", "projects"]
    if "regression" in suites:
        extra_checks(opt.fcc)
    cases = [c for c in make_cases(opt.cc_tests.resolve(), suites) if re.search(opt.filter, c.name)]
    if opt.limit is not None:
        cases = cases[:opt.limit]
    categories = list(dict.fromkeys(c.category for c in cases))
    counts = {category: Counter() for category in categories}
    records = []
    with tempfile.TemporaryDirectory(dir=os.environ.get("TMPDIR")) as scratch:
        defs = Path(scratch) / "predefines.h"
        dump = run([opt.fcc, "-E", "-dM", "-"])
        dump.check_returncode()
        defs.write_bytes(dump.stdout + normalized_features())
        # Normalize the compiler's identity and data model. The extra shared-
        # header comparison holds the actual resource headers fixed as well.
        normalized = ["-undef", "-Wno-macro-redefined", "-Wno-builtin-macro-redefined",
                      "-U__has_warning", "-imacros", str(defs)]
        shared_headers = ["-nostdinc", "-isystem", str(ROOT / "cc/include"),
                          "-isystem", "/usr/local/include",
                          "-isystem", "/usr/include/x86_64-linux-gnu",
                          "-isystem", "/usr/include"]
        gcc_include = os.environ.get("FCC_GCC_INCLUDE")
        if gcc_include is None:
            gcc_dir = run(["gcc", "-print-file-name=include"])
            if gcc_dir.returncode == 0:
                gcc_include = gcc_dir.stdout.decode().strip()
        if gcc_include and Path(gcc_include).is_dir() and not gcc_include.endswith("/include-fixed"):
            shared_headers.extend(["-isystem", gcc_include])
        with ThreadPoolExecutor(max_workers=opt.jobs) as pool:
            work = pool.map(lambda c: test_case(opt.fcc, c, normalized, shared_headers, defs), cases)
            for number, (case, status, detail) in enumerate(work, 1):
                counter = counts[case.category]
                counter[status] += 1
                key = f"{case.category}/{case.name}"
                expectation = expected.get(key)
                reason = expectation[1] if expectation and expectation[0] == status else None
                failed = status in {"diff", "fcc-error", "timeout"} or (
                    status == "clang-skip" and case.category in {"regression", "manifest"}
                )
                unexpected += failed and reason is None
                record = dict(category=case.category, name=case.name, status=status, detail=detail)
                if reason:
                    record["expected_reason"] = reason
                records.append(record)
                if status not in {"same", "normalized-same", "shared-headers-same", "clang-skip"} and counter[status] <= 8:
                    suffix = f" (expected: {reason})" if reason else ""
                    print(f"{key}: {status}{suffix}: {detail}", flush=True)
                if number % 1000 == 0:
                    print(f"compared {number}/{len(cases)} inputs", flush=True)
    columns = "same normalized-same shared-headers-same diff fcc-error clang-skip timeout".split()
    print("| Suite | " + " | ".join(columns) + " |")
    print("| --- | " + " | ".join("---:" for _ in columns) + " |")
    for category, count in counts.items():
        print(f"| {category} | " + " | ".join(str(count[s]) for s in columns) + " |")
    if opt.json:
        opt.json.write_text(json.dumps({"counts": counts, "cases": records}, indent=2) + "\n")
    print(f"Unexpected differences: {unexpected}")
    return bool(unexpected)


if __name__ == "__main__":
    raise SystemExit(main())
