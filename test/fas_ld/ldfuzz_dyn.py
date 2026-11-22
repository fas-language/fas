#!/usr/bin/env python3
"""Differentially fuzz x86-64 glibc links and linker diagnostics."""

import argparse
from collections import Counter
import json
import os
from pathlib import Path
import random
import resource
import shlex
import shutil
import subprocess
import sys

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "tools/refcache"))
from linker import oracle_link
from refcache import run as cached_run

LOADER = "/lib64/ld-linux-x86-64.so.2"
LIBC_DIR = "/usr/lib/x86_64-linux-gnu"
STATUSES = ("SAME", "DIFF", "BYTES", "REFUSED", "RUN-MISMATCH", "TIMEOUT")
LIBC_INPUTS = tuple(str(Path(LIBC_DIR) / name)
                    for name in ("libc.so", "libc.so.6", "libc_nonshared.a")) + (LOADER,)
REFERENCE_STATUSES = ("ref-ok", "EXPECTED-DIAG", "UNEXPECTED-FAIL")
START = """\
.text
.globl _start
.type _start,@function
_start:
    and $-16, %rsp
    call main
    mov %eax, %edi
    call _exit@PLT
.size _start, .-_start
.section .note.GNU-stack,"",@progbits
"""


class Case:
    def __init__(self, path, seed, reference, candidate):
        self.path = path
        path.mkdir()
        (path / "new").mkdir()
        self.rng = random.Random(seed)
        self.reference = reference
        self.candidate = candidate
        self.env = {k: v for k, v in os.environ.items()
                    if not k.startswith("LD_") and k not in ("FAS_ORACLE", "FAS_LD_THREADS")}
        self.threads = self.rng.randint(1, 8)
        self.commands = [
            "#!/bin/sh",
            'cd -- "$(dirname -- "$0")" || exit 1',
            "ulimit -f 524288",
            "ulimit -c 0",
            "unset FAS_ORACLE FAS_LD_THREADS",
            'for key in $(env | cut -d= -f1 | sed -n "/^LD_/p"); do unset "$key"; done',
            "mkdir -p new",
        ]
        self.results = []

    def run(self, args, log, *, candidate=False, cwd=".", compile_input=None):
        command = shlex.join(["timeout", "30s", *args])
        if candidate:
            command = f"FAS_LD_THREADS={self.threads} {command}"
        if cwd != ".":
            command = f"(cd {shlex.quote(cwd)} && {command})"
        self.commands.append(command)
        env = dict(self.env, FAS_LD_THREADS=str(self.threads)) if candidate else self.env
        try:
            if compile_input is not None:
                proc = cached_run(args, ns="fas-ld/fuzz-cc", deps=[compile_input], outs=[log],
                                  cwd=self.path / cwd, scratch=self.path, env=env, timeout=30, c=True)
            elif args[0] == self.reference:
                proc = oracle_link(args, cwd=self.path / cwd, scratch=self.path,
                                   env=env, timeout=30, libraries=LIBC_INPUTS)
            else:
                proc = subprocess.run(args, cwd=self.path / cwd, env=env, capture_output=True, timeout=30)
            result = (proc.returncode, proc.stdout, proc.stderr)
        except subprocess.TimeoutExpired as error:
            result = (None, error.stdout or b"", error.stderr or b"")
        code, stdout, stderr = result
        (self.path / f"{log}.stdout").write_bytes(stdout)
        (self.path / f"{log}.stderr").write_bytes(stderr)
        (self.path / f"{log}.exit").write_text(f"{code if code is not None else 'timeout'}\n")
        return result

    def compile(self, source, contents, *flags):
        (self.path / source).write_text(contents)
        obj = str(Path(source).with_suffix(".o"))
        command = [
            os.environ.get("CC", "clang-22"), "-c", source, "-o", obj,
            "-fno-stack-protector", "-ffunction-sections", "-fdata-sections",
            self.rng.choice(["-O0", "-O1", "-O2", "-Os"]), *flags,
        ]
        code, _, stderr = self.run(command, obj, compile_input=source)
        self.commands[-1] += " || exit $?"
        if code != 0:
            raise RuntimeError(f"{source}: compiler exit {code}: {stderr.decode(errors='replace')}")
        return obj

    def archive(self, name, obj):
        code, _, stderr = self.run(["ar", "rcs", name, obj], name)
        self.commands[-1] += " || exit $?"
        if code != 0:
            raise RuntimeError(f"{name}: ar exit {code}: {stderr.decode(errors='replace')}")

    def link(self, output, args, *, kind, diagnostic=None):
        # Give both linkers identical arguments and dependency bytes. Keep the
        # reference output at its original name for the next stage and loader.
        self.commands.append(shlex.join(["rm", "-f", "--", output, f"new/{output}"]))
        arguments = ["-o", output, *args]
        new = self.run([self.candidate, *arguments], f"{output}.new", candidate=True)
        new_file = self.path / "new" / output
        ref_file = self.path / output
        self.commands.append(f"if test -f {output}; then mv -- {output} new/{output}; fi")
        if ref_file.exists():
            ref_file.rename(new_file)
        ref = self.run([self.reference, *arguments], f"{output}.ref")

        ref_ok = ref[0] == 0 and ref_file.is_file()
        if diagnostic:
            scenario, expected = diagnostic
            intended = ref[0] == 1 and expected.encode() in ref[2] and not ref_file.exists()
            reference_status = "EXPECTED-DIAG" if intended else "UNEXPECTED-FAIL"
        else:
            reference_status = "ref-ok" if ref_ok else "UNEXPECTED-FAIL"

        if new[0] is None or ref[0] is None:
            status = "TIMEOUT"
        elif diagnostic:
            # A refusal is a mismatch here: this link must emit the same
            # failure as the reference, without publishing an output file.
            same = (new[0], new[2]) == (ref[0], ref[2])
            status = "SAME" if same and not new_file.exists() and not ref_file.exists() else "DIFF"
        elif new[0] == 3:
            status = "REFUSED"
        elif new != ref:
            status = "DIFF"
        elif artifact(new_file) != artifact(ref_file):
            status = "BYTES"
        else:
            status = "SAME"
        result = {
            "kind": kind, "output": output, "status": status,
            "ref_exit": ref[0], "new_exit": new[0], "reference_status": reference_status,
        }
        if diagnostic:
            result.update(tag="EXPECT-DIAG", scenario=scenario, expected_stderr=expected)
        self.results.append(result)
        return ref_ok

    def save(self):
        replay = self.path / "replay.sh"
        replay.write_text("\n".join(self.commands) + "\n")
        replay.chmod(0o755)
        (self.path / "results.json").write_text(json.dumps(self.results, indent=2) + "\n")


def artifact(path):
    return path.read_bytes() if path.is_file() else None


def link_options(rng):
    args = ["--hash-style=" + rng.choice(["sysv", "gnu", "both"])]
    if rng.random() < 0.5:
        args += ["-z", "now"]
    if rng.random() < 0.8:
        args += ["-z", rng.choice(["relro", "norelro"])]
    for flag in ["--gc-sections", "--export-dynamic", "--eh-frame-hdr"]:
        if rng.random() < 0.5:
            args.append(flag)
    if rng.random() < 0.3:
        args.append(rng.choice(["--build-id", "--build-id=sha1", "--build-id=md5"]))
    return args


def origin_options(rng):
    return ["-rpath", "$ORIGIN", rng.choice(["--enable-new-dtags", "--disable-new-dtags"])]


def tls_options(rng):
    return rng.choice([(), ("-ftls-model=initial-exec",), ("-mtls-dialect=gnu2",)])


def library(case, name, sources, extra, *, deps=(), tls_flags=()):
    rng = case.rng
    objects = [case.compile(source, code, rng.choice(["-fPIC", "-fpic"]), *tls_flags)
               for source, code in sources.items()]
    rng.shuffle(objects)
    # A slash in a fallback DT_NEEDED name bypasses the loader's search path.
    args = ["-shared", "-soname", name, *link_options(rng), *extra, *objects, *deps]
    return case.link(name, args, kind="shared+dso" if deps else "shared")


def generate_diagnostic(case, rng):
    scenario = rng.choice(["undefined", "missing-version", "missing-library", "tls-mismatch", "non-pic"])
    case.commands.append(f"# EXPECT-DIAG {scenario}")
    args = ["-shared", *link_options(case.rng)]
    if scenario == "undefined":
        obj = case.compile("bad.c", "int missing(void); int api(void) { return missing(); }\n", "-fPIC")
        args += [obj, *rng.choice([["--no-undefined"], ["-z", "defs"]])]
        expected = "undefined symbol missing"
    elif scenario == "missing-version":
        obj = case.compile("bad.c", 'int impl(void) { return 1; }\n'
                           '__asm__(".symver impl,api@MISSING");\n', "-fPIC")
        (case.path / "versions.map").write_text("V1 { global: api; };\n")
        args += [obj, "--version-script=versions.map"]
        expected = "references missing version node MISSING"
    elif scenario == "missing-library":
        obj = case.compile("bad.c", "int api(void) { return 1; }\n", "-fPIC")
        args += [obj, "-L.", "-lldfuzz_diag_missing"]
        expected = "cannot find -lldfuzz_diag_missing"
    elif scenario == "tls-mismatch":
        obj = case.compile("bad.c", "extern __thread int slot; int api(void) { return slot; }\n", "-fPIC")
        definition = case.compile("definition.c", "int slot = 1;\n", "-fPIC")
        args += [obj, definition]
        expected = "TLS relocation references non-TLS symbol slot"
    else:
        obj = case.compile("bad.c", "int datum = 1; int api(void) { return datum; }\n", "-fno-pic", "-fno-pie")
        args.append(obj)
        expected = "direct import relocation (compile position-independent code)"
    case.link("libbad.so", args, kind="shared", diagnostic=(scenario, expected))


def generate(case):
    rng = case.rng
    def value():
        return rng.randint(1, 30)

    pie = rng.random() < 0.55
    chain = rng.random() < 0.3
    unused_library = not chain and rng.random() < 0.25
    callback = rng.random() < 0.35
    tls = rng.random() < 0.55
    ifunc = rng.random() < 0.35
    versions = rng.choice(["none", "none", "anonymous", "named", "symver"])
    weak_override = rng.random() < 0.5

    datum, hidden, weak, strong = (value() for _ in range(4))
    impl = [
        f"int api_data = {datum};",
        'extern __attribute__((visibility("hidden"))) int hidden_value(void);',
        f"__attribute__((weak)) int weak_value(void) {{ return {weak}; }}",
        "int private_unused(void) { return 99; }",
    ]
    helper = [f'__attribute__((visibility("hidden"))) int hidden_value(void) {{ return {hidden}; }}']
    if weak_override:
        helper.append(f"int weak_value(void) {{ return {strong}; }}")
    terms = ["api_data", "hidden_value()", "weak_value()"]
    library_value = datum + hidden + (strong if weak_override else weak)

    if tls:
        initialized = value()
        visibility = rng.choice(["", '__attribute__((visibility("hidden"))) '])
        helper.append(f"{visibility}__thread int private_tls = {initialized};")
        impl += [
            f"extern {visibility}__thread int private_tls;",
            f"static __thread int zeros __attribute__((aligned({rng.choice([4, 16, 64])})));",
        ]
        terms += ["private_tls", "++zeros"]
        library_value += initialized + 1

    main_decls = ["extern int api_data;", "extern int api_answer(void);"]
    main_terms = ["api_data", "api_answer()"]
    delta = rng.randint(1, 10)
    expected = datum + 2 * delta
    alignment_check = ""
    if rng.random() < 0.7:
        array_first, array_last, block_value = (value() for _ in range(3))
        length = rng.randint(3, 10)
        alignment = rng.choice([16, 32])
        block_type = f"struct __attribute__((aligned({alignment}))) block {{ int value; }};"
        impl += [
            block_type,
            f"int api_array[{length}] = {{ {array_first}, [{length - 1}] = {array_last} }};",
            f"struct block api_block = {{ {block_value} }};",
        ]
        main_decls += [block_type, f"extern int api_array[{length}];", "extern struct block api_block;"]
        main_terms += ["api_array[0]", f"api_array[{length - 1}]", "api_block.value"]
        expected += array_first + array_last + block_value
        alignment_check = (f"volatile unsigned long address = (unsigned long)&api_block; "
                           f"if (address % {alignment}) return 125; ")

    if rng.random() < 0.45:
        protected = value()
        impl.append('__attribute__((visibility("protected"))) '
                    f"int api_protected(void) {{ return {protected}; }}")
        terms.append("api_protected()")
        library_value += protected
        main_decls.append("extern int api_protected(void);")
        main_terms.append("api_protected()")
        expected += protected

    if rng.random() < 0.65:
        impl += [
            "extern int api_weak_function(void) __attribute__((weak));",
            "extern int api_weak_data __attribute__((weak));",
        ]
        terms += ["(api_weak_function ? api_weak_function() : 1)",
                  "(&api_weak_data ? api_weak_data : 2)"]
        for declaration, fallback in [
            ("int api_weak_function(void) {{ return {}; }}", 1),
            ("int api_weak_data = {};", 2),
        ]:
            if rng.random() < 0.5:
                present = value()
                helper.append(declaration.format(present))
                library_value += present
            else:
                library_value += fallback

    tls_obj = []
    if tls:
        tls_value, local_tls = value(), value()
        helper.append(f"__thread int api_tls = {tls_value};")
        impl.append("extern __thread int api_tls;")
        terms.append("api_tls")
        library_value += tls_value + 1
        # A separate PIC unit selects the TLS access model independently of
        # the executable's direct references to imported non-TLS data.
        tls_obj.append(case.compile("main_tls.c", f"""\
extern __thread int api_tls;
static __thread int local_tls = {local_tls};
int update_tls(void) {{ return local_tls++ + ++api_tls; }}
""", "-fPIC", *tls_options(rng)))
        main_decls.append("extern int update_tls(void);")
        main_terms.append("tls_value")
        expected += local_tls + tls_value + 1
    if ifunc:
        selected = value()
        impl += [
            f"static int selected(void) {{ return {selected}; }}",
            "static int (*resolve(void))(void) { return selected; }",
            'int api_ifunc(void) __attribute__((ifunc("resolve")));',
        ]
        main_decls.append("extern int api_ifunc(void);")
        main_terms.append("api_ifunc()")
        expected += selected

    if callback:
        result = value()
        impl.append("extern int callback(void);")
        terms.append("callback()")
        library_value += result
        main_decls.append(f"int callback(void) {{ return {result}; }}")

    deps = []
    if chain or unused_library:
        result = value()
        b_source = f"int from_b(void) {{ return {result}; }}\n"
        if chain and tls:
            from_b_tls = value()
            b_source += f"__thread int from_b_tls = {from_b_tls};\n"
            impl.append("extern __thread int from_b_tls;")
            terms.append("from_b_tls")
            library_value += from_b_tls
        if not library(case, "libb.so", {"b.c": b_source}, []):
            return
    if chain:
        deps = rng.choice([["./libb.so"], ["-L.", "-lb"]])
        impl.append("extern int from_b(void);")
        terms.append("from_b()")
        library_value += result

    impl.append("int api_answer(void) { return " + " + ".join(terms) + "; }")
    lib_flags = origin_options(rng) if chain else []
    if versions == "symver":
        old, default = value(), value()
        impl += [
            f"int impl_old(void) {{ return {old}; }}",
            f"int impl_new(void) {{ return {default}; }}",
            '__asm__(".symver impl_old,api_versioned@V1");',
            '__asm__(".symver impl_new,api_versioned@@V2");',
        ]
        main_decls.append("extern int api_versioned(void);")
        main_terms.append("api_versioned()")
        expected += default
        if rng.random() < 0.5:
            main_decls += [
                "extern int old_version(void);",
                '__asm__(".symver old_version,api_versioned@V1");',
            ]
            main_terms.append("old_version()")
            expected += old
    if versions != "none":
        scripts = {
            "anonymous": "{ global: api_*; local: *; };\n",
            "named": "V1 { global: api_answer; local: private*; };\n"
                     "V2 { global: api_*; local: *; } V1;\n",
            "symver": "V1 { local: impl_*; };\n"
                      "V2 { global: api_*; local: *; } V1;\n",
        }
        (case.path / "versions.map").write_text(scripts[versions])
        lib_flags.append("--version-script=versions.map")

    sources = {"a.c": "\n".join(impl) + "\n", "helper.c": "\n".join(helper) + "\n"}
    if not library(case, "liba.so", sources, lib_flags, deps=deps,
                   tls_flags=tls_options(rng) if tls else ()):
        return

    archive_args = []
    if rng.random() < 0.45:
        archived = value()
        obj = case.compile("support.c", f"int archived(void) {{ return {archived}; }}\n", "-fPIC")
        case.archive("libsupport.a", obj)
        archive_args = ["-Bstatic", rng.choice(["-lsupport", "./libsupport.a"]), "-Bdynamic"]
        main_decls.append("extern int archived(void);")
        main_terms.append("archived()")
        expected += archived

    # Mutating the import checks that the DSO sees the executable's copy.
    # The ABI-aligned entry point calls _exit through the libc PLT.
    main = "\n".join(main_decls) + "\n"
    tls_update = "int tls_value = update_tls(); " if tls else ""
    main += (f"int main(void) {{ {alignment_check}{tls_update}api_data += {delta}; "
             f"return ({' + '.join(main_terms)}) % 125; }}\n")
    main_flags = ["-fPIC"] if pie else ["-fno-pic", "-fno-pie"]
    main_obj = case.compile("main.c", main, *main_flags)
    start_obj = case.compile("start.S", START)
    exe_options = link_options(rng)
    if callback and "--export-dynamic" not in exe_options:
        exe_options.append("--export-dynamic")
    args = [
        "-pie" if pie else "-no-pie", "-dynamic-linker", LOADER,
        *origin_options(rng), *exe_options,
        "-L" + LIBC_DIR, "-L.", start_obj, main_obj, *tls_obj, *archive_args,
        rng.choice(["--as-needed", "--no-as-needed"]), rng.choice(["-la", "./liba.so"]),
        *([rng.choice(["-lb", "./libb.so"])] if unused_library else []), "-lc",
    ]
    if chain:
        args += ["-rpath-link", "."]
    if case.link("program", args, kind="executable"):
        expected = (expected + library_value) % 125
        # Use a different working directory to exercise the loader's $ORIGIN.
        (case.path / "run").mkdir()
        case.commands.append("mkdir -p run")
        actual, _, _ = case.run(["../program"], "program.run", cwd="run")
        case.results.append({
            "kind": "runtime", "output": "program", "expected": expected, "actual": actual,
            "status": "SAME" if actual == expected else "RUN-MISMATCH",
        })


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("ref_ld", type=Path)
    parser.add_argument("new_ld", type=Path)
    parser.add_argument("workdir", type=Path)
    parser.add_argument("seed", type=int)
    parser.add_argument("count", type=int)
    parser.add_argument("--keep-refused", action="store_true",
                        help="keep reproducers for cases whose only difference is refusal")
    args = parser.parse_args()
    if args.count < 1:
        parser.error("count must be positive")
    reference, candidate = (path.resolve() for path in [args.ref_ld, args.new_ld])
    for path in (reference, candidate):
        if not path.is_file() or not os.access(path, os.X_OK):
            parser.error(f"linker is not executable: {path}")
    workdir = args.workdir.resolve()
    workdir.mkdir(parents=True, exist_ok=True)
    seeds = range(args.seed, args.seed + args.count)
    if any((workdir / f"c{seed}").exists() for seed in seeds):
        parser.error("case directories already exist; choose a fresh workdir to preserve reproducers")

    resource.setrlimit(resource.RLIMIT_CORE, (0, 0))
    counts = {kind: Counter() for kind in ("shared", "shared+dso", "executable", "runtime")}
    diagnostic_counts = {}
    case_counts = Counter()
    for seed in seeds:
        case = Case(workdir / f"c{seed}", seed, str(reference), str(candidate))
        try:
            diagnostic_rng = random.Random(f"expect-diag:{seed}")
            if diagnostic_rng.random() < 0.15:
                generate_diagnostic(case, diagnostic_rng)
            else:
                generate(case)
        except (OSError, RuntimeError):
            case.save()
            raise
        statuses = {item["status"] for item in case.results} - {"SAME"}
        failures = []
        unexpected = False
        for item in case.results:
            status = item["status"]
            group = counts[item["kind"]]
            group[status] += 1
            reference_status = item.get("reference_status")
            label = item["output"]
            if reference_status:
                group[reference_status] += 1
            if item.get("tag") == "EXPECT-DIAG":
                diagnostic = diagnostic_counts.setdefault(item["scenario"], Counter())
                diagnostic[reference_status] += 1
                diagnostic[status] += 1
                label = f'EXPECT-DIAG {item["scenario"]} {label}'
            if reference_status == "UNEXPECTED-FAIL":
                failures.append(f"{label}=UNEXPECTED-FAIL")
                unexpected = True
            if status != "SAME":
                failures.append(f"{label}={status}")
        for status in statuses or {"SAME"}:
            case_counts[status] += 1
        if failures and (args.keep_refused or statuses != {"REFUSED"} or unexpected):
            case.save()
            print(f"{case.path}: {' '.join(failures)}", flush=True)
        else:
            shutil.rmtree(case.path)

    for kind, group in counts.items():
        keys = STATUSES if kind != "runtime" else ("SAME", "RUN-MISMATCH")
        if kind != "runtime":
            keys += REFERENCE_STATUSES
        print(f"{kind}: " + " ".join(f"{key}={group[key]}" for key in keys))
    for scenario, group in sorted(diagnostic_counts.items()):
        keys = ("EXPECTED-DIAG", "UNEXPECTED-FAIL", "SAME", "DIFF", "TIMEOUT")
        print(f"EXPECT-DIAG {scenario}: " + " ".join(f"{key}={group[key]}" for key in keys))
    print("cases: " + " ".join(f"{key}={case_counts[key]}" for key in STATUSES))
    return int(any(case_counts[key] for key in ("DIFF", "BYTES", "REFUSED", "RUN-MISMATCH", "TIMEOUT"))
               or any(group["UNEXPECTED-FAIL"] for group in counts.values()))


if __name__ == "__main__":
    raise SystemExit(main())
