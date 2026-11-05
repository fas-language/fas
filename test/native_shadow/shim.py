#!/usr/bin/env python3
"""Native shadow shims: run the existing LLVM-based test scripts with the
native backend generating the Fas code, without editing the scripts.

The scripts reach Fas, clang and llc through $OCAML_FAS, $CC and $LLVM_LLC
(test/native_shadow.sh points those at this file under the names `fas`, `cc`
and `llc`). The shims work as follows:

- `fas --emit-llvm ...` prints the real LLVM IR, so every IR check still
  sees real IR. It also compiles the same source with
  `--backend=native -c` into a cache keyed by the IR text, and appends
  named metadata `!fas.native.object` naming that object. opt keeps named
  metadata through its pipelines.
- `cc` / `llc` asked to compile or link a .ll that carries the marker use
  the native object instead of compiling the IR. A request for assembly
  (`-S`, `-filetype=asm`) still goes to the real tool.
- `fas x.fas -o exe` (or `-c`) builds with `--backend=native` added.
- When the native backend refuses a module, the shim falls back to the
  plain LLVM behaviour, so diagnostics and exit codes are unchanged. Every
  decision is appended to $FAS_SHADOW_LOG so the runner can tell "native
  ran" from "native refused".

Environment: FAS_SHADOW_REAL_FAS, FAS_SHADOW_REAL_CC, FAS_SHADOW_REAL_LLC
(the real tools), FAS_SHADOW_CACHE (object cache dir), FAS_SHADOW_LOG,
FAS_SHADOW_SCRIPT (name of the running test, for the log).
"""

import hashlib
import os
import re
import subprocess
import sys

MARK = "!fas.native.object"
MARK_RE = re.compile(r'^!fas\.native\.object = !\{!(\d+)\}\s*$', re.M)
# A metadata id far above anything Fas emits, so it never collides
MARK_ID = 987654321


def env(name):
    v = os.environ.get(name)
    if not v:
        sys.stderr.write(f"native shadow: {name} is not set\n")
        sys.exit(2)
    return v


def log(kind, detail=""):
    path = os.environ.get("FAS_SHADOW_LOG")
    if not path:
        return
    script = os.environ.get("FAS_SHADOW_SCRIPT", "?")
    detail = detail.replace("\n", " ")[:300]
    with open(path, "a") as f:
        f.write(f"{script}\t{kind}\t{detail}\n")


def first_error(stderr):
    """The line that says why a build failed (--keep prints notes first)"""
    lines = stderr.decode(errors="replace").strip().splitlines()
    for line in lines:
        if "error" in line:
            return line
    return lines[0] if lines else "?"


def run(argv, **kw):
    return subprocess.run(argv, **kw)


def passthrough(argv):
    os.execv(argv[0], argv)


# ---------------------------------------------------------------- fas

def native_eligible(args):
    """Linux x86-64 builds only; anything else stays on LLVM."""
    for a in args:
        if a.startswith("--target") and a not in ("--target=linux", "--target=x86_64-linux"):
            # force-native and friends are LLVM-only spellings too
            return False
        if a == "--target":
            return False
        if a.startswith("--sanitize") or a.startswith("--backend") or a == "-g":
            return False
        if a.startswith("--pgo") or a.startswith("--profile"):
            return False
    return True


def fas_inputs(args):
    return [a for a in args if a.endswith(".fas") and not a.startswith("-")]


def strip_output(args):
    out, rest, i = None, [], 0
    while i < len(args):
        a = args[i]
        if a == "-o" and i + 1 < len(args):
            out = args[i + 1]
            i += 2
            continue
        if a.startswith("-o") and len(a) > 2 and not a.startswith("-O"):
            out = a[2:]
            i += 1
            continue
        rest.append(a)
        i += 1
    return out, rest


def fas_main(args):
    real = env("FAS_SHADOW_REAL_FAS")
    emit = [a for a in args if a == "--emit-llvm" or a.startswith("--emit-llvm=")]
    if emit:
        return fas_emit_llvm(real, args, emit[0])
    # --emit-obj is the long spelling of -c, which native supports
    other_emit = any((a.startswith("--emit") and a != "--emit-obj") or a == "-S" for a in args)
    subcommand = args and not args[0].startswith("-") and not args[0].endswith(".fas")
    if other_emit or subcommand or not fas_inputs(args) or not native_eligible(args):
        passthrough([real] + args)
    # executable or object build: try native first
    r = run([real, "--backend=native"] + args, capture_output=True)
    if r.returncode == 0:
        log("native-build", " ".join(args))
        sys.stdout.buffer.write(r.stdout)
        sys.stderr.buffer.write(r.stderr)
        sys.exit(0)
    log("native-refused-build", first_error(r.stderr) + " :: " + " ".join(args))
    passthrough([real] + args)


def fas_emit_llvm(real, args, emit_flag):
    out, rest = strip_output(args)
    r = run([real] + args, capture_output=True)
    if r.returncode != 0:
        sys.stdout.buffer.write(r.stdout)
        sys.stderr.buffer.write(r.stderr)
        sys.exit(r.returncode)
    ir = open(out, "rb").read() if out else r.stdout
    sys.stderr.buffer.write(r.stderr)
    cache = env("FAS_SHADOW_CACHE")
    # Keyed by the IR alone, so two builds that print the same IR also print
    # the same marker (several scripts compare IR text between builds)
    key = hashlib.sha256(ir).hexdigest()[:24]
    obj = os.path.join(cache, key + ".o")
    if not fas_inputs(args) or not native_eligible(args) or "-" in args:
        # Not built natively, but identical IR (`--target=portable` vs the
        # default, say) is the same program: reuse its object if there is one
        if not os.path.exists(obj):
            log("skip-emit", " ".join(args))
            write_ir(out, ir)
            return
    elif not os.path.exists(obj):
        nargs = [a for a in rest if a != emit_flag]
        tmp = obj + f".{os.getpid()}.tmp"
        n = run([real, "--backend=native", "-c"] + nargs + ["-o", tmp], capture_output=True)
        if n.returncode != 0:
            log("native-refused-emit", first_error(n.stderr) + " :: " + " ".join(args))
            write_ir(out, ir)
            return
        os.replace(tmp, obj)
    log("native-emit", " ".join(args))
    text = ir.decode(errors="surrogateescape")
    if not text.endswith("\n"):
        text += "\n"
    text += f'{MARK} = !{{!{MARK_ID}}}\n!{MARK_ID} = !{{!"{obj}"}}\n'
    write_ir(out, text.encode(errors="surrogateescape"))


def write_ir(out, data):
    if out:
        with open(out, "wb") as f:
            f.write(data)
    else:
        sys.stdout.buffer.write(data)
        sys.stdout.flush()


# ---------------------------------------------------------------- cc / llc

def marker_object(path):
    """The native object named by an IR file's marker, if any."""
    if not (path.endswith(".ll") and os.path.isfile(path)):
        return None
    try:
        with open(path, "rb") as f:
            data = f.read()
    except OSError:
        return None
    if MARK.encode() not in data:
        return None
    m = re.search(rb'^!\d+ = !\{!"([^"]+\.o)"\}', data[data.rindex(MARK.encode()):], re.M)
    if not m:
        return None
    obj = m.group(1).decode()
    return obj if os.path.isfile(obj) else None


def cc_main(args):
    real = env("FAS_SHADOW_REAL_CC")
    if "-S" in args or "-E" in args or "-emit-llvm" in args:
        passthrough([real] + args)
    swapped = []
    new = []
    for a in args:
        obj = marker_object(a)
        if obj:
            swapped.append(a)
            new.append(obj)
        else:
            new.append(a)
    if not swapped:
        passthrough([real] + args)
    # `-c x.ll -o y.o`: the object is the result
    if "-c" in args:
        out, _ = strip_output(args)
        if out and len(swapped) == 1:
            with open(new[args.index(swapped[0])], "rb") as s, open(out, "wb") as d:
                d.write(s.read())
            log("cc-object", " ".join(swapped))
            sys.exit(0)
        passthrough([real] + args)
    # Linking: an object among sources needs no -x; drop IR-only flags
    new = [a for a in new if a not in ("-Wno-override-module",)]
    log("cc-link", " ".join(swapped))
    passthrough([real] + new)


def llc_main(args):
    real = env("FAS_SHADOW_REAL_LLC")
    if "-filetype=obj" not in args:
        passthrough([real] + args)
    ins = [a for a in args if a.endswith(".ll")]
    obj = marker_object(ins[0]) if len(ins) == 1 else None
    out, _ = strip_output(args)
    if not obj or not out:
        passthrough([real] + args)
    with open(obj, "rb") as s, open(out, "wb") as d:
        d.write(s.read())
    log("llc-object", ins[0])
    sys.exit(0)


def main():
    # The generated wrappers call `shim.py TOOL ARGS...`: the tool is an
    # argument, since scripts may resolve the wrapper path
    tool = sys.argv[1]
    args = sys.argv[2:]
    if tool == "fas":
        fas_main(args)
    elif tool == "cc":
        cc_main(args)
    elif tool == "llc":
        llc_main(args)
    else:
        sys.stderr.write(f"native shadow: unknown tool name {tool}\n")
        sys.exit(2)


main()
