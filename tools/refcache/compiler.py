"""Adapters for the test harnesses' C and assembler invocations."""

import os
from pathlib import Path
import re
import subprocess
import sys

from refcache import Cache

ASSEMBLER_READ = re.compile(rb"(?m)^[ \t]*\.(?:include|incbin)\b")


def assemble(argv, *, ns, output, scratch, source=None, input=None):
    data = Path(source).read_bytes() if source else input
    if ASSEMBLER_READ.search(data):
        return subprocess.run(argv, input=input, capture_output=True)
    return Cache().run(argv, ns=ns, deps=[source] if source else [], outs=[output],
                       scratch=scratch, input=input)


def compile_command(argv, *, ns, scratch=None, **options):
    """Cache the ordinary driver forms used by the shell fixtures."""
    _, *args = argv
    output = None
    inputs = []
    skip_value = False
    for i, arg in enumerate(args):
        if skip_value:
            skip_value = False
            continue
        if arg == "-o" and i + 1 < len(args):
            output = args[i + 1]
            skip_value = True
        elif arg in ("-x", "-I", "-isystem", "-L", "-D", "-U", "-l"):
            skip_value = True
        elif arg.startswith(("@", "-M", "-save-temps", "-Wp,", "-Wa,", "-Xassembler",
                             "-Xclang", "-fplugin", "-wrapper", "-specs", "--config",
                             "-Wl,", "-Xlinker", "-B", "--sysroot", "-fprofile",
                             "-gsplit-dwarf", "-ftime-trace", "-fuse-ld", "-fmodules")):
            return None
        elif arg == "-":
            return None
        elif not arg.startswith("-"):
            if Path(arg).suffix not in (".c", ".s", ".S", ".ll", ".o", ".a", ".so") or not Path(arg).is_file():
                return None
            inputs.append(arg)
    if not inputs or not output or output == "-" or not Path(output).parent.is_dir():
        return None
    # One -MF is safe for a single preprocessor input.
    if sum(Path(p).suffix in (".c", ".S") for p in inputs) > 1:
        return None
    if any(Path(p).suffix in (".s", ".S") and ASSEMBLER_READ.search(Path(p).read_bytes()) for p in inputs):
        return None
    if any(Path(p).suffix == ".a" and Path(p).open("rb").read(8) == b"!<thin>\n" for p in inputs):
        return None
    return Cache().run(argv, ns=ns, deps=inputs, outs=[output], scratch=scratch, c=True, **options)


def main():
    namespace, scratch, tool, *args = sys.argv[1:]
    argv = [tool, *args]
    if Cache().root is None:
        os.execvp(tool, argv)
    result = compile_command(argv, ns=namespace, scratch=scratch or None)
    if result is None:
        os.execvp(tool, argv)
    sys.stdout.buffer.write(result.stdout)
    sys.stderr.buffer.write(result.stderr)
    return result.returncode if result.returncode >= 0 else 128 - result.returncode


if __name__ == "__main__":
    raise SystemExit(main())
