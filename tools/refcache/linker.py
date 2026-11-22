"""Explicit dependency lists for the shadow linkers and generated ELF fixtures."""

import os
from pathlib import Path
import sys
import subprocess

from refcache import Cache

# Scripts, thin archives, plugins, response files and undeclared library-search
# modes can open other files. They go through the original linker.
SWITCHES = frozenset("""
-shared -pie --pie -no-pie -static -r --relocatable -Bstatic -Bdynamic
--as-needed --no-as-needed --export-dynamic -E --gc-sections --eh-frame-hdr
--no-undefined --enable-new-dtags --disable-new-dtags --build-id
--strip-debug -S --strip-all -s --whole-archive --no-whole-archive
--start-group --end-group --no-dynamic-linker --fatal-warnings
""".split())
VALUES = frozenset("""
-e --entry -u --undefined -z --hash-style --build-id -soname --soname
-rpath --rpath -rpath-link --rpath-link -dynamic-linker --dynamic-linker -m
""".split())


def link_inputs(args, cwd, libraries):
    output = "a.out"
    paths = []
    search = []
    libs = []
    rpath_links = []
    it = iter(args)
    for arg in it:
        key, eq, value = arg.partition("=")
        if key in ("-o", "--output"):
            output = value if eq else next(it)
        elif key in ("--version-script", "--dynamic-list"):
            name = value if eq else next(it)
            script = cwd / name
            if not script.is_file() or b"INCLUDE" in script.read_bytes():
                return None
            paths.append(name)
        elif key in ("-rpath-link", "--rpath-link"):
            if not libraries:
                return None
            rpath_links.extend((value if eq else next(it)).split(":"))
        elif arg in SWITCHES:
            continue
        elif key in VALUES:
            if not eq:
                next(it)
        elif libraries and arg.startswith("-L"):
            search.append(arg[2:] or next(it))
        elif libraries and arg.startswith("-l"):
            libs.append(arg[2:] or next(it))
        elif arg.startswith(("-", "@")):
            return None
        else:
            path = cwd / arg
            try:
                if not path.is_file():
                    return None
                with path.open("rb") as stream:
                    if not stream.read(8).startswith((b"\x7fELF", b"!<arch>\n")):
                        return None
            except OSError:
                return None
            paths.append(arg)
    if libs:
        # These search roots and files are part of the generated fuzz fixture.
        # Record each existing alternative, including names whose preference
        # depends on -Bstatic/-Bdynamic or the search order.
        for lib in libs:
            if lib == "c":
                paths.extend(libraries)
                continue
            matches = [str(Path(d) / f"lib{lib}.{suffix}") for d in search for suffix in ("so", "a")
                       if (cwd / d / f"lib{lib}.{suffix}").is_file()]
            if not matches:
                return None
            paths.extend(matches)
    # The fuzzer uses -rpath-link to find a generated transitive input.
    for directory in rpath_links:
        paths.extend(str(p) for p in sorted((cwd / directory).glob("lib*.so"))
                     if p.name != Path(output).name)
    return (output, sorted(set(paths))) if paths else None


def oracle_link(argv, *, cwd=None, scratch=None, env=None, timeout=None, libraries=()):
    cache = Cache()
    current = Path(cwd or os.getcwd())
    env = dict(os.environ if env is None else env)
    try:
        inputs = link_inputs(argv[1:], current, libraries) if cache.root is not None else None
    except (OSError, StopIteration):
        inputs = None
    if inputs is None or env.get("LD_PRELOAD") or any(key.startswith("FAS_IO_") for key in env):
        return subprocess.run(argv, cwd=cwd, env=env, capture_output=True, timeout=timeout)
    output, paths = inputs
    # Include the selected search result in the request so adding a preferred
    # archive or DSO invalidates the old entry even if its files still exist.
    # The reference linker can choose different aliases in error diagnostics.
    return cache.run(argv, ns="ocaml-ld", deps=paths, outs=[output],
                     cwd=cwd, scratch=scratch, env=env, timeout=timeout, cache_failures=False)


def shadow_scratch(args):
    base = Path(os.environ.get("TMPDIR") or "/tmp").resolve()
    for arg in args:
        path = Path(arg.partition("=")[2] if "=" in arg else arg)
        if path.is_absolute() and base in path.parents:
            relative = path.relative_to(base)
            if len(relative.parts) > 1 and relative.parts[0].startswith("fas"):
                return base / relative.parts[0]
    return None


if __name__ == "__main__":
    command = sys.argv[1:]
    if Cache().root is None:
        os.execvpe(command[0], command, os.environ)
    result = oracle_link(command, scratch=shadow_scratch(command[1:]))
    sys.stdout.buffer.write(result.stdout)
    sys.stderr.buffer.write(result.stderr)
    raise SystemExit(result.returncode if result.returncode >= 0 else 128 - result.returncode)
