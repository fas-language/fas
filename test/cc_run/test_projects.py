#!/usr/bin/env python3
"""Build real-world C projects with fcc and Clang and compare exact output."""

import argparse
from collections import deque
from contextlib import nullcontext
import http.client
import os
from pathlib import Path
import re
import resource
import shlex
import shutil
import signal
import socket
import subprocess
import sys
import tempfile
import time

MEM_LIMIT_BYTES = 2 * 1024 * 1024 * 1024  # 2 GiB cap per compiler child


def _limit_as():
    resource.setrlimit(resource.RLIMIT_AS, (MEM_LIMIT_BYTES, MEM_LIMIT_BYTES))

ROOT = Path(__file__).resolve().parents[2]
UPSTREAM = ROOT.parent / "cc-tests" / "upstream"
FCC = str(Path(os.environ.get("FCC", str(ROOT / "_build/fcc"))).resolve())
CLANG = os.environ.get("CLANG", "clang-22")
ENV = {
    **os.environ,
    "FCC_FIR": str(Path(os.environ.get("FCC_FIR", str(ROOT / "_build/default/tools/fas_fir/fas_fir.exe"))).resolve()),
    "FCC_LD": str(Path(os.environ.get("FCC_LD", str(ROOT / "_build/fas-ld"))).resolve()),
    "FCC_INCLUDE": str(Path(os.environ.get("FCC_INCLUDE", str(ROOT / "cc/include"))).resolve()),
}


PROJECT_JOBS = 1
PROJECT_ROOT = None

sys.path.insert(0, str(ROOT / "tools/refcache"))
from compiler import compile_command
from refcache import Cache


def run_cmd(cmd, cwd=None, input_bytes=None, timeout=180, *, both_streams=False, env=None):
    cmd = list(map(os.fspath, cmd))
    cmd_env = {**ENV, **(env or {})}
    if PROJECT_ROOT is not None:
        print(f"+ ({cwd or Path.cwd()}) {shlex.join(cmd)}", flush=True)
    cp = None
    if cmd[0] == CLANG and cwd is None and input_bytes is None and env is None and PROJECT_ROOT is not None:
        # The reference cache fingerprints C dependencies and can relocate
        # outputs below this project's temporary tree.
        if Cache().root is not None:
            cp = compile_command(cmd, ns="cc-run/projects", scratch=PROJECT_ROOT,
                                 env=cmd_env, timeout=timeout)
    preexec = _limit_as if cmd and str(cmd[0]) == FCC else None
    if cp is None:
        try:
            cp = subprocess.run(cmd, cwd=cwd, env=cmd_env, input=input_bytes,
                                capture_output=True, timeout=timeout, preexec_fn=preexec)
        except subprocess.TimeoutExpired as exc:
            log_streams(exc.stdout or b"", exc.stderr or b"")
            raise
    log_streams(cp.stdout, cp.stderr)
    if cp.returncode != 0:
        command = " ".join(cmd)
        if len(command) > 600:
            command = command[:540] + " ... " + command[-55:]
        err = cp.stderr.decode("utf-8", "replace")
        err_lo = err.lower()
        if ("out of memory" in err_lo or "out_of_memory" in err_lo
                or "cannot allocate memory" in err_lo or "bad_alloc" in err_lo
                or cp.returncode == -9):
            raise RuntimeError(f"compile-oom ({cp.returncode}): {command}\n{err[:1200]}")
        out = cp.stdout.decode("utf-8", "replace")
        errors = "\n".join(line for line in (out + "\n" + err).splitlines()
                           if re.search(r"error:|^fas-ld:|assertion failed|\*\*\*|Segmentation fault", line))
        raise RuntimeError(f"Command failed ({cp.returncode}): {command}\n"
                           f"{errors[:2500]}\n{out[-2500:]}\n{err[-2500:]}")
    return (cp.stdout, cp.stderr) if both_streams else cp.stdout


def log_streams(stdout, stderr):
    if PROJECT_ROOT is None:
        return
    for name, data in (("stdout", stdout), ("stderr", stderr)):
        if data:
            print(f"[{name}]", flush=True)
            sys.stdout.buffer.write(data)
            if not data.endswith(b"\n"):
                sys.stdout.buffer.write(b"\n")
            sys.stdout.buffer.flush()


ZLIB_SOURCES = (
    "adler32.c", "crc32.c", "deflate.c", "infback.c", "inffast.c",
    "inflate.c", "inftrees.c", "trees.c", "zutil.c", "compress.c",
    "uncompr.c", "gzclose.c", "gzlib.c", "gzread.c", "gzwrite.c",
)


def test_zlib(tmp: Path) -> bool:
    zdir = UPSTREAM / "zlib"
    srcs = ZLIB_SOURCES
    w_fcc = tmp / "zlib_fcc"
    w_ref = tmp / "zlib_ref"
    w_fcc.mkdir()
    w_ref.mkdir()

    objs = []
    for s in srcs:
        obj = w_fcc / (s[:-2] + ".o")
        run_cmd([FCC, "-c", "-DHAVE_UNISTD_H", f"-I{zdir}", str(zdir / s), "-o", str(obj)])
        objs.append(str(obj))
    run_cmd([FCC, "-DHAVE_UNISTD_H", f"-I{zdir}", str(zdir / "test/example.c"), *objs, "-o", str(w_fcc / "example")])
    run_cmd([FCC, "-DHAVE_UNISTD_H", f"-I{zdir}", str(zdir / "test/minigzip.c"), *objs, "-o", str(w_fcc / "minigzip")])

    clang_executable(w_ref / "example", [zdir / "test/example.c", *(zdir / s for s in srcs)],
                     ["-DHAVE_UNISTD_H", f"-I{zdir}"])

    out_fcc = run_cmd([str(w_fcc / "example")], cwd=str(w_fcc))
    out_ref = run_cmd([str(w_ref / "example")], cwd=str(w_ref))
    assert out_fcc == out_ref, f"zlib example mismatch:\nfcc={out_fcc[:200]!r}\nref={out_ref[:200]!r}"

    payload = (b"The quick brown fox jumps over the lazy dog 0123456789\n" * 500)
    compressed = run_cmd([str(w_fcc / "minigzip")], input_bytes=payload)
    roundtrip = run_cmd([str(w_fcc / "minigzip"), "-d"], input_bytes=compressed)
    assert roundtrip == payload, "zlib minigzip roundtrip mismatch"
    print("PASS: zlib (example exact match vs clang-22 + minigzip round-trip)")
    return True


def test_lua(tmp: Path) -> bool:
    ldir = UPSTREAM / "lua/src"
    srcs = sorted(p for p in ldir.glob("*.c") if p.name not in ("luac.c", "onelua.c"))
    w_fcc = tmp / "lua_fcc"
    w_ref = tmp / "lua_ref"
    w_fcc.mkdir()
    w_ref.mkdir()

    objs = []
    for p in srcs:
        obj = w_fcc / (p.stem + ".o")
        run_cmd([FCC, "-c", "-DLUA_USE_LINUX", f"-I{ldir}", str(p), "-o", str(obj)])
        objs.append(str(obj))
    run_cmd([FCC, *objs, "-lm", "-ldl", "-o", str(w_fcc / "lua")])
    clang_executable(w_ref / "lua", srcs, ["-DLUA_USE_LINUX", f"-I{ldir}"], ["-lm", "-ldl"])

    script = r"""
-- Comprehensive Lua differential test: arithmetic, closures, tables, metamethods,
-- coroutines, pcall/error (setjmp/longjmp), string patterns, sorting, bitops, math.
local function fib(n)
  if n < 2 then return n end
  return fib(n - 1) + fib(n - 2)
end
print("fib", fib(15))

local t = {}
for i = 1, 50 do t[i] = (i * 37) % 53 end
table.sort(t)
print("sort", t[1], t[25], t[50])

local mt = { __add = function(a, b) return setmetatable({v = a.v + b.v}, getmetatable(a)) end }
local a = setmetatable({v = 17}, mt)
local b = setmetatable({v = 25}, mt)
print("meta", (a + b).v)

local co = coroutine.create(function()
  for i = 1, 5 do coroutine.yield(i * i) end
end)
local csum = 0
while true do
  local ok, v = coroutine.resume(co)
  if not ok or v == nil then break end
  csum = csum + v
end
print("coro", csum)

local ok1, e1 = pcall(function() error("e_one") end)
local ok2, e2 = xpcall(function() error("e_two") end, function(m) return "caught:" .. tostring(m):match("e_two") end)
print("pcall", ok1, tostring(e1):match("e_one"), ok2, e2)

local s = string.gsub("hello 123 world 456", "(%d+)", function(d) return tostring(tonumber(d) * 2) end)
print("gsub", s)
print("bits", (0xff00 & 0x0ff0), (1 << 12) | 7, (~0) & 0xff)
print("math", string.format("%.6f %.6f", math.sin(1.0), math.sqrt(2.0)))
"""
    out_fcc = run_cmd([str(w_fcc / "lua"), "-e", script])
    out_ref = run_cmd([str(w_ref / "lua"), "-e", script])
    assert out_fcc == out_ref, f"lua mismatch:\nfcc={out_fcc!r}\nref={out_ref!r}"
    print("PASS: lua (vm, closures, tables, metamethods, coroutines, pcall/setjmp, patterns, math: exact match vs clang-22)")
    return True


def test_sqlite(tmp: Path) -> bool:
    sdir = UPSTREAM / "sqlite"
    w_fcc = tmp / "sqlite_fcc"
    w_ref = tmp / "sqlite_ref"
    w_fcc.mkdir()
    w_ref.mkdir()

    flags = ["-DSQLITE_THREADSAFE=0", "-DSQLITE_OMIT_LOAD_EXTENSION", f"-I{sdir}"]
    run_cmd([FCC, "-c", *flags, str(sdir / "sqlite3.c"), "-o", str(w_fcc / "sqlite3.o")], timeout=300)
    run_cmd([FCC, "-c", *flags, str(sdir / "shell.c"), "-o", str(w_fcc / "shell.o")], timeout=120)
    run_cmd([FCC, str(w_fcc / "sqlite3.o"), str(w_fcc / "shell.o"), "-lm", "-o", str(w_fcc / "sqlite3")], timeout=120)

    clang_executable(w_ref / "sqlite3", [sdir / "sqlite3.c", sdir / "shell.c"], flags, ["-lm"])

    sql = """
CREATE TABLE items(id INTEGER PRIMARY KEY, cat TEXT, name TEXT, price REAL, qty INT);
INSERT INTO items VALUES
  (1, 'fruit', 'apple', 1.25, 10),
  (2, 'fruit', 'banana', 0.75, 20),
  (3, 'veg', 'carrot', 0.50, 15),
  (4, 'veg', 'broccoli', 2.10, 5),
  (5, 'grain', 'rice', 3.40, 8);
CREATE INDEX idx_cat ON items(cat);
SELECT cat, count(*), sum(qty), round(sum(price * qty), 2)
  FROM items GROUP BY cat ORDER BY cat;
WITH RECURSIVE fib(n, a, b) AS (
  SELECT 0, 0, 1
  UNION ALL
  SELECT n+1, b, a+b FROM fib WHERE n < 15
)
SELECT group_concat(a, ',') FROM fib;
UPDATE items SET qty = qty + 2 WHERE cat = 'fruit';
DELETE FROM items WHERE price < 0.60;
SELECT id, name, qty FROM items ORDER BY price DESC, id ASC;
"""
    out_fcc = run_cmd([str(w_fcc / "sqlite3"), ":memory:"], input_bytes=sql.encode())
    out_ref = run_cmd([str(w_ref / "sqlite3"), ":memory:"], input_bytes=sql.encode())
    assert out_fcc == out_ref, f"sqlite3 mismatch:\nfcc={out_fcc!r}\nref={out_ref!r}"
    print("PASS: sqlite3 (amalgamation + shell + B-tree/index/CTE/aggregates: exact match vs clang-22)")
    return True


def clang_executable(output, sources, flags, libraries=(), *, timeout=180):
    # refcache supports a single preprocessing input per invocation. Keep
    # temporary object names distinct even when two sources share a basename.
    compile_flags = [*flags, *(flag for flag in libraries if flag == "-pthread")]
    objects = []
    for i, source in enumerate(sources):
        if Path(source).suffix in (".c", ".S", ".s"):
            obj = output.with_name(f"{output.name}-{i}.o")
            run_cmd([CLANG, "-w", *compile_flags, "-c", str(source), "-o", str(obj)], timeout=timeout)
            objects.append(str(obj))
        else:
            objects.append(str(source))
    run_cmd([CLANG, *objects, *libraries, "-o", str(output)], timeout=timeout)


def build_pair(tmp, name, sources, flags=(), libraries=(), *, timeout=180):
    paths = [tmp / f"{name}-{tag}" for tag in ("fcc", "clang")]
    run_cmd([FCC, "-std=gnu23", *flags, *map(str, sources),
             *libraries, "-o", str(paths[0])], timeout=timeout)
    clang_executable(paths[1], sources, ["-std=gnu23", *flags], libraries, timeout=timeout)
    return paths


def compare_output(paths, args=(), *, cwd=None, input_bytes=None, both_streams=False):
    actual, expected = [run_cmd([str(exe), *args], cwd=cwd, input_bytes=input_bytes,
                                both_streams=both_streams)
                        for exe in paths]
    if actual != expected:
        raise AssertionError(f"{paths[0].name} {args} output mismatch: "
                             f"fcc={repr(actual)[:600]}, clang={repr(expected)[:600]}")
    return actual


def test_cjson(tmp):
    tests = UPSTREAM / "cjson/tests"
    unity = tests / "unity/src/unity.c"
    cases = ("parse_examples", "parse_number", "parse_string", "print_number",
             "compare_tests", "cjson_add", "readme_examples")
    for name in cases:
        # These tests include cJSON.c through common.h to exercise static helpers.
        paths = build_pair(tmp, f"cjson-{name}", [tests / f"{name}.c", unity],
                           libraries=["-lm"])
        compare_output(paths, cwd=tests)
    print(f"PASS: cJSON ({len(cases)} upstream Unity executables)")
    return True


def test_miniz(tmp):
    src = UPSTREAM / "miniz"
    # A static build needs only the empty export annotation usually generated by CMake.
    (tmp / "miniz_export.h").write_text("#define MINIZ_EXPORT\n")
    files = ["miniz.c", "miniz_tdef.c", "miniz_tinfl.c", "miniz_zip.c", "examples/example1.c"]
    paths = build_pair(tmp, "miniz", [src / name for name in files],
                       flags=[f"-I{src}", f"-I{tmp}"])
    compare_output(paths)
    print("PASS: miniz (upstream compression and decompression example)")
    return True


def test_bzip2(tmp):
    src = UPSTREAM / "bzip2"
    files = ["bzip2.c", "blocksort.c", "huffman.c", "crctable.c", "randtable.c",
             "compress.c", "decompress.c", "bzlib.c"]
    paths = build_pair(tmp, "bzip2", [src / name for name in files],
                       flags=["-D_FILE_OFFSET_BITS=64"])
    for level in (1, 2, 3):
        raw = (src / f"sample{level}.ref").read_bytes()
        packed = (src / f"sample{level}.bz2").read_bytes()
        assert compare_output(paths, ["-c", f"-{level}"], input_bytes=raw) == packed
        args = ["-dsc"] if level == 3 else ["-dc"]
        assert compare_output(paths, args, input_bytes=packed) == raw
    print("PASS: bzip2 (three upstream sample streams in both directions)")
    return True


def test_zstd(tmp):
    src = UPSTREAM / "zstd"
    lib = src / "lib"
    sources = [p for part in ("common", "compress", "decompress", "dictBuilder")
               for p in sorted((lib / part).glob("*.c"))]
    # Build the same assembly routine for both compilers. The two C
    # libraries still select it through zstd's usual CPU detection.
    asm = tmp / "huf_decompress_amd64.o"
    run_cmd([CLANG, "-c", str(lib / "decompress/huf_decompress_amd64.S"), "-o", str(asm)])
    paths = build_pair(tmp, "zstd-roundtrip", [src / "tests/roundTripCrash.c", *sources, asm],
                       [f"-I{lib}", f"-I{lib / 'common'}", "-DZSTD_MULTITHREAD"],
                       ["-pthread"], timeout=300)
    sample = tmp / "zstd-payload"
    sample.write_bytes((src / "README.md").read_bytes() * 1024 +
                       bytes(range(256)) * 3701)
    for args in ((), ("--cctxParams",)):
        for payload in (src / "README.md", sample):
            assert compare_output(paths, [*args, str(payload)], both_streams=True) == (
                b"", b"no pb detected\n")
    print("PASS: zstd (four upstream one-shot and streaming round-trips)")
    return True


def test_duktape(tmp):
    src = UPSTREAM / "duktape"
    print_alert = src / "extras/print-alert"
    sources = [src / "src/duktape.c", src / "examples/cmdline/duk_cmdline.c",
               print_alert / "duk_print_alert.c"]
    paths = build_pair(tmp, "duktape", sources,
                       [f"-I{src / 'src'}", f"-I{print_alert}", "-DDUK_CMDLINE_PRINTALERT_SUPPORT"],
                       ["-lm"], timeout=300)
    guide = src / "examples/guide"
    compare_output(paths, [str(guide / "fib.js")], both_streams=True)
    prime_sample = "print([2, 3, 4, 97, 221, 9973, 10000].filter(primeCheck).join(' '))"
    assert compare_output(paths, [str(guide / "prime.js"), "-e", prime_sample],
                          both_streams=True) == (b"2 3 97 9973\n", b"")
    print("PASS: Duktape (upstream Fibonacci and prime examples)")
    return True


def test_mbedtls(tmp):
    src = UPSTREAM / "mbedtls"
    library = src / "library"
    paths = build_pair(tmp, "mbedtls-selftest", [src / "programs/test/selftest.c",
                                                 *sorted(library.glob("*.c"))],
                       [f"-I{src / 'include'}", f"-I{library}"], timeout=300)
    stdout, _ = compare_output(paths, both_streams=True)
    assert b"[ All tests PASS ]" in stdout
    print("PASS: Mbed TLS (upstream selftest)")
    return True


def test_oniguruma(tmp):
    src = UPSTREAM / "oniguruma"
    build = tmp / "onig-config"
    run_cmd(["cmake", "-S", str(src), "-B", str(build), f"-DCMAKE_C_COMPILER={CLANG}",
             "-DBUILD_SHARED_LIBS=OFF"])
    # Follow the same translation units as the upstream static target.
    listed = re.search(r"set\(_SRCS ([^)]+)\)", (src / "CMakeLists.txt").read_text())
    sources = [src / name for name in listed[1].split() if name.endswith(".c")]
    flags = [f"-I{src / 'src'}", f"-I{build}", "-DONIG_STATIC"]
    cases = ("test_utf8", "test_syntax", "test_options", "test_regset", "test_back",
             "testc", "testu")
    binaries = {case: [] for case in cases}
    for compiler, tag in ((FCC, "fcc"), (CLANG, "clang")):
        out = tmp / f"onig-{tag}"
        out.mkdir()
        options = [compiler, "-std=gnu23", *flags, *(["-w"] if compiler == CLANG else [])]
        objects = []
        for source in sources:
            obj = out / f"{source.stem}.o"
            run_cmd([*options, "-c", str(source), "-o", str(obj)])
            objects.append(str(obj))
        for case in cases:
            exe = out / case
            run_cmd([*options, str(src / f"test/{case}.c"), *objects, "-o", str(exe)])
            binaries[case].append(exe)
    for case in cases:
        compare_output(binaries[case], cwd=tmp, both_streams=True)
    print(f"PASS: Oniguruma ({len(cases)} upstream regex tests)")
    return True


def test_libpng(tmp):
    src = UPSTREAM / "libpng"
    zlib = UPSTREAM / "zlib"
    # Upstream's prebuilt configuration is the standard static C build.
    (tmp / "pnglibconf.h").write_bytes((src / "scripts/pnglibconf.h.prebuilt").read_bytes())
    sources = sorted(src.glob("png*.c")) + [zlib / name for name in ZLIB_SOURCES]
    paths = build_pair(tmp, "pngtest", sources, [f"-I{tmp}", f"-I{zlib}", "-DHAVE_UNISTD_H"],
                       ["-lm"], timeout=300)
    for tag, exe in zip(("fcc", "clang"), paths):
        cwd = tmp / f"pngtest-{tag}-output"
        cwd.mkdir()
        (cwd / "pngtest.png").symlink_to(src / "pngtest.png")
        output = run_cmd([str(exe)], cwd=cwd, both_streams=True)
        result = (cwd / "pngout.png").read_bytes()
        if tag == "fcc":
            fcc_output, fcc_result = output, result
        else:
            assert output == fcc_output, "libpng pngtest output differs"
            assert result == fcc_result, "libpng pngtest generated different image bytes"
    print("PASS: libpng with zlib (upstream pngtest and byte-identical output PNG)")
    return True


def checkout_pair(tmp, name):
    # Projects that build in their source tree need one writable copy per compiler.
    for compiler, tag in ((FCC, "fcc"), (CLANG, "clang")):
        yield compiler, shutil.copytree(UPSTREAM / name, tmp / f"{name}-{tag}")


def check_results(name, actual, expected):
    if actual != expected:
        raise AssertionError(f"{name} output mismatch:\n"
                             f"fcc={repr(actual)[:800]}\nclang={repr(expected)[:800]}")


def test_quickjs(tmp):
    tests = ("tests/test_closure.js", "tests/test_language.js", "tests/test_builtin.js",
             "tests/test_loop.js", "tests/test_bigint.js", "tests/test_cyclic_import.js",
             "tests/test_worker.js", "tests/test_std.js", "tests/test_bjson.js",
             "examples/test_point.js")
    results = []
    for compiler, src in checkout_pair(tmp, "quickjs"):
        version = (src / "VERSION").read_text().strip()
        defines = f'-std=gnu23 -D_GNU_SOURCE -DCONFIG_VERSION=\\"{version}\\"'
        run_cmd(["make", f"-j{PROJECT_JOBS}", f"CC={compiler}", f"DEFINES={defines}", "qjs",
                 "tests/bjson.so", "examples/point.so"], cwd=src, timeout=600)
        results.append({test: run_cmd(["./qjs", *(["--std"] if test.endswith("test_builtin.js") else []), test],
                                      cwd=src, both_streams=True) for test in tests})
    for test in tests:
        check_results(test, results[0][test], results[1][test])
    print(f"PASS: QuickJS ({len(tests)} upstream JS tests, including native modules)")
    return True


def test_redis(tmp):
    src = UPSTREAM / "redis/src"
    fpconv = src.parent / "deps/fpconv"
    files = ("sds.c", "ziplist.c", "listpack.c", "dict.c", "util.c", "zmalloc.c",
             "redisassert.c", "sha256.c", "siphash.c", "mt19937-64.c", "adlist.c", "strl.c")
    paths = build_pair(tmp, "redis-standalone",
                       [ROOT / "test/cc_run/redis_standalone.c", fpconv / "fpconv_dtoa.c",
                        *(src / file for file in files)],
                       ["-DREDIS_TEST", f"-I{src}", f"-I{fpconv}"], ["-lm", "-pthread"])
    stdout, _ = compare_output(paths, both_streams=True)
    assert b"standalone collections passed\n" in stdout
    print("PASS: Redis (upstream sds and util tests, plus standalone ziplist/listpack/dict checks)")
    return True


def test_curl(tmp):
    payload = bytes(range(256)) * 32
    file = tmp / "local data"
    file.write_bytes(payload)
    paths = []
    for compiler, tag in ((FCC, "fcc"), (CLANG, "clang")):
        build = tmp / f"curl-{tag}"
        run_cmd(["cmake", "-S", str(UPSTREAM / "curl"), "-B", str(build),
                 f"-DCMAKE_C_COMPILER={compiler}", "-DCMAKE_C_FLAGS=-std=gnu23",
                 "-DBUILD_TESTING=OFF", "-DBUILD_SHARED_LIBS=OFF", "-DCURL_ENABLE_SSL=OFF",
                 "-DCURL_USE_LIBPSL=OFF", "-DCURL_BROTLI=OFF", "-DCURL_ZSTD=OFF",
                 "-DENABLE_ARES=OFF", "-DENABLE_MANUAL=OFF"], timeout=600)
        run_cmd(["cmake", "--build", str(build), "-j", str(PROJECT_JOBS)], timeout=1200)
        paths.append(build / "src/curl")
    for range_arg, expected in (([], payload), (["--range", "250-263"], payload[250:264])):
        args = ["--disable", "--silent", "--show-error", *range_arg, file.as_uri()]
        assert compare_output(paths, args, both_streams=True) == (expected, b"")
    print("PASS: curl (local binary transfer and byte-range request)")
    return True


def test_cpython(tmp):
    src = UPSTREAM / "cpython"
    # Per-test results are deterministic. unittest's normal summary includes timings.
    runner = r"""
import sys
import unittest

class Result(unittest.TestResult):
    def addSuccess(self, test):
        print(test.id(), 'ok')
        super().addSuccess(test)

    def addSkip(self, test, reason):
        print(test.id(), 'skip', reason)
        super().addSkip(test, reason)

suite = unittest.defaultTestLoader.loadTestsFromNames(sys.argv[1:])
result = Result()
suite.run(result)
for _, details in result.errors + result.failures:
    print(details, file=sys.stderr)
print('ran', result.testsRun)
sys.exit(not result.wasSuccessful() or result.testsRun == len(result.skipped))
"""
    script = tmp / "run_cpython_tests.py"
    script.write_text(runner)
    modules = ("test.test_bisect", "test.test_heapq", "test.test_itertools", "test.test_struct")
    results = []
    for compiler, tag in ((FCC, "fcc"), (CLANG, "clang")):
        build = tmp / f"cpython-{tag}"
        build.mkdir()
        run_cmd([str(src / "configure"), "--without-ensurepip"], cwd=build,
                env={"CC": f"{compiler} -std=gnu23"}, timeout=1200)
        # Use C23 for both builds even when configure adds its default C11 flag.
        makefile = build / "Makefile"
        makefile.write_text(makefile.read_text().replace(" -std=c11", " -std=gnu23"))
        run_cmd(["make", f"-j{PROJECT_JOBS}"], cwd=build, timeout=2400)
        results.append(run_cmd(["./python", "-B", str(script), *modules], cwd=build,
                               env={"PYTHONHASHSEED": "0"}, both_streams=True))
    check_results("CPython Lib/test", *results)
    print(f"PASS: CPython ({len(modules)} upstream Lib/test modules)")
    return True


def test_git(tmp):
    tests = ("t0001-init.sh", "t0008-ignores.sh", "t0060-path-utils.sh")
    results = []
    work = tmp / "git-work"
    work.mkdir()
    for compiler, src in checkout_pair(tmp, "git"):
        run_cmd(["make", f"-j{PROJECT_JOBS}", f"CC={compiler} -std=gnu23", "NO_GETTEXT=YesPlease",
                 "NO_TCLTK=YesPlease", "NO_CURL=YesPlease", "NO_OPENSSL=YesPlease"],
                cwd=src, timeout=2400)
        results.append({test: run_cmd(["sh", test, f"--root={work}"], cwd=src / "t", both_streams=True)
                        for test in tests})
    for test in tests:
        check_results(test, results[0][test], results[1][test])
    print(f"PASS: Git ({len(tests)} upstream shell TAP suites)")
    return True


def test_musl(tmp):
    src = UPSTREAM / "musl"
    tests = UPSTREAM / "libc-test" / "src"
    cases = ("fnmatch", "inet_pton", "memstream", "pthread_cond", "qsort", "setjmp",
             "snprintf", "strtod", "tls_align")
    results = []
    for compiler, label in ((FCC, "fcc"), (CLANG, "clang")):
        build = tmp / f"musl-{label}"
        build.mkdir()
        prefix = build / "install"
        wrapper = build / "cc"
        # Both builds use the same assembler for musl's hand-written .s/.S
        # startup and syscall code. Every C source goes to its test compiler.
        wrapper.write_text(
            '#!/bin/bash\n'
            'asm=false\nargs=()\nfor arg do\n'
            '  case "$arg" in *.s|*.S) asm=true ;; esac\n'
            '  if [[ $arg != -fno-tree-loop-distribute-patterns ]]; then args+=("$arg"); fi\n'
            'done\n'
            f'if $asm; then exec {shlex.quote(CLANG)} "${{args[@]}}"; fi\n'
            f'exec {shlex.quote(compiler)} "$@"\n')
        wrapper.chmod(0o755)
        run_cmd([str(src / "configure"), "--disable-shared", "--target=x86_64-linux-musl",
                 f"--prefix={prefix}"], cwd=build, env={"CC": str(wrapper), "CFLAGS": "-O0 -g0"})
        # x86-64 host tools can archive these objects. The target name in
        # configure does not require an installed musl toolchain.
        run_cmd(["make", f"-j{PROJECT_JOBS}", "AR=ar", "RANLIB=ranlib", "install"],
                cwd=build, timeout=1800)
        runtime = run_cmd(["gcc", "-print-libgcc-file-name"]).decode().strip()
        lib = prefix / "lib"
        flags = ["-std=gnu17", "-nostdinc", "-nostdlib", "-static", "-fno-pic", "-no-pie",
                 "-isystem", str(prefix / "include"), f"-I{tests / 'common'}",
                 str(lib / "crt1.o"), str(lib / "crti.o")]
        probe = ROOT / "test/cc_run/projects/musl_link.c"
        for case in cases + ("musl_link",):
            inputs = [probe] if case == "musl_link" else [
                tests / "functional" / f"{case}.c", tests / "common/print.c"]
            if case == "qsort":
                inputs.append(tests / "common/rand.c")
            if case == "tls_align":
                inputs.append(tests / "functional/tls_align_dso.c")
            exe = build / case
            run_cmd([compiler, *flags, *map(str, inputs), str(lib / "libc.a"), runtime,
                     str(lib / "crtn.o"), "-o", str(exe)], cwd=build)
        results.append([run_cmd([str(build / case)], cwd=build, timeout=30, both_streams=True)
                        for case in cases + ("musl_link",)])
    check_results("musl static libc-test and linked program", *results)
    print(f"PASS: musl ({len(cases)} static libc-test programs and a linked client)")
    return True


def test_lz4(tmp):
    results = []
    payload = b"abcabc\x00\xff" * 15000 + bytes(range(256))
    for compiler, src in checkout_pair(tmp, "lz4"):
        run_cmd(["make", f"-j{PROJECT_JOBS}", "-C", "programs", "lz4", f"CC={compiler}", "CFLAGS=-O0 -g0"],
                cwd=src, timeout=600)
        tool = str(src / "programs/lz4")
        frames = [run_cmd([tool, "-q", level, "-c"], input_bytes=payload) for level in ("-1", "-9")]
        for frame in frames:
            assert run_cmd([tool, "-q", "-d", "-c"], input_bytes=frame) == payload
        results.append(frames)
    check_results("LZ4 frames", *results)
    print("PASS: lz4 (byte-identical frames and round-trips at two compression levels)")
    return True


def cmake_project(tmp, project, targets, options=()):
    # CMake identifies fcc by its GNU compatibility macros. Tell it how
    # to select C11 without changing the advertised __GNUC__ for headers.
    rules = tmp / "fcc-cmake-rules.cmake"
    rules.write_text('set(CMAKE_C11_STANDARD_COMPILE_OPTION "-std=c11")\n'
                     'set(CMAKE_C11_EXTENSION_COMPILE_OPTION "-std=gnu11")\n')
    for compiler, label in ((FCC, "fcc"), (CLANG, "clang")):
        build = tmp / f"{project}-{label}"
        extra = [f"-DCMAKE_USER_MAKE_RULES_OVERRIDE={rules}"] if label == "fcc" else []
        run_cmd(["cmake", "-S", str(UPSTREAM / project), "-B", str(build),
                 f"-DCMAKE_C_COMPILER={compiler}", "-DCMAKE_C_FLAGS=-O0 -g0",
                 *extra, *options], timeout=600)
        run_cmd(["cmake", "--build", str(build), "-j", str(PROJECT_JOBS), "--target", *targets], timeout=1800)
        yield build


def test_xz(tmp):
    payload = (b"exact LZMA2 stream\x00" + bytes(range(256))) * 2000
    results = []
    for build in cmake_project(tmp, "xz", ("xz",), ("-DXZ_NLS=OFF",)):
        tool = str(build / "xz")
        streams = []
        for check in ("crc32", "crc64", "sha256"):
            encoded = run_cmd([tool, "-T1", "-6", f"--check={check}", "-c"], input_bytes=payload)
            assert run_cmd([tool, "-dc"], input_bytes=encoded) == payload
            streams.append(encoded)
        results.append(streams)
    check_results("XZ streams", *results)
    print("PASS: xz/liblzma (byte-identical streams and round-trips with three integrity checks)")
    return True


def test_libuv(tmp):
    cases = ("loop_alive", "timer", "async", "fs_fstat", "thread_create", "walk_handles")
    results = []
    for build in cmake_project(tmp, "libuv", ("uv_run_tests_a",),
                               ("-DBUILD_TESTING=ON", "-DLIBUV_BUILD_TESTS=ON")):
        results.append([run_cmd([str(build / "uv_run_tests_a"), case], cwd=build, both_streams=True)
                        for case in cases])
    check_results("libuv upstream tests", *results)
    print(f"PASS: libuv ({len(cases)} upstream tests)")
    return True


def nginx_requests(binary, src):
    payload = b"nginx static compiler differential\n"
    root = src / "html"
    index = root / "index.html"
    index.write_bytes(payload)
    os.utime(index, (1700000000, 1700000000))
    with socket.socket() as reservation:
        reservation.bind(("127.0.0.1", 0))
        port = reservation.getsockname()[1]
    configuration = src / "fcc-test.conf"
    configuration.write_text(f"""
daemon off;
master_process off;
error_log stderr;
pid fcc-test.pid;
events {{}}
http {{
    access_log off;
    server {{
        listen 127.0.0.1:{port};
        location / {{ root "{root}"; }}
    }}
}}
""")
    proc = subprocess.Popen([str(binary), "-p", str(src), "-c", str(configuration)], env=ENV,
                            stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
    try:
        deadline = time.monotonic() + 10
        while True:
            try:
                with socket.create_connection(("127.0.0.1", port), timeout=0.3):
                    break
            except OSError:
                if proc.poll() is not None or time.monotonic() >= deadline:
                    raise RuntimeError("nginx failed to start")
                time.sleep(0.05)

        results = []
        for method, path, headers in (("GET", "/index.html", {}), ("HEAD", "/index.html", {}),
                                      ("GET", "/index.html", {"Range": "bytes=0-4"}),
                                      ("GET", "/missing.html", {})):
            conn = http.client.HTTPConnection("127.0.0.1", port, timeout=5)
            try:
                conn.request(method, path, headers=headers)
                response = conn.getresponse()
                fields = [(k, v) for k, v in response.getheaders() if k.lower() != "date"]
                results.append((response.status, fields, response.read()))
            finally:
                conn.close()
        assert [status for status, _, _ in results] == [200, 200, 206, 404]
        assert results[0][2] == payload and results[1][2] == b"" and results[2][2] == payload[:5]
        return results
    finally:
        proc.terminate()
        try:
            _, stderr = proc.communicate(timeout=5)
        except subprocess.TimeoutExpired:
            proc.kill()
            _, stderr = proc.communicate()
        if proc.returncode not in (0, -15):
            print(stderr.decode("utf-8", "replace"), file=sys.stderr)


def test_nginx(tmp):
    results = []
    for compiler, src in checkout_pair(tmp, "nginx"):
        run_cmd(["sh", "configure", f"--with-cc={compiler}", "--without-http_rewrite_module",
                 "--without-http_gzip_module", "--with-cc-opt=-O0 -g0"], cwd=src, timeout=600)
        run_cmd(["make", f"-j{PROJECT_JOBS}"], cwd=src, timeout=1800)
        results.append(nginx_requests(src / "objs/nginx", src))
    check_results("nginx HTTP responses", *results)
    print("PASS: nginx (localhost static GET, HEAD, byte range and 404 responses)")
    return True


def test_postgresql(tmp):
    results = []
    cases = ("test_setup", "boolean", "int2", "int4", "int8", "text")
    for compiler, label in ((FCC, "fcc"), (CLANG, "clang")):
        build = tmp / f"postgresql-{label}"
        build.mkdir()
        run_cmd([str(UPSTREAM / "postgresql/configure"), "--without-icu", "--without-readline",
                 "--without-zlib", "--without-llvm", "--without-lz4", "--without-zstd"],
                cwd=build, env={"CC": compiler, "CFLAGS": "-O0 -g0"}, timeout=900)
        run_cmd(["make", f"-j{PROJECT_JOBS}"], cwd=build, timeout=3600)
        regress = build / "src/test/regress"
        try:
            run_cmd(["make", "check-tests", f"TESTS={' '.join(cases)}"],
                    cwd=regress, env={"LC_ALL": "C", "PGTZ": "UTC",
                                      "TMPDIR": ENV["FCC_PROJECT_PG_TMPDIR"]}, timeout=1200)
        except RuntimeError:
            for name in ("regression.diffs", "log/initdb.log", "log/postmaster.log"):
                path = regress / name
                if path.is_file() and path.stat().st_size:
                    print(f"{label} {name}\n{path.read_text(errors='replace')[-4000:]}", file=sys.stderr)
            raise
        results.append([(regress / f"results/{case}.out").read_bytes() for case in cases])
    check_results("PostgreSQL SQL results", *results)
    print(f"PASS: PostgreSQL ({len(cases)} upstream make check tests)")
    return True


def test_openssl(tmp):
    outputs = []
    tests = ("sha_test", "bn_internal_test")
    test_env = {"OPENSSL_TEST_RAND_SEED": "42"}
    for compiler, label in ((FCC, "fcc"), (CLANG, "clang")):
        build = tmp / f"openssl-{label}"
        build.mkdir()
        run_cmd(["perl", str(UPSTREAM / "openssl/Configure"), "linux-x86_64",
                 f"--prefix={build / 'install'}", "no-asm", "no-shared",
                 "no-module", "-O0"], cwd=build, env={"CC": compiler})
        run_cmd(["make", f"-j{PROJECT_JOBS}", "build_generated"], cwd=build, timeout=180)
        run_cmd(["make", f"-j{PROJECT_JOBS}", "link-utils", "util/wrap.pl", *[f"test/{name}" for name in tests]],
                cwd=build, timeout=1800)
        run_cmd(["make", "run_tests", "TESTS=test_sha test_internal_bn"], cwd=build, env=test_env, timeout=120)
        outputs.append([run_cmd([str(build / "test" / name)], cwd=build, env=test_env, both_streams=True)
                        for name in tests])
    check_results("OpenSSL SHA and bignum test output", *outputs)
    print("PASS: OpenSSL with no assembly (SHA and internal bignum tests)")
    return True


def test_perl(tmp):
    outputs = []
    tests = ("arith2", "append", "array", "blocks", "concat2")
    for compiler, label in ((FCC, "fcc"), (CLANG, "clang")):
        build = tmp / f"perl-{label}"
        shutil.copytree(UPSTREAM / "perl", build)
        run_cmd(["sh", "Configure", "-des", f"-Dcc={compiler}", "-Doptimize=-O0",
                 f"-Dprefix={build / 'install'}"], cwd=build, timeout=600)
        run_cmd(["make", f"-j{PROJECT_JOBS}", "miniperl", "lib/Config.pm"], cwd=build, timeout=1200)
        report = [run_cmd(["../miniperl", "-I../lib", f"op/{name}.t"], cwd=build / "t",
                          env={"PERL_HASH_SEED": "0"}, both_streams=True) for name in tests]
        for stdout, stderr in report:
            failures = [line for line in stdout.splitlines()
                        if line.startswith(b"not ok ") and b" # TODO " not in line]
            assert b"1.." in stdout and not failures, (stdout, stderr)
        outputs.append(report)
    check_results("miniperl core TAP", *outputs)
    print("PASS: miniperl (five upstream TAP scripts)")
    return True


def test_tcl(tmp):
    reports = []
    for compiler, label in ((FCC, "fcc"), (CLANG, "clang")):
        build = tmp / f"tcl-{label}"
        build.mkdir()
        env = {"CC": compiler, "CFLAGS": "-O0", "LC_ALL": "C", "TZ": "UTC"}
        run_cmd([str(UPSTREAM / "tcl/unix/configure"), "--disable-shared", "--disable-symbols"],
                cwd=build, env=env, timeout=300)
        run_cmd(["make", f"-j{PROJECT_JOBS}", "tcltest"], cwd=build, env=env, timeout=1200)
        # tcltest prints its working directory and timestamps. Replace these
        # fields so test names, results and skip counts compare exactly.
        report = run_cmd(["./tcltest", str(UPSTREAM / "tcl/tests/all.tcl"), "-verbose", "ps",
                          "-file", "append.test apply.test binary.test concat.test dict.test"],
                         cwd=build, env={**env, "TCL_LIBRARY": str(UPSTREAM / "tcl/library")})
        assert re.search(rb"Failed\s+0(?:\s|$)", report), report[-3000:]
        reports.append(re.sub(rb"(?m)^(Tests began at|Tests ended at) .*$", rb"\1",
                              report.replace(os.fsencode(build), b"<build>")))
    check_results("Tcl tcltest output", *reports)
    print("PASS: Tcl (five upstream tcltest files)")
    return True


def test_busybox(tmp):
    reports = []
    applets = ("printf", "cut", "sort")
    enabled = ("PRINTF", "CUT", "SORT", "FEATURE_SORT_BIG", "FEATURE_SORT_OPTIMIZE_MEMORY")
    for compiler, label in ((FCC, "fcc"), (CLANG, "clang")):
        build = tmp / f"busybox-{label}"
        shutil.copytree(UPSTREAM / "busybox", build)
        # Kbuild invokes CC for standalone .S even when the routines are disabled.
        cc = build / "test-cc"
        # Reject the GCC-only options in Kbuild's exit-status probes so that
        # the shared assembly path receives compatible flags on both builds.
        # The final link also requests two diagnostic-only GNU ld switches.
        # Keep the map output, which both linkers support.
        cc.write_text("#!/bin/sh\nassembly=no\nfor arg do\n  shift\n  case $arg in\n"
                      "    -malign-data=abi|-fno-guess-branch-probability) exit 1;;\n"
                      "    -Wl,--warn-common|-Wl,--verbose) continue;;\n"
                      "    *.S) assembly=yes;;\n  esac\n  set -- \"$@\" \"$arg\"\ndone\n"
                      f"if [ \"$assembly\" = yes ]; then exec {shlex.quote(CLANG)} \"$@\"; fi\n"
                      f"exec {shlex.quote(compiler)} \"$@\"\n")
        cc.chmod(0o755)
        run_cmd(["make", "allnoconfig"], cwd=build)
        config = build / ".config"
        text = config.read_text()
        for name in enabled:
            text = text.replace(f"# CONFIG_{name} is not set\n", f"CONFIG_{name}=y\n")
        config.write_text(text)
        run_cmd(["make", "oldconfig"], cwd=build, input_bytes=b"\n" * 256)
        run_cmd(["make", f"-j{PROJECT_JOBS}", f"CC={cc}", "busybox"], cwd=build, timeout=600)
        report = run_cmd(["sh", "runtest", *applets], cwd=build / "testsuite",
                         env={"LC_ALL": "C"}, both_streams=True)
        assert report[0].count(b"PASS: ") >= len(applets), report
        reports.append(report)
    check_results("BusyBox applet tests", *reports)
    print("PASS: BusyBox (printf, cut and sort upstream tests)")
    return True


def test_libjpeg_turbo(tmp):
    outputs = []
    for build in cmake_project(tmp, "libjpeg-turbo", ["tjunittest-static", "cjpeg-static", "djpeg-static"],
                               ("-DWITH_SIMD=OFF", "-DENABLE_SHARED=OFF", "-DWITH_JAVA=OFF",
                                "-DCMAKE_C_FLAGS=-O0 -g0 -DSPNG_DISABLE_OPT")):
        output = run_cmd([str(build / "tjunittest-static")], cwd=build, both_streams=True)
        image = UPSTREAM / "libjpeg-turbo/testimages/testorig.ppm"
        compressed = run_cmd([str(build / "cjpeg-static"), "-quality", "85", str(image)], cwd=build)
        decoded = run_cmd([str(build / "djpeg-static")], cwd=build, input_bytes=compressed)
        outputs.append((output, compressed, decoded))
    check_results("libjpeg-turbo tests and image bytes", *outputs)
    print("PASS: libjpeg-turbo with no SIMD (tjunittest, cjpeg and djpeg)")
    return True


def test_ffmpeg(tmp):
    outputs = []
    tests = ("base64", "crc", "avstring")
    for compiler, label in ((FCC, "fcc"), (CLANG, "clang")):
        build = tmp / f"ffmpeg-{label}"
        build.mkdir()
        run_cmd([str(UPSTREAM / "ffmpeg/configure"), f"--cc={compiler}", "--disable-asm",
                 "--disable-everything", "--disable-autodetect", "--disable-optimizations",
                 "--disable-programs", "--disable-doc"], cwd=build, timeout=300)
        run_cmd(["make", f"-j{PROJECT_JOBS}", *[f"fate-{name}" for name in tests]], cwd=build, timeout=1200)
        outputs.append([run_cmd([str(build / "libavutil/tests" / name)], cwd=build, both_streams=True)
                        for name in tests])
    check_results("FFmpeg libavutil FATE output", *outputs)
    print("PASS: FFmpeg with no assembly (three libavutil FATE tests)")
    return True


PROJECTS = {
    "zlib": test_zlib, "lua": test_lua, "sqlite": test_sqlite, "cjson": test_cjson,
    "miniz": test_miniz, "bzip2": test_bzip2, "zstd": test_zstd, "duktape": test_duktape,
    "mbedtls": test_mbedtls, "oniguruma": test_oniguruma, "libpng": test_libpng,
    "quickjs": test_quickjs, "redis": test_redis, "curl": test_curl, "cpython": test_cpython,
    "git": test_git, "musl": test_musl, "lz4": test_lz4, "xz": test_xz, "libuv": test_libuv,
    "nginx": test_nginx, "postgresql": test_postgresql, "openssl": test_openssl,
    "perl": test_perl, "tcl": test_tcl, "busybox": test_busybox,
    "libjpeg-turbo": test_libjpeg_turbo, "ffmpeg": test_ffmpeg,
}

# Give larger builds first use of the parallel slots. Small serial projects
# fill spare slots without dividing the jobs of the largest build at -j3.
LARGE = ("ffmpeg", "postgresql", "cpython", "git", "perl", "nginx", "musl", "busybox",
         "openssl", "tcl", "libjpeg-turbo", "curl", "xz", "libuv", "quickjs", "lz4")


def stop_group(proc):
    # make or an upstream test can leave children after its own failure.
    try:
        os.killpg(proc.pid, signal.SIGKILL)
    except ProcessLookupError:
        pass
    proc.wait()


def run_projects(wanted, jobs, log_dir, tmp, pg_tmp):
    parallel = deque(name for name in LARGE if name in wanted)
    serial = deque(name for name in PROJECTS if name in wanted and name not in LARGE)
    running = {}
    passed = 0
    try:
        while parallel or serial or running:
            free = jobs - sum(weight for _, weight, _ in running.values())
            if len(running) < 2 and free and (parallel or serial):
                if parallel and (free >= 2 or not running):
                    # At six jobs split the budget between two large builds.
                    weight = (jobs + 1) // 2 if jobs >= 4 and len(parallel) > 1 else jobs
                    if serial and jobs > 1:
                        weight = min(weight, jobs - 1)
                    weight = min(weight, free)
                    name = parallel.popleft()
                elif serial:
                    weight = 1
                    name = serial.popleft()
                else:
                    name = None
            else:
                name = None
            if name is not None:
                work = tmp / name
                work.mkdir()
                log_path = log_dir / f"{name}.log"
                worker_env = {**ENV, "TMPDIR": str(work), "FAS_LD_THREADS": "1",
                              "MAKEFLAGS": f"-j{weight}", "MFLAGS": "", "GNUMAKEFLAGS": "",
                              "CMAKE_BUILD_PARALLEL_LEVEL": str(weight)}
                if name == "postgresql":
                    worker_env["FCC_PROJECT_PG_TMPDIR"] = str(pg_tmp)
                with log_path.open("wb") as log:
                    proc = subprocess.Popen(
                        [sys.executable, __file__, "--worker", name, str(work), str(weight)],
                        env=worker_env,
                        stdout=log, stderr=subprocess.STDOUT, start_new_session=True)
                    running[name] = (proc, weight, time.monotonic())
                continue
            for name, (proc, _, started) in list(running.items()):
                if (rc := proc.poll()) is None:
                    continue
                stop_group(proc)
                del running[name]
                shutil.rmtree(tmp / name)
                passed += rc == 0
                print(f"{'PASS' if rc == 0 else 'FAIL'} project {name} "
                      f"({time.monotonic() - started:.1f}s) {log_dir / (name + '.log')}", flush=True)
            if running:
                time.sleep(0.1)
    finally:
        for proc, _, _ in running.values():
            stop_group(proc)
    print(f"Projects: {passed}/{len(wanted)} passed")
    return passed == len(wanted)


def interrupted(signum, _frame):
    raise SystemExit(128 + signum)


def main():
    global PROJECT_JOBS, PROJECT_ROOT
    if len(sys.argv) == 5 and sys.argv[1] == "--worker":
        name, work, jobs = sys.argv[2:]
        PROJECT_JOBS = int(jobs)
        PROJECT_ROOT = Path(work)
        return PROJECTS[name](PROJECT_ROOT)
    signal.signal(signal.SIGTERM, interrupted)
    signal.signal(signal.SIGHUP, interrupted)
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--jobs", default=3, type=int, help="total build jobs, capped at six")
    parser.add_argument("--log-dir", type=Path, help="directory for per-project build and test logs")
    parser.add_argument("--exclude", action="append", default=[], choices=PROJECTS, help="omit a project")
    parser.add_argument("projects", nargs="*")
    args = parser.parse_args()
    if args.jobs < 1:
        parser.error("--jobs must be positive")
    wanted = set(args.projects) or set(PROJECTS)
    if unknown := wanted - PROJECTS.keys():
        parser.error(f"unknown projects: {', '.join(sorted(unknown))}")
    wanted.difference_update(args.exclude)
    if not wanted:
        parser.error("no projects selected")
    log_dir = args.log_dir or Path(tempfile.mkdtemp(prefix="projects-", dir=ROOT / "_build"))
    log_dir = log_dir.resolve()
    log_dir.mkdir(parents=True, exist_ok=True)
    downloads = wanted - {"zlib", "lua", "sqlite"}
    if downloads:
        proc = subprocess.Popen(["sh", str(UPSTREAM.parent / "fetch.sh"), "--projects", *sorted(downloads)],
                                start_new_session=True)
        try:
            if proc.wait() != 0:
                return False
        finally:
            stop_group(proc)
    # PostgreSQL's Unix-domain sockets need a short path. Keep only an alias
    # in /tmp, and own it here so it is cleaned on both failures and signals.
    socket_dir = (tempfile.TemporaryDirectory(prefix="fcc-pg-", dir="/tmp")
                  if "postgresql" in wanted else nullcontext(None))
    with (
        socket_dir as short,
        tempfile.TemporaryDirectory(prefix="fcc_projects_", dir=os.environ.get("TMPDIR", ROOT / "_build")) as tmp,
    ):
        pg_tmp = Path(short) / "work" if short else None
        if pg_tmp is not None:
            pg_tmp.symlink_to(Path(tmp) / "postgresql", target_is_directory=True)
        return run_projects(wanted, min(6, args.jobs), log_dir, Path(tmp), pg_tmp)


if __name__ == "__main__":
    sys.exit(0 if main() else 1)
