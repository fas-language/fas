#!/usr/bin/env python3
"""Build real-world C projects with fcc and Clang and compare exact output."""

import os
from pathlib import Path
import re
import resource
import subprocess
import sys
import tempfile

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


def run_cmd(cmd, cwd=None, input_bytes=None, timeout=180, *, both_streams=False):
    preexec = _limit_as if cmd and str(cmd[0]) == FCC else None
    cp = subprocess.run(cmd, cwd=cwd, env=ENV, input=input_bytes,
                        stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                        timeout=timeout, preexec_fn=preexec)
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
        raise RuntimeError(f"Command failed ({cp.returncode}): {command}\n{err[:1200]}")
    return (cp.stdout, cp.stderr) if both_streams else cp.stdout


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

    run_cmd([CLANG, "-w", "-DHAVE_UNISTD_H", f"-I{zdir}", str(zdir / "test/example.c"),
             *[str(zdir / s) for s in srcs], "-o", str(w_ref / "example")])

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
    run_cmd([CLANG, "-w", "-DLUA_USE_LINUX", f"-I{ldir}", *[str(p) for p in srcs], "-lm", "-ldl", "-o", str(w_ref / "lua")])

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

    run_cmd([CLANG, "-w", *flags, str(sdir / "sqlite3.c"), str(sdir / "shell.c"), "-lm", "-o", str(w_ref / "sqlite3")], timeout=120)

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


def build_pair(tmp, name, sources, flags=(), libraries=(), *, timeout=180):
    paths = [tmp / f"{name}-{tag}" for tag in ("fcc", "clang")]
    run_cmd([FCC, "-std=gnu23", *flags, *map(str, sources),
             *libraries, "-o", str(paths[0])], timeout=timeout)
    run_cmd([CLANG, "-std=gnu23", "-w", *flags, *map(str, sources),
             *libraries, "-o", str(paths[1])], timeout=timeout)
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


def main():
    projects = {"zlib": test_zlib, "lua": test_lua, "sqlite": test_sqlite,
                "cjson": test_cjson, "miniz": test_miniz, "bzip2": test_bzip2,
                "zstd": test_zstd, "duktape": test_duktape, "mbedtls": test_mbedtls,
                "oniguruma": test_oniguruma, "libpng": test_libpng}
    wanted = set(sys.argv[1:]) or set(projects)
    if unknown := wanted - projects.keys():
        sys.exit(f"Unknown projects: {', '.join(sorted(unknown))}")
    downloads = wanted - {"zlib", "lua", "sqlite"}
    if downloads:
        run_cmd(["sh", str(UPSTREAM.parent / "fetch.sh"), "--projects", *sorted(downloads)])
    with tempfile.TemporaryDirectory(prefix="fcc_projects_") as tmp:
        return all(test(Path(tmp)) for name, test in projects.items() if name in wanted)


if __name__ == "__main__":
    sys.exit(0 if main() else 1)
