#!/usr/bin/env python3
"""Differential tests for real-world C projects (zlib, lua, sqlite3): fcc vs clang-22."""

import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile

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


def run_cmd(cmd, cwd=None, input_bytes=None, timeout=180):
    cp = subprocess.run(cmd, cwd=cwd, env=ENV, input=input_bytes,
                        stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=timeout)
    if cp.returncode != 0:
        raise RuntimeError(f"Command failed ({cp.returncode}): {' '.join(cmd)}\n{cp.stderr.decode('utf-8', 'replace')[:600]}")
    return cp.stdout


def test_zlib(tmp: Path) -> bool:
    zdir = UPSTREAM / "zlib"
    srcs = [
        "adler32.c", "crc32.c", "deflate.c", "infback.c", "inffast.c",
        "inflate.c", "inftrees.c", "trees.c", "zutil.c", "compress.c",
        "uncompr.c", "gzclose.c", "gzlib.c", "gzread.c", "gzwrite.c",
    ]
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


def main():
    wanted = set(sys.argv[1:]) or {"zlib", "lua", "sqlite"}
    tmp = Path(tempfile.mkdtemp(prefix="fcc_projects_"))
    ok = True
    try:
        if "zlib" in wanted:
            ok = test_zlib(tmp) and ok
        if "lua" in wanted:
            ok = test_lua(tmp) and ok
        if "sqlite" in wanted:
            ok = test_sqlite(tmp) and ok
    finally:
        shutil.rmtree(tmp, ignore_errors=True)
    if not ok:
        sys.exit(1)


if __name__ == "__main__":
    main()
