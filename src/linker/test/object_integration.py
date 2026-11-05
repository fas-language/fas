#!/usr/bin/env python3
"""Validate our object writer with independent GNU tooling and execution."""
from pathlib import Path
import struct
import subprocess
import sys
import tempfile

writer = str(Path(sys.argv[1]).resolve())
linker = str(Path(sys.argv[2]).resolve())
with tempfile.TemporaryDirectory(prefix="fas-object-tests-") as directory:
    root = Path(directory)
    subprocess.run([writer, directory], check=True, timeout=10)
    for name in ("emitted-start.o", "emitted-support.o"):
        path = root / name
        data = path.read_bytes()
        assert struct.unpack_from("<HH", data, 16) == (1, 62)
        assert struct.unpack_from("<QQ", data, 24) == (0, 0)
        report = subprocess.run(["readelf", "-aW", str(path)], capture_output=True, check=True)
        assert b"Warning:" not in report.stderr and b"Error:" not in report.stderr
    report = subprocess.run(["readelf", "-rW", str(root / "emitted-start.o")],
                            capture_output=True, check=True).stdout
    assert b"R_X86_64_PC32" in report and b"answer - 4" in report
    assert b"R_X86_64_64" in report
    for tool, name in (("ld", "gnu"), (linker, "fas")):
        out = root / name
        subprocess.run([tool, "-o", str(out), str(root / "emitted-start.o"),
                        str(root / "emitted-support.o")], check=True, timeout=10)
        assert subprocess.run([str(out)], timeout=3).returncode == 42
    assert subprocess.run([str(root / "emitted-memory")], timeout=3).returncode == 42
print("ELF object writer: independent GNU ld/readelf interoperability passed")
