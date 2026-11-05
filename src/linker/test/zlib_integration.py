#!/usr/bin/env python3
"""Differential RFC 1950/1951 decoding against Python's independent zlib."""
from pathlib import Path
import random
import struct
import subprocess
import sys
import zlib

driver = str(Path(sys.argv[1]).resolve())
rng = random.Random(0x19501951)
cases = []


def add(data, size):
    try:
        oracle = zlib.decompressobj()
        decoded = oracle.decompress(data, 1024 * 1024)
        if not oracle.eof or oracle.unused_data or len(decoded) != size:
            decoded = None
    except zlib.error:
        decoded = None
    cases.append((data, size, decoded))


payloads = [b'', b'A', bytes(range(256)), b'ABRACADABRA' * 5000,
            bytes(rng.randrange(256) for _ in range(70000)), b'\0' * 100000]
payloads += [bytes(rng.randrange(1 + n % 256) for _ in range(rng.randrange(12000)))
             for n in range(100)]
for i, raw in enumerate(payloads):
    for level in (0, 1, 6, 9):
        for strategy in (zlib.Z_DEFAULT_STRATEGY, zlib.Z_FIXED, zlib.Z_HUFFMAN_ONLY,
                         zlib.Z_RLE, zlib.Z_FILTERED):
            compressor = zlib.compressobj(level, zlib.DEFLATED, 15, 8, strategy)
            compressed = compressor.compress(raw) + compressor.flush()
            add(compressed, len(raw))
            if i < 6:
                add(compressed, len(raw) + 1)
                if raw:
                    add(compressed, len(raw) - 1)
                add(compressed + b'junk', len(raw))
                for _ in range(3):
                    damaged = bytearray(compressed)
                    at = rng.randrange(len(damaged))
                    damaged[at] ^= 1 << rng.randrange(8)
                    add(bytes(damaged), len(raw))
                for cut in (0, 1, 2, 3, 5, len(compressed) - 1):
                    add(compressed[:cut], len(raw))

# Mixed blocks, flush boundaries, alternate RFC window sizes and overlapping
# back-references, including multi-block streams with empty stored blocks.
for window in range(9, 16):
    c = zlib.compressobj(6, zlib.DEFLATED, window)
    stream = bytearray()
    raw = b''
    for n in range(20):
        piece = (b'abc' * (n * 91)) + bytes([n]) * 200
        raw += piece
        stream.extend(c.compress(piece) + c.flush(zlib.Z_SYNC_FLUSH))
    stream.extend(c.flush())
    add(bytes(stream), len(raw))

# Header and block validation independent of a checksum failure.
for stream in (b'\x78\x20\0\0\0\0\0\0\0\0',  # preset dictionary
               b'\x78\x9c\x07\0\0\0\x01',      # reserved block
               b'\x78\x9c\x01\x01\0\0\0X\0\0\0\x01'):
    add(stream, 1)

request = b''.join(struct.pack('<II', size, len(data)) + data for data, size, _ in cases)
result = subprocess.run([driver], input=request, capture_output=True, timeout=90)
assert result.returncode == 0, result.stderr.decode(errors='replace')
cursor = 0
for n, (data, size, expected) in enumerate(cases):
    actual_size, = struct.unpack_from('<I', result.stdout, cursor)
    cursor += 4
    actual = None
    if actual_size != 0xffffffff:
        actual = result.stdout[cursor:cursor + actual_size]
        cursor += actual_size
    assert actual == expected, (n, len(data), size, actual_size, expected and len(expected))
assert cursor == len(result.stdout)
print(f'zlib differential checks: {len(cases)} passed')
