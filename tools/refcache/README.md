# Reference result cache

`refcache` stores results from deterministic, non-interactive commands. It
uses Python 3.11 or later, the standard library, and a Linux filesystem with
atomic rename and advisory file locks.

```sh
tools/refcache/refcache run --ns clang-obj --c --dep example.c --out example.o \
  -- clang-22 -c example.c -o example.o
tools/refcache/refcache stats
tools/refcache/refcache verify --sample 20
tools/refcache/refcache gc --max-size 4GiB
```

Set `FAS_REFCACHE` to a directory on disk. The default is
`/home/glonex/fas-experimental-backend/cc-tests/refcache`.
`FAS_REFCACHE=off` or `run --refresh` invokes the command directly and bypasses
cache reads and writes.

Declare every explicit input with `--dep` and every output with `--out`.
Use `--c` for the supported GCC and Clang driver forms. The driver writes a
private `-MD` dependency file including system headers. Hosted links also use
the GNU linker map to record loaded files. C commands that fail to report
dependencies, assembly that reads extra files, and linker commands with
undeclared search or script inputs run directly. Use `--stdin` to include
arbitrary piped input in the key. An explicit `-` operand also enables capture
of piped or redirected stdin.

The key includes the namespace, full argument vector, relevant environment,
compiler path, version output and binary hash, and dependency paths and hashes.
C driver identities also include glibc and the host's startup files and
toolchain components. Add command-specific environment variables with
`--env NAME`. The working directory is included by default. Use `--no-cwd`
when callers can guarantee it has no effect. Changing a known header invalidates
the entry even if its length is unchanged. Use `--refresh` after changing
include search roots by adding a new shadowing header or replacing an external
resource the compiler's dependency file does not list. Remove the corresponding
cache entries or use a new namespace when making that change permanent.

Use `--scratch DIR` for callers that regenerate files under a private scratch
directory. It makes argument paths under that directory relative to the
scratch root for keying, while executing the original argument vector. The
caller must ensure that location outside that directory is irrelevant. Results
containing the scratch path in any stream or output bypass storage.

Blobs are keyed by SHA-256. Indexes record the input fingerprints, exit status,
stdout, stderr, and output digests and modes. Hits stat their inputs and hash
files when stat information changes. The store treats missing or damaged
records as misses. Writers atomically replace files, and garbage collection
takes an exclusive store lock before removing records and orphaned blobs.
Use private output destinations for concurrent commands. Output publication
on replay uses atomic rename per file.

Verification samples entries with available inputs and the same tool identity.
It rehydrates declared scratch inputs in a private directory and checks the
exit code, both streams, and output contents and permissions. For a regular
output, the output operand is redirected to a private file for verification.
Declare the output as its own argument for this to work. Commands that embed
output paths can use the scratch mode. Verify with a trusted store because the
command and input snapshots are read from the stored entry.

The Python entry point is `refcache.run(argv, ns=..., deps=..., outs=..., ...)`.
It returns a `subprocess.CompletedProcess` with captured streams and accepts
`cwd`, `env`, `input` bytes, `timeout`, `text`, `check`, `c`, and `scratch`.
`Cache(root)` sets the store explicitly.

## Reference timings

`bench.py record program.c` measures GCC and Clang 22 at `-O1`, `-O2`, and
`-O3`. It measures fresh builds and executions separately and saves the median
and median absolute deviation in seconds. Use `--repeats N`, `--opt 2`,
`--compiler clang-22`, or `--flag=-lm` to choose the measurement inputs.
The key includes the program and its headers, arguments, compiler identity,
CPU model and microcode, governor, kernel, glibc, SMT state, and CPU affinity.
Timing records are stored under `bench/` and are independent of artifact
garbage collection.

`bench.py compare fresh.json` accepts a list of fresh Fas measurements.

```json
[
  {
    "reference_key": "<key printed by record>",
    "build": {"median": 0.12, "mad": 0.001},
    "run": {"median": 0.01, "mad": 0.0002}
  }
]
```

It checks the machine and compiler fingerprint, measures a short calibration
program again, and prints build and run ratios with a drift flag. Excessive
drift makes the command exit nonzero. The default relative drift threshold
is 15%, with a floor based on the calibration's observed dispersion.

Run `tools/refcache/test.sh` to exercise cache correctness and the benchmark
self-test. The timing self-test uses two tiny programs and two repeats.
