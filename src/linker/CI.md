# Repeatable Linux qualification

The [linker workflow](../../.github/workflows/linker.yml) defines an Ubuntu 24.04
x86-64 gate using OCaml 5.4.1 and Dune 3.24.2. Pull requests and pushes affecting
source/build/test files run the linker regressions, an isolated musl 1.2.5 build,
the full large-input runner, zlib 1.3.1 dynamic/static/static-PIE upstream tests,
checksum-pinned Chibicc self-hosting and both compiler-driver suites,
SQLite static/shared libraries and GCC/Clang/native-Fas consumers,
and a staged executable install/compile/run check. The linker Dune alias includes
native compiler integration tests; this workflow does not replace the separate
full compiler regression suite.

SQLite correctness runs build the unmodified 3.54.1 canonical source with
`configure --all`, then independently link the same objects with GNU ld. Normal
runs use upstream `main.test`; weekly/manual runs select `extraquick` through
upstream `test/testrunner.tcl` with two workers and isolated per-file processes. Tcl
development files are build/test dependencies, not linker runtime dependencies.
The corpus also checks on-disk databases, WAL/threads, backup, callbacks,
JSON/FTS5/RTREE and Fas/GNU loadable extensions, plus full-static/static-PIE
consumers. The public Tcl suite does not establish SQLite full-release or
power-loss qualification.

Weekly and manual runs add fourteen coverage-guided targets at 600 seconds each,
sequentially at reduced scheduling priority. The fuzz job has a three-hour limit.
Saved crashes/hangs fail the job and are retained with the target, queues, seeds
and source snapshot. Correctness artifacts are retained for 14 days; fuzz artifacts
for 30 days. Retention is not a durable release archive. The workflow uses pinned
action commits and read-only repository permissions; it publishes no releases.

Weekly/manual runs also define six independent popular-C jobs for pinned Lua,
bzip2, libpng, libarchive, curl and Redis. Each builds through Fas, runs its
declared upstream tests, then runs GNU-linked outputs from the same native
objects. The four shared-library corpora add producer/consumer cross-checks;
CMake corpora compare feature probes against an independent GNU configuration.
Valgrind stays enabled for curl and is installed for the focused debug-reader
regression in the normal gate. Project-specific selections and limits are
documented in the [corpus guide](corpus/README.md).

A separate four-hour GCC torture job uses the pinned official GCC 15.2.0 image,
matching source commit and DejaGnu's `execute.exp` at `-O0`, `-O2` and `-Os`.
It compares official executions with Fas/GNU linking identical retained inputs.
The pinned baseline requires exactly 4,974 execution passes per linker; a changed
capability selection fails the gate for investigation.
Its explicit board capability excludes executable-stack trampolines, matching
the linker's stack policy. LTO probes are unavailable and no LTO support is
added. These are native C execution tests, not the complete GCC regression suite.
Popular-C/GCC artifacts are retained for 14 days. The new jobs have not yet been
qualified on hosted runners.

This is a workflow definition, not evidence of a completed hosted run. Local
qualification is recorded separately under `audit/`, `scale/results/`,
`corpus/results/` and `fuzz/results/`. Ubuntu/toolchain compatibility remains
unqualified until the workflow actually passes there. No performance comparison
or production-readiness claim follows from adding automation.

Equivalent local commands, from the repository root, with fresh output directories:

```sh
dune build --profile release
FAS_MUSL_SYSROOT=/path/to/musl/sysroot dune runtest --profile release
python3 src/linker/scale/run.py _build/default/src/linker/bin/main.exe \
  --output /tmp/fas-scale-new --profile full
python3 src/linker/corpus/run.py --output /tmp/fas-zlib-new \
  --zlib /path/to/zlib-1.3.1 --static-pie
python3 src/linker/fuzz/run.py --output /tmp/fas-fuzz-new --seconds 600
```

See [corpus setup](corpus/README.md), [large-input scope](scale/README.md) and
[fuzzing scope](fuzz/README.md) for dependencies and interpretation. None of the
test toolchains or the upstream zlib C library become runtime dependencies of
`fas-ld`.
