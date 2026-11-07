# Repeatable Linux qualification

The [final 1.0.0 acceptance](corpus/results/2026-11-07-v1.0.0/README.md) passes all nine hosted jobs on source
`3cf2962665405578b7a32a35f25e810e2ff188d5`, including fourteen ten-minute fuzz targets and archive/source rebuild
checks. Three one-hour local campaigns, the complete host suite and real native
Fas programs also pass. The versioned release and durable evidence are published.
Historical observations below retain their original outcomes; a passing
individual job alone does not complete these gates.

The [linker workflow](../../.github/workflows/linker.yml) defines an Ubuntu 24.04
x86-64 gate using OCaml 5.4.1 and Dune 3.24.2. Pull requests and pushes affecting
source/build/test files run the linker regressions, an isolated musl 1.2.5 build,
the full large-input runner, zlib 1.3.1 dynamic/static/static-PIE upstream tests,
checksum-pinned Chibicc self-hosting and both compiler-driver suites,
SQLite static/shared libraries and GCC/Clang/native-Fas consumers,
generated valid-program comparisons with Fas/GNU/LLD and mixed partial stages,
and a staged executable install/compile/run check. The linker Dune alias includes
native compiler integration tests; this workflow does not replace the separate
full compiler regression suite.

The [semantic checkpoint](corpus/results/2026-11-06-semantic/README.md) records a
passing local shared recipe and a separate unresolved fuzz timeout. A passing
correctness job alone does not complete the production acceptance gates.

The hosted correctness job and [clean local container runner](ci/README.md)
share one recipe, including verified local binary/source release archives. Its
final selection guard requires 273 Python regression cases without reported skips
and passing scale/corpus/semantic reports. The musl zlib
matrix adds GCC/Clang native objects linked by Fas/GNU as PIE, static ET_EXEC
and static PIE, including upstream tests and all gzip producer/consumer pairs.
The [archive checkpoint](corpus/results/2026-11-06-release/README.md) also qualifies
source-archive rebuilding, delivered/rebuilt executable smoke tests and actual
candidate reproducibility/rejection controls.

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
and source snapshot. Acceptance verifies the requested duration, complete target
selection, execution/edge statistics and the expected clean source commit.
Original AFL filenames are preserved inside a tar archive, with separate JSON
campaign and verification reports. Two-second manual upload smoke tests cannot
satisfy the ten-minute selection. Correctness artifacts are retained for 14 days; fuzz artifacts
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
Popular-C/GCC artifacts are retained for 14 days. All six popular-C jobs have
passing hosted evidence at the [first checkpoint](corpus/results/2026-11-06-hosted/README.md).
The [canonical-address checkpoint](corpus/results/2026-11-06-canonical/README.md)
retains the later GCC failure and its focused repair. Complete hosted acceptance
on the repaired source remains separate from those earlier passes. The
[repaired-source observation](corpus/results/2026-11-06-hosted-canonical/README.md)
passes the full declared GCC and correctness selections; two popular-C jobs
and the full fuzz campaign are still pending at that observation.
The [reference-baseline observation](corpus/results/2026-11-06-hosted-baseline/README.md)
subsequently passes both remaining popular-C jobs; full fuzzing remains running.

The workflow definition alone is not evidence of a completed hosted run. Local
qualification is recorded separately under `audit/`, `scale/results/`,
`corpus/results/` and `fuzz/results/`. The [fresh local Ubuntu qualification](corpus/results/2026-11-06-clean/README.md)
passes the shared correctness recipe, including GCC 13/Clang 18, glibc 2.39 and
isolated musl. The [hosted correctness checkpoint](corpus/results/2026-11-06-hosted/README.md)
also passes its declared Ubuntu/kernel baseline. Other snapshots, environments
and the complete GCC/fuzz selection require their own passing records. No performance comparison
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
