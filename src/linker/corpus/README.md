# Real-program acceptance

This opt-in corpus complements the self-contained linker tests. It compiles
snapshots into a new output directory, records source/toolchain hashes and all
commands, and compares behavior against GNU ld. It makes no timing comparisons.
Current paths/library names target the tested Debian-style Linux x86-64 sysroot.
Source inputs and game assets are not redistributed or changed.
Each run copies the harness, selected linker and (for Sieve/SQLite) compiler into its
output directory and hashes those snapshots, so rebuilding development
executables cannot change a running replay. To replay a saved harness from its
temporary directory, pass explicit `--linker` and `--compiler` snapshot paths.
Commands run in separate process groups. A timeout kills the inherited group
and records captured output before failing, so build children do not continue
into another phase. Daemons that deliberately create their own sessions still
require the upstream harness's cleanup.

## Isolated musl real-library matrix

`musl.py` compiles zlib 1.3.1's archive and four upstream example/gzip objects
with each requested GCC/Clang compiler against an isolated musl sysroot. Fas
and GNU link identical native inputs into PIE, static ET_EXEC and static PIE.
Every phase runs upstream `test`/`test64`; four gzip producers are decoded by
all four consumers in each mode. ELF checks enforce type, interpreter and
static-PIE dependency policy, and input hashes must remain unchanged.

```sh
python3 src/linker/corpus/musl.py --output /tmp/new-musl-zlib \
  --zlib /path/to/zlib-1.3.1 --musl-sysroot /path/to/musl/sysroot \
  --cc gcc --cc clang
```

Upstream configure probes use the installed musl GCC driver and GNU ld; the
selected compiler builds all payload objects. This qualifies native object and
musl startup compatibility, including Clang's musl target, rather than Clang
driver discovery. It does not test a zlib DSO or static TLS relaxation. Pinned
source fetching and musl installation use `fetch.py` as in the other corpus
recipes. The [clean container gate](../ci/README.md) includes this matrix.

From `fas`, after a release build:

```sh
python3 src/linker/corpus/run.py --output /tmp/fas-corpus-new \
  --chibicc /path/to/chibicc \
  --sieve /path/to/fas_solution \
  --doom /path/to/linuxdoom-1.10 \
  --wolf3d /path/to/wolf4sdl --wolf-data /path/to/data
```

Select any subset. Requires GCC, GNU tools and Clang/LLVM for Fas's LLVM backend;
Doom requires X11 development libraries; Wolf3D requires SDL 1.2 and SDL_mixer
development libraries. No display is opened. Wolf3D runtime probes use SDL's dummy
video/audio drivers and a recorded change to the snapshot: bypass menus, fix the
random seed and hash the first three E1M1 game frames. This is a bounded rendering
check, not general gameplay acceptance. Without assets, Wolf3D is link-only.
Doom compares startup's missing-WAD failure path. Add `--doom-wad /path/to/DOOM1.WAD`
to also render 32 single-tic frames headlessly with fixed and PIE Fas/GNU links.
The recorded snapshot changes bypass display/audio devices and hash indexed
framebuffer pixels; simulation, asset loading and rendering still run. Local
assets are copied into the temporary directory and hashed, never redistributed.
Linux Doom 1.10 discovers recognized lowercase IWAD filenames via `DOOMWADDIR`;
the runner normalizes the supplied filename. This remains bounded rendering
coverage; input, audio output and general gameplay are unverified.

Add `--strict-dependencies --dependency-path /usr/lib/x86_64-linux-gnu` to replay
hosted links with `--no-allow-shlib-undefined` on both Fas and GNU ld. Repeat the
dependency-path option for other declared sysroot directories. The report records
this policy separately; no environment/cache/default-directory discovery is added.

Chibicc's GCC-built compiler is linked by fas-ld, then compiles its upstream C
test programs. Each is linked by Fas/GNU and both executions must pass and match.
The source filename stays relative because upstream tests assert `__FILE__`.
That `--chibicc` mode covers stage 1. For the unmodified upstream self-hosting
build, both driver suites and 82 GNU/Fas C runtime comparisons, use:

```sh
python3 src/linker/corpus/fetch.py chibicc --output /tmp/fas-chibicc-source
python3 src/linker/corpus/run.py --output /tmp/fas-chibicc-selfhost \
  --chibicc-selfhost /tmp/fas-chibicc-source/chibicc-90d1f7f199cc55b13c7fdb5839d1409806633fdb
```

The bootstrap `cc` wrapper uses GCC with `-fPIC -fno-use-linker-plugin` and selects
Fas through `-B`. Chibicc's own generated stage-2 objects keep their upstream
code-generation policy and exercise imported-data COPY relocations. Both `cc`
and `ld` wrappers are confined to the test directory; source files are unchanged.
This mode cannot be combined with static/static-PIE or strict-dependency options:
the upstream driver suite chooses its own shared/static/dynamic link policies.

Sieve runs each original `.fas` source and its built-in prime-count validation.
By default both LLVM and native backends are required; select only one with
`--sieve-backend llvm` or `--sieve-backend native`. Every object is linked and
executed with Fas and GNU ld. Native sources also compile and run through the
automatic hosted driver. Native compilation sets unavailable `opt`/`llc` paths
to catch LLVM fallback. Any selected backend failure fails qualification.
The combined native ABI/driver branch passes all four original sources,
including variadic libc calls, dynamically and with static glibc. C header
import and hosted CRT discovery still use the configured C compiler.
Sieve prints its own elapsed time/pass count; these values are not benchmark
comparisons, and pass counts need not match across executions.

Download a pinned small library and run its upstream tests:

```sh
python3 src/linker/corpus/fetch.py cjson --output /tmp/fas-cjson-new
python3 src/linker/corpus/run.py --output /tmp/fas-cjson-tests-new \
  --cjson /tmp/fas-cjson-new/cJSON-1.7.19
```

The cJSON v1.7.19 corpus covers 21 upstream core/utility test programs, using the
upstream Unity framework. Some tests include `cJSON.c` to exercise private code;
the runner respects that arrangement. GNU and Fas executables must pass and have
identical output. Download URLs and SHA-256 hashes are pinned in `fetch.py`.

Build musl 1.2.5 into a temporary, isolated sysroot:

```sh
python3 src/linker/corpus/fetch.py musl --output /tmp/fas-musl-new --build-musl
FAS_MUSL_SYSROOT=/tmp/fas-musl-new/sysroot dune runtest --profile release src/linker
```

Both the install prefix and runtime loader path remain under the selected
directory. Host `/lib` and `/usr` are not modified. `musl_integration.py` checks
static/fixed/PIE startup, libc calls, arguments/environment, constructor priority,
atexit/destructors, independent threads/errno, LE/IE/GD/LD/TLSDESC and late-loaded
TLS. Static output currently supports LE/IE/DTPOFF with runtime-owned TLS startup;
GD/LD/TLSDESC instruction relaxation remains outside this gate. Installed static
glibc has a separate `static_ifunc_integration.py` suite in the default tests.
Without the environment variable, the default suite explicitly reports musl as
skipped. The self-contained static TLS startup test still runs.

For a real-program static glibc replay, select Chibicc, Sieve and/or cJSON:

```sh
python3 src/linker/corpus/run.py --output /tmp/fas-static-corpus --static \
  --chibicc /path/to/chibicc --sieve /path/to/fas_solution \
  --cjson /path/to/cJSON-1.7.19
```

This mode uses the installed static CRT/libc/libgcc/libm archives and records their
hashes. It verifies ET_EXEC output without interpreter or dynamic program headers.
Chibicc's upstream tests and cJSON's 21 test programs compare behavior with GNU ld;
Sieve retains its prime-count checks. Static game dependency
closures remain unqualified; `--static` cannot be combined with the dynamic
strict-dependency option or the Doom/Wolf3D corpus selections.

For CRT-relocated static PIE, use `--static-pie --cjson /path/to/cJSON-1.7.19`.
This selects `rcrt1.o`, PIC-compatible objects and static libc/libgcc/libm, verifies
ET_DYN with PT_DYNAMIC and no interpreter, and compares all 21 upstream test
programs against GNU ld. The mode accepts cJSON and zlib; the existing
Chibicc/Sieve object policies do not establish PIC compatibility, and game static
dependency closures remain unqualified. `--static` and `--static-pie` are mutually
exclusive. Both reject dynamic strict-dependency options.

The zlib 1.3.1 corpus uses its upstream `configure`/`make` build with GCC invoking
Fas as `ld`. The test-only compiler wrapper disables GCC's linker plugin; no LTO
is used. Every actual linker invocation is retained. Dynamic mode builds the
versioned shared library as well as the static archive and executes upstream
static/shared/large-file API tests. GNU consumers load the Fas-produced DSO;
Fas/GNU gzip producers and consumers cross-decode the same payload. Static and
static PIE modes exercise the upstream archive build with the corresponding
GCC driver flags, inspect the resulting executable types and compare GNU links.
These are correctness checks, without timing comparisons.

```sh
python3 src/linker/corpus/fetch.py zlib --output /tmp/fas-zlib-new
python3 src/linker/corpus/run.py --output /tmp/fas-zlib-dynamic \
  --zlib /tmp/fas-zlib-new/zlib-1.3.1
python3 src/linker/corpus/run.py --output /tmp/fas-zlib-static-pie --static-pie \
  --zlib /tmp/fas-zlib-new/zlib-1.3.1
```

[Preserved results](results/2026-11-05/README.md) distinguish tested scopes and
remaining limitations. Full binaries, snapshots and command logs remain in the
recorded temporary directories; they are not durable release artifacts.

SQLite 3.54.1 uses the complete canonical source release, pinned by SHA-256 and
SQLite's published SHA3-256. Tcl development files are required (`tcl-dev` on
the CI host). The runner invokes upstream `configure --all` and `make` through
Fas, building the CLI, static archive, shared library and Tcl test executable.
Temporary compiler objects are retained so GNU ld can link the same CLI, DSO
and test-executable inputs independently.

```sh
python3 src/linker/corpus/fetch.py sqlite --output /tmp/fas-sqlite-source
python3 src/linker/corpus/run.py --output /tmp/fas-sqlite-tests \
  --sqlite /tmp/fas-sqlite-source/sqlite-src-3540100 --sqlite-extraquick
```

Without `--sqlite-extraquick`, upstream `main.test` is the short gate. The larger
selection runs through unmodified `test/testrunner.tcl --jobs 2 extraquick`,
which gives each test its own process and scratch directory. Both selections
use that official runner with separate Fas/GNU working directories and the
matching CLI beside each test executable. Per-file results and assertion counts
are retained, and both oracles run before upstream failures fail qualification.
Each must pass the same script selection; assertion counts can differ where
upstream generates a variable number of checks from random data. Those count
differences are recorded explicitly. This is the public Tcl suite,
not SQLite's proprietary TH3 or full release suite; upstream's test executable
defines `SQLITE_NO_SYNC`, so it does not establish power-loss durability.
Optional `--sqlite-tcl-config DIR` and `--sqlite-tclsh PATH` select a local Tcl
installation without changing system files.

This SQLite matrix currently uses the installed glibc toolchain; SQLite on
musl is not qualified by the separate libc smoke tests.
GCC and, when available, Clang API consumers exercise the SQLite archive in
fixed/PIE/full-static/static-PIE executables and both Fas/GNU shared libraries.
Dynamic consumers load both Fas/GNU extensions. Checks include prepared
statements, transactions/savepoints, on-disk reopen, WAL with four writer
threads, JSON, FTS5, RTREE, callback/destructor execution, backup and integrity.
Native Fas consumers use archive/shared/full-static/static-PIE links with LLVM
tools unavailable. The shell also cross-reads databases and restores dumps
between independent linkers. All modes are selected by this one corpus option;
do not combine `--sqlite` with the runner's global linkage/dependency options.
No timing comparisons are collected. The larger suite creates large databases;
allow several GiB of free scratch space and prefer disk storage over a small
`/tmp` tmpfs.

## Six additional C projects

`popular.py` builds pinned Lua 5.1.5, bzip2 1.0.8, libpng 1.6.47,
libarchive 3.8.11, curl 8.14.1 and Redis 7.2.8 through GCC's real driver
with Fas selected as `ld`. GCC's LTO plugin is disabled. It runs the declared
upstream tests, retains compiler temporaries and hashes object/archive inputs,
then relinks the same inputs with GNU ld and reruns the checks. Every project
gets a source snapshot, local compiler/linker wrappers, saved Fas outputs and
complete command logs. The original sources and system toolchain are unchanged.
Use a new disk-backed output directory with several GiB free:

```sh
python3 src/linker/corpus/fetch.py libarchive --output "$HOME/.cache/archive-source-new"
python3 src/linker/corpus/popular.py --output "$HOME/.cache/archive-tests-new" \
  --project libarchive "$HOME/.cache/archive-source-new/libarchive-3.8.11"
python3 src/linker/corpus/cross.py --corpus "$HOME/.cache/archive-tests-new" \
  --project libarchive --output "$HOME/.cache/archive-cross-new"
```

Repeat `--project NAME SOURCE` to select multiple projects. The fetcher accepts
all six names and pins release archives with SHA-256. Build/test dependencies
include GCC, GNU binutils, make, CMake, pkg-config, Python, Perl and Tcl; library
features depend on installed development packages. The CI recipe declares its
dependencies explicitly. Installed Valgrind is used automatically by curl's
upstream harness; it remains enabled. curl uses `LC_ALL=C.UTF-8` because the
system IDN implementation rejects international hostnames in the ASCII locale.
Redis uses a private short directory under `/var/tmp` for its Unix sockets; the
report records that path. Completed test artifacts are moved into the corpus
`tests/tmp` directory. An interrupted run can retain its recorded scratch path.

The declared selections are:

| Project | Runtime coverage |
| --- | --- |
| Lua | 13 included example scripts, including the expected readonly error, plus bytecode compilation/execution; not the separate Lua release test suite |
| bzip2 | Upstream sample checks and shared/static binary-payload compression/decompression |
| libpng | Upstream CTest selection with shared and static libraries enabled |
| libarchive | Upstream CTest selection and shared bsdtar/bsdcpio/bsdcat/bsdunzip builds; optional features follow the configured dependencies |
| curl | Upstream `test-nonflaky`, two local workers; timing-sensitive/flaky tests are recorded as ignored, unavailable protocols as skipped; no external service required |
| Redis | Upstream auth, protocol, keyspace, string/list/set/zset/hash, other, multi, scripting, dump, RDB and AOF units; libc allocator and native `-O2`; no cluster, Sentinel, TLS, large-memory or benchmark claim |

`cross.py` requires a passing `popular.py` report. It checks GNU executables
with Fas DSOs and Fas executables with GNU DSOs for bzip2, libpng, libarchive
and curl, including `ldd` provenance. For CMake projects it also independently
configures with GNU and compares numeric/boolean feature availability, catching
features that a failed Fas probe could silently disable. It temporarily swaps
only saved corpus DSOs and restores them in `finally`; do not run it concurrently
with another consumer of the same snapshot. These are bounded interoperability
probes, not another complete upstream test run. Lua dynamic C modules and Redis
modules are not covered by these shared-library probes.

`archive_pipeline.py` reuses a passing Lua/libarchive popular-C snapshot to
qualify thin archives and mixed partial links with real library members:

```sh
python3 src/linker/corpus/archive_pipeline.py --corpus /path/to/popular-results \
  --output /new/external/archive-pipelines --lld ld.lld
```

Select `--project lua` or `--project libarchive` for a single-project snapshot.
It verifies retained source/object/archive/CRT hashes, extracts the project
archives into its own output tree and compares four pipelines: regular archive,
thin archive, Fas-to-GNU partial stages repackaged as a regular archive, and
GNU-to-Fas partial stages repackaged as a thin archive. Fas/GNU and optional LLD
perform final links. Partial stages aggregate whole archives, so the final
archive's extraction granularity changes; this is a runtime-equivalence check,
not a byte-equality assertion. Original corpus files are unchanged.

The Lua selection repeats the same 13 examples and bytecode compile/execute
checks. Libarchive runs eight named read/write cases for tar, zip, cpio and 7zip
against its retained reference data. Each case must report exactly one executed
test, positive assertions, zero failures and zero skips, in addition to exit 0.
This bounded selection is not another complete libarchive CTest run. Reports,
member hashes/order, all archive variants, intermediates, programs and command
logs are retained. Weekly/manual popular-C jobs run this after the upstream and
shared-library checks for these two projects.

## Official GCC torture execution tests

`torture.py` invokes GCC's unmodified DejaGnu `execute.exp` with its official
`dg` directives and effective-target checks. It does not reinterpret directives
or treat an arbitrary zero exit as a successful test. Source is pinned to GCC
15.2.0 commit `5115c7e447fc07457443df874bf57840e8316d5f`; use a matching
GCC 15.2 compiler, GNU binutils, DejaGnu and Expect:

```sh
python3 src/linker/corpus/fetch.py gcc-torture --output "$HOME/.cache/gcc-source-new"
python3 src/linker/corpus/torture.py \
  --gcc-source "$HOME/.cache/gcc-source-new/gcc-15.2.0" \
  --output "$HOME/.cache/gcc-tests-new" --no-trampolines
```

The default matrix is `-O0`, `-O2`, `-Os` across top-level C execution tests.
`--pattern '200*.c'` selects an official filename glob; repeat
`--optimization=-O2` with desired optimization levels to bound a run. Each
compiler invocation disables GCC's linker plugin and effective-target LTO
probes report LTO unavailable. No LTO objects are accepted or supported.
The torture wrapper also passes `-U_FORTIFY_SOURCE` to remove distribution-
injected fortified stdio wrappers: upstream builtin tests define their own
`__vfprintf_chk`/`__vprintf_chk` and otherwise abort with GNU ld as well.
This compiler-test setting applies equally to both linkers and is recorded in
the report; the real-project corpora keep their own compilation policies.
GNU capability-probe links use [`--no-warnings`](https://sourceware.org/binutils/docs-2.40/ld/Options.html): its libc `tmpnam` warning
otherwise makes the official file-I/O probe incorrectly report unavailable.
Probe failures still propagate through their exit status, and actual torture
test links retain all diagnostics. The recorded link arguments expose this
probe-only policy; both phases must still execute identical test selections.
The GNU phase substitutes the exact retained Fas object/archive inputs and
verifies their hashes. Official execution labels and native-link selections must
match between phases; `FAIL`, `XPASS`, `UNRESOLVED`, errors, or an empty execution
selection fail qualification. Official `UNSUPPORTED` and `XFAIL` remain visible
in `gcc-{fas,gnu}.sum` and the report. Full logs and wrappers are preserved.
`--expected-executions N` additionally freezes a known execution count per
linker. CI requires 4,974 for this pinned three-optimization baseline so a
regression in capability detection cannot silently shrink the corpus. A changed
count requires investigating the official skips before updating that baseline.

`--no-trampolines` explicitly declares the non-executable-stack subset using
GCC's `gcc,no_trampolines` board capability. The linker currently rejects objects
requesting an executable stack, so GNU nested-function trampoline tests cannot
pass its normal output policy. Omit the option to expose that compatibility gap;
no trampoline test is silently rewritten or marked passing. This is C torture
execution coverage, not all GCC regression suites, optimization variants,
architectures, C++ tests or language conformance certification.
The local baseline skips 14 trampoline files and five tests guarded by GCC's
default-disabled `run_expensive_tests` capability at each optimization.

`--dejagnu-root DIR` can use locally extracted Debian DejaGnu/Expect packages
without installing system files. The weekly/manual CI job uses an immutable
GCC 15.2.0 container image instead. CI definitions require a hosted run before
being claimed as verified; local runs do not establish hosted CI success.
