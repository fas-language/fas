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
