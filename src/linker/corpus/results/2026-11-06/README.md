# Local C/Fas compatibility qualification — 2026-11-06

All final runs in this directory use linker SHA-256
`5d4d4afeaaf3ba572d19b78204aa0ba0cca2730358abb450199fc53404775260`.
These are local correctness results on Linux x86-64 with the installed GCC,
Clang and glibc toolchains. They make no speed comparison or production-release
claim. Sources are pinned releases, not claims about the latest versions.

[The historical musl zlib matrix](musl-zlib.json) also qualifies this binary
with GCC 15/Clang 21/Clang 22 native objects in three linkage modes, plus a
saved-harness replay. The subsequent [fresh Ubuntu checkpoint](../2026-11-06-clean/README.md)
records the repaired core and its separately identified compiler/libc matrices.

| Project | Observed runtime coverage per linker | Result |
| --- | --- | --- |
| Lua 5.1.5 | 13 included examples plus bytecode compile/run, including the expected readonly error | Fas/GNU pass |
| bzip2 1.0.8 | Upstream sample checks and shared/static binary-payload compression/decompression | Fas/GNU pass |
| libpng 1.6.47 | 33 CTest entries, no skips | Fas/GNU pass |
| libarchive 3.8.11 | 1,004 CTest passes; 133 skips retained | Fas/GNU pass |
| curl 8.14.1 | 1,563 reported OK under Valgrind; 276 upstream skips | Fas/GNU pass |
| Redis 7.2.8 | 14 upstream Tcl units covering core types, scripting, transactions, RDB and AOF | Fas/GNU pass |
| SQLite 3.54.1 | 1,300 isolated extraquick scripts; 398,237 Fas / 398,257 GNU assertions, zero errors | Fas/GNU pass |

`popular-small.json` records the first four projects; `redis.json` records the
bounded Redis selection. The GNU phase relinks the exact retained native
objects/archives. Each result pins source manifests, harnesses, linker traces,
complete command logs and local snapshot paths. `triage.json` preserves failed
and superseded development runs; they do not count as passing qualification.

`shared-consumers-small.json` adds GNU executables using Fas libraries and Fas
executables using GNU libraries for bzip2, libpng and libarchive, with loaded
library provenance. Independent GNU CMake configurations agree on 13 libpng
and 469 libarchive numeric/boolean feature values. These bounded checks do not
repeat the full upstream suites or compare every CMake string/path setting.

[curl.json](curl.json) records both complete upstream phases and 326 GNU-relinked
ELF outputs. Both phases considered 1,839 tests, skipped 276 and reported all
1,563 remaining tests OK. Ten upstream ignored tests also passed; no ignored
failure is hidden. The skip reasons include unavailable optional features and
test servers and remain in the record. [The curl cross checks](shared-consumers-curl.json)
pass both shared-library directions using a loopback HTTP payload. Independent
GNU configuration matches all 488 checked numeric/boolean feature settings.

[Integrity and toolchain provenance](integrity-and-toolchain.json) records
original-source checks, retained oracle inputs and output hashes, compiler and
runtime versions, runtime dependency probes and local workflow lint. All 4,029
original curl files remain unchanged; all 3,260 selected input observations and
both copies of the 326 outputs match their recorded hashes. Separate temporary
CMake probe inputs deleted by CMake are identified rather than claimed retained.

`sqlite.json` replays the canonical source after section coalescing: 40 C API
executions, four native Fas consumers and both full selected upstream phases.
Every one of the 2,290 original source files remains unchanged. The randomized
FTS5 big-token script accounts for the assertion-count difference; both phases
complete all 1,300 scripts without errors. Original release archive checksums
are in [the source record](../2026-11-05/sqlite-inputs.json). This does not cover
TH3, full SQLite release testing, power loss or musl SQLite.

## GCC official C torture execution

The pinned GCC 15.2.0 source uses its unmodified DejaGnu driver, directives and
effective-target checks at `-O0`, `-O2` and `-Os`. Fas and the GNU exact-input
control each pass 4,974 executions, with identical execution labels and 4,974
matching native-input keys. Fourteen trampoline files and five
default-disabled expensive tests produce 57 official unsupported cases across
the three optimizations. This is not the complete GCC regression suite.

The compiler baseline explicitly removes distribution-injected fortified stdio
wrappers. GNU capability probes suppress linker diagnostics to avoid a libc
`tmpnam` warning falsely disabling file I/O; actual test links keep diagnostics.
Both corrections have independent official controls, retained alongside the
earlier failures. CI requires exactly 4,974 execution passes per linker to
detect an unexpectedly reduced selection. No new XFAILs or test-source rewrites
are used, and LTO remains unavailable.

[gcc-torture.json](gcc-torture.json) pins the completed reports, source commit,
harnesses, compiler flags, official unsupported labels and input traces. Each
phase has 9,948 DejaGnu PASS rows (compile plus execute), 57 UNSUPPORTED rows and
zero failures. The source checkout is clean. The completed full run predates
the optional execution-count CLI guard, so this evidence additionally asserts
the same exact count after completion; separate positive and deliberately
failing controls exercise the new guard.

## Scope and remaining release gates

Lua coverage is its included examples, not its separate release test suite.
libarchive's optional features follow the configured dependencies. curl keeps
upstream skips and ignored tests visible. Redis excludes cluster, Sentinel,
TLS, modules, large-memory and benchmark coverage. These project runs do not
qualify their musl configurations or additional compiler/libc versions.

The forced current linker gate passes 248 Python cases across 41 groups with
isolated musl and no skips; two subsequent process-cleanup harness cases also
pass. The full compiler regression suite is earlier evidence, not rerun here.
The updated linker also passes the [four large-input groups](../../../scale/results/2026-11-06/README.md)
and [seven output-mode fuzz campaigns](../../../fuzz/results/2026-11-06/README.md).
The native driver now combines embedded C/assembly into relocatable output
through the Fas partial linker; package-library inclusion remains outside that
object-output contract.

Hosted CI, sustained fuzz/release qualification and durable release artifacts
remain open. The workflow passes Actionlint locally; it has not run on hosted
runners. The main performance campaign remains deferred until the supported
production baseline is qualified. Full binaries, source snapshots, inputs and
logs remain in the recorded local cache directories; they are not a published
release archive.
