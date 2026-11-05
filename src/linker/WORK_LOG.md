# Linker implementation work log


### Current checkpoint: quiet-PC performance replay

The idle-PC interval on 2026-11-05 ran from 21:17 to 21:22 CET. Pinned release
snapshots, unchanged machine settings and warm-cache rotated sequential runs
compare the SHA-1/IFUNC changes with LLD 22.1.8 and GNU ld 2.46. The 51-sample
repeat shows approximately 7% lower dynamic Chibicc medians; static changes are
around 1%, insufficient for a speed claim. Fas remains 1.46–2.76 times LLD's
elapsed time across those four workloads. Separate 21-sample application links
put native Sieve/Doom/Wolf3D at 28.47/124.52/41.17 ms versus LLD's
18.14/36.12/21.58 ms. Runtime checks pass for all tools, outside link timing.

Synthetic scaling exposes the next bottleneck: extracting all 1,024 archive
members takes 328.68 ms versus LLD's 55.08 ms. One-of-1,024 extraction still
spends most of its untimed phase sample in parsing (container/index and objects). Archive indexing and
packed symbol/relocation storage remain priorities. Lower process peak RSS than
LLD in these application samples is not private memory or a new allocation-to-RSS
claim. No general ranking, cold-cache, multicore or production claim follows.

Seven raw reports/manifests are preserved under `benchmarks/results/2026-11-05/`;
`benchmarks/README.md` records methodology, results and temporary snapshots.
The replay harness now supports runtime environment overrides and required
stdout text. The Doom framebuffer check passes for Fas/LLD/GNU, and a deliberate
wrong-output expectation fails as required. No fresh timing runs assume the PC
is still idle after the announced end of the quiet window.


### Current checkpoint: static IFUNC reference planning

The next static Chibicc profile identified per-relocation definition-key and
option-closure allocation in static IFUNC trampoline lookup. Planning now builds
an invocation-local integer reference table once per symbol, only when live
static IFUNCs exist. Emission uses direct table lookup; the no-IFUNC path also
avoids its previous closure. Canonical definitions determine the mapping,
including weak references and overridden IFUNC declarations.

Compared with the SHA-1 checkpoint, static Chibicc allocates 814,438 fewer words
(7.6%); dynamic Chibicc and static-PIE cJSON drop by 0.5–1.7%. All four pinned
replay outputs remain byte-identical and pass runtime checks. Existing static
IFUNC tests, glibc lifecycle/TLS/thread tests and concurrent/repeated links pass.
A new GNU/LLD oracle case covers an ordinary strong definition overriding a weak
IFUNC, weak declarations resolving to live IFUNCs and absent weak references,
with direct/partial links and GC. The release gate passes with musl: 244 Python cases across 41 groups (including
the final incremental IFUNC rerun), OCaml checks and 3,792 compiler checks.
The development build and formatting checks also pass. No timings or peak-RSS claims are made.
Evidence: `benchmarks/results/2026-11-05/allocation-ifunc-references.json` and
`/tmp/fas-ifunc-reference-{profile,release,canonical}-20261105*`.


### Current checkpoint: build-ID allocation reduction

Untimed Memprof samples identify SHA-1 boxing as roughly 35%/39% of allocated
words in stage-1/stage-2 dynamic Chibicc links. Masked immediate integer words
remove that loop allocation on the supported 64-bit host. Exact stats show
35%/40% fewer total allocated words, with byte-identical outputs. Static replay
recipes omit build IDs and remain unchanged. No new wall-time claim is made;
the quiet-PC question is pending and Claude is running integration checks.

SHA-1 matches Python hashlib on 717 deterministic binary inputs, including every
length 0..512, random larger payloads and a 1 MiB input. Additional high-byte and
padding boundary vectors are permanent units. The full local release suite with
isolated musl passes 243 Python cases in 41 groups, OCaml checks and all 3,792
compiler checks, without skips. All four pinned real-link replay outputs retain
identical bytes and pass their runtime checks. Evidence and the reusable optional
allocation sampler are under `benchmarks/`; raw logs/profiles remain in
`/tmp/fas-allocation-profile-20261105` and `/tmp/fas-sha1-release-20261105.log`.


### Current checkpoint: combined native real-program qualification

Qualification runs use the isolated `linker-qualification-20261105` worktree,
pinned to Claude's combined ABI/driver merge `5fd063f`. Preserve his atomic
intrinsic exclusion in automatic runtime selection and his FIR hook. Claude
reports 3,792 compiler checks, native ABI/atomics/shell suites and broad
native-versus-LLVM differential checks passing; those are his integration
results, separate from the locally observed corpus evidence below.

All four original Sieve sources pass both backends and Fas/GNU object links.
Native sources additionally pass the direct automatic hosted driver: 20 dynamic
executions, then 12 native static-glibc executions. Every program's original
prime-count assertion runs unchanged; native compilation uses nonexistent
`opt`/`llc` paths to detect fallback. The harness now snapshots the compiler and
itself as well as the linker, and selected backend failures fail qualification.
No native backend or original corpus source files were changed.

Doom's 62-object startup comparison and Wolf3D's three-frame probe pass again.
A new optional Doom device harness renders 32 single-tic E1M1 skill-3 frames.
Fas/GNU fixed/PIE outputs share framebuffer hash `52e20f0b442bf948`. The initial
attempt used modern `-iwad`, which Doom 1.10 does not implement; lowercase IWAD
snapshot discovery via `DOOMWADDIR` fixes the harness. Original rendering and
simulation run; display/audio output is bypassed. General gameplay is unverified.
Only local temporary copies contain assets and instrumentation.

Pinned reports are `corpus/results/2026-11-05/{native-hosted,native-static,doom-render}.json`.
Their harness/tool/source hashes match the preserved local snapshots. Program
elapsed times are output evidence only, not performance comparisons. No new
linker core changes or timing measurements were needed for this qualification.


### Current checkpoint: hosted native compiler driver

The migration and complete linker/object handoff are now local commits
`8bc3edd` and `97a1bb5` on `native-prep-frontend-fixes`. Claude confirmed merging
them onto `backend-v2` and owns further C ABI/codegen changes there. No commits
were pushed. The handoff preserves the compiler source bytes and excludes
generated Python caches and corpus artifacts outside the checked-in evidence.

Native executable builds now select the C runtime conservatively for C inputs,
imports, packages and external declarations; closed programs retain tool-free
Fas startup. Explicit `--hosted`/`--freestanding` overrides are available.
Hosted links emit a native object without `_start`, ask GCC/Clang for its dry-run
link plan and execute that plan with the Fas linker. CRT discovery has no
hardcoded distro paths and never runs an external linker. Positional C sources
and embedded C/assembly/package objects are supported. C harnesses may supply
`main`. Hosted runtime discovery still requires a configured C compiler; raw
native object emission and explicit freestanding linking remain independent.

Driver regressions cover GCC/Clang, libc, lifecycle, C source and object inputs,
archives/DSOs, no-Fas-main C harnesses, embedded C/header imports and assembly,
thread package flags, argc, glibc PIE/static/static-PIE and isolated musl startup.
They also verify no external linker execution, `.note.GNU-stack`/non-executable
stack, explicit tool-free linking, invalid plans/options, output preservation
and original C-source aliases. Native sanitizers are rejected explicitly.
The existing native shell smoke check now accepts hosted libc and retains an
explicit freestanding unresolved-libc check. Backend ABI expansion is separate;
this branch still enforces its existing external ABI gate.

Final validation passes: the development build, a forced full release run with
isolated musl enabled (243 Python cases across 41 groups, OCaml checks and 3,782
compiler regression checks, no skips), and `test/native_backend.sh`. The smoke
test caught intrinsic declarations being mistaken for C externals; runtime
selection now excludes backend-handled `llvm.*` declarations, with a tool-free
popcount/ctz regression. Evidence is recorded in
`audit/hosted-native-regression-2026-11-05.json`. No new performance measurements
were taken during this driver work.

### Current checkpoint: executable data imports and upstream C builds

After these compatibility changes, the user provided a quiet-PC interval for
real measurements. Added a manifest-based real-link replay harness, plus current
object/all-member/lazy-archive baselines. All comparisons use 21 samples after
three warm-ups, rotated tool order, warm inputs and separate peak-RSS samples.
LLD 22 and GNU ld are compared; mold is unavailable. The baseline confirms a
performance gap, particularly for archive indexes and large input sets.

The first performance change snapshots repeated positional-file bytes by
canonical path within each invocation. It preserves per-occurrence parse/member
state, alias checks and logical file/byte budgets. A new archive regression checks
selection before/after its consumer, symlink aliases, whole-archive duplicates,
explicit muldefs and a repeated 32 MiB input crossing the 256 MiB budget.
The two Chibicc dynamic links show 13%/24% lower median times, 23%/21% fewer
allocated words and 32%/31% lower sampled peak RSS. All four before/after real
outputs are byte-identical. Static Chibicc has no cache hits and no meaningful
gain. Fas still trails LLD on these workloads; no general speedup is claimed.
Raw reports/manifests are under `benchmarks/results/2026-11-05`, with full preserved
replays under `/tmp/fas-real-link-{baseline,read-cache}-20261105`.

After the read-cache change, the full release gate passes 229 Python cases across
40 groups, OCaml checks and 3,782 compiler checks, with musl enabled and no skips.
Chibicc again passes both stages, 126 driver checks and 82 GNU runtime comparisons.
The development build and replay-harness portability check pass. Performance
measurements have finished; the PC may be used normally again. Remaining packing,
archive/index and parallel-work changes belong to the ongoing v0.8 work.

Continuing from the source migration, the unmodified pinned Chibicc build exposed
three real C-driver gaps: output to `/dev/null`, non-PIC imported data (`stderr`),
and shared output supplied with an interpreter. Its driver tests also require
explicit `-z muldefs`. These now have implementation and regression coverage.

The invocation-owned COPY planner runs after GC. Direct executable data imports
get bounded BSS or RELRO storage, provider versions and dynamic COPY records.
Tests cover fixed/PIE GCC/Clang inputs, Fas/GNU/LLD DSO producers and consumers,
8192-byte alignment, pointer/address identity, preload overrides, independent
symbol versions, partial links, archives, repeat/failure/domain reentry, PC64
references and fixed read-only absolute pointers. Read-only copies actually fault
on writes after loader startup. Protected/unique data, overlapping exported
storage, malformed/zero-sized bounds, invalid alignment and mixed spellings of
one copied version fail explicitly. PIC imports retain alias compatibility.
GNU BFD 2.46 and LLD 22 coalesce the combined two-version COPY fixture; Fas keeps
its independent expected values. Individual-version tests compare all consumers.

Null-device output validates the whole image/map and discards it without opening
or replacing the device; symlinks work and other special files remain rejected.
Explicit multiple-definition options keep the first strong definition, including
partial links, without changing weak/common precedence. Shared output ignores
the interpreter option and still rejects incompatible PIE selection.

The new checksum-pinned Chibicc corpus mode builds both stages and runs both
unmodified upstream C/driver suites through isolated test wrappers. All 82 GNU
runtime comparisons and 126 driver checks pass, with 116 actual Fas linker calls
logged. The source uses no LTO. Its bootstrap CC is PIC; Chibicc-generated stage-2
objects use upstream policy and exercise COPY. This gate is now in the workflow;
hosted Ubuntu execution remains unperformed.

Regular/thin archive order/group comparisons establish that indirect DSO
requirements do not extract members, matching GNU ld. Objects, whole archives,
`-u` roots or direct DSO references followed by the archive supply callbacks.
The roadmap now records this boundary instead of an unqualified missing feature.

The pre-read-cache release regression gate passes 228 Python cases across 40 groups, OCaml
checks and all 3,782 compiler checks with isolated musl enabled. One older test
still expected direct data imports to fail; it now tests unsupported direct
function references, with a successful cached Dune follow-up. Both release and
development builds, formatting and Actionlint pass. Fresh COPY/shared campaigns
completed 237,413 executions over two 180-second targets without saved crashes
or hangs. Pinned reports and log hashes are collected in
`audit/copy-driver-regression-2026-11-05.json`; both successful self-hosting and
the initial missing-COPY failure remain preserved. Untimed stage-1/stage-2
allocation profiles and replay commands are under
`/tmp/fas-copy-profile-preparation-20261105`. Timings remain deferred while the
PC is in use. No production-readiness or speedup claim is made.

### Source migration and qualification

The user requested migration to Claude's source layout and continued linker work.
`src/linker/` is now the linker root, with `(include_subdirs no)` separating its
library from the compiler. The 86 renames from layout commit `08289e7` plus the
new native object adapter were applied to the current file contents; backend and
driver edits were preserved byte-for-byte. No commits or index changes were made.
The pre-migration recovery snapshot and move hashes are under
`/tmp/fas-layout-migration-20261105-m4bt86wv`. Claude's Fir work remains on his
`backend-v2` branch. Build/fixture paths and active reproduction commands now use
the new layout; historical JSON evidence retains its original paths/hashes.

Current user policy supersedes the historical overnight notes below: leave the
PC awake, do not compare performance while it is in use, do not poll the agent
inbox unless notified. LTO remains excluded; preserve the existing usable C++
interop level. Backend/IR development belongs to Claude.

Recent additions include explicit sysroots and script search paths, deterministic
publication failure injection, glibc/musl CRT-owned static PIE, a staged CLI
installer, and a separate large-input qualification runner. Static PIE uses
narrow local GOTPCRELX bootstrap rewrites; a regression also protects prefixes
covered by another relocation. The regular static PIE group has nine cases.
The migrated release build and full suite pass: 3,782 compiler checks and 216
Python linker cases across 39 groups, with musl enabled and no skips, plus OCaml
unit/zlib gates. Subsequent focused static PIE runs pass all nine cases,
including ordinary and `-fno-plt` glibc IFUNC calls with LE/IE TLS. The default
development build and staged CLI installation with a space-containing prefix
also pass. See `audit/migration-regression-2026-11-05.json` for pinned evidence.

The interrupted pre-pause static PIE AFL campaign completed 49,906 executions in
79 seconds with no saved crashes/hangs; it did not run the requested 180 seconds.
Its report is `/tmp/fas-static-pie-fuzz-20261105/report.json`. Treat this as short
coverage evidence only. A fresh migrated campaign completed the full 180 seconds:
136,902 executions with no saved crashes/hangs. The revised full scale runner
passes four groups using independent input files, 65,260 retained sections,
200,000 definitions/relocations and a 64 MiB file-backed payload. Reports are
under `fuzz/results/2026-11-05` and `scale/results/2026-11-05`.

Checksum-pinned zlib 1.3.1 upstream configure/make builds pass dynamic, strict
dependency, static and static PIE configurations, with GNU consumers and 82 gzip
cross-decoding runs. The new Linux correctness/fuzz workflow passes Actionlint;
its hosted Ubuntu run and longer fuzz campaigns have not been performed. No
performance comparison or production-readiness claim follows from these results.

### v0.7: static IFUNC, installed static glibc and late C++ lifecycle

Static ET_EXEC now emits canonical IPLT stubs, writable IFUNC slots and symbol-zero
IRELATIVE records. On-demand `__rela_iplt_start/end` boundaries support CRT-owned
resolver startup; no runtime is injected. Six integration groups cover GCC/Clang
PIC/non-PIC, local/global calls and pointer identity, GC, partial links, empty
boundaries, invalid entry points and repeated/failing/concurrent library calls.
Installed glibc startup passes with both compilers and Fas/GNU/LLD consumers,
including arguments/environment, threads, 8192-byte-aligned TLS, memory functions
and constructor/atexit/destructor order. Real GCC/Clang `-static` driver runs pass.
Static libc/libgcc archives and CRT hashes are recorded in the test environment.
Static PIE remains separate work.

An additional GNU/LLD comparison caught eager startup execution of an unreferenced
IFUNC definition. Table selection now requires a live allocatable reference;
unwind/debug metadata alone cannot create a resolver slot. The regression checks
resolver side effects with and without GC and with a debug-only address reference.
The ordinary static path skips the IFUNC relocation scan entirely when no live
IFUNC definitions exist. After this planning change, all 67 Fas links from the
static real-program replay produce byte-identical executables; a separate report
preserves the latest binary hash and every compared output hash. No timing
comparison was made while the desktop was in use.

The installed libc archive required bounded SystemTap v3 notes, section-symbol
COMDAT signatures and multiple GNU property notes within a section. Probe notes
preserve bounded strings, accept R64 only at address fields and do not root dead
code. Dead pointers receive zero tombstones, including nonzero input addends.
Partial-link producer/consumer and GNU readelf checks pass. GNU comparisons
establish OR merging for repeated properties within one object, before the
existing across-object policy. Empty GNU property descriptors are accepted.
Review additionally found that metadata names with an incompatible section type
could reach an unchecked lookup or read; these now fail explicitly, with SDT and
NOBITS unwind regression cases.

A fifth C++ exception group checks late RTLD_LOCAL loading, exception cleanup,
thread-local construction/destruction and unload/reload with GCC/Clang and all
Fas/GNU/LLD producer/consumer combinations. The tested raw-link DSO explicitly
supplies the loader as the provider of `__tls_get_addr`, like existing TLS tests.
The acceptance runner now snapshots and hashes its linker executable before
starting, preventing a development rebuild from changing an in-flight replay.
An opt-in static corpus mode runs Chibicc/Sieve/cJSON against installed glibc
archives, verifies ET_EXEC with no interpreter/dynamic headers and compares the
C programs with GNU ld. It records the static libc/compiler-runtime/libm inputs;
static game dependency closures remain outside this declared corpus mode.

All 71 strict dynamic corpus cases pass after the metadata/unwind work. All 69
recorded static glibc cases pass after IFUNC selection was corrected, covering
Chibicc's compiler and 41 C tests, four Sieve sources and cJSON's 21 tests.
Counts include aggregate project rows. Earlier prose incorrectly said 40
Chibicc tests; the preserved manifests enumerate 41. JSON reports and snapshot
paths are recorded under corpus/results/2026-11-05. The complete release-profile
suite passes with the isolated musl sysroot and all 3,782 compiler checks.

Six fresh 180-second AFL targets completed 668,272 executions with no saved
crashes/hangs. A final ELF/static-emission follow-up rebuilt the latest code,
imported 805 saved queue inputs and completed another 219,322 executions without
findings. Source snapshots, target hashes, seed provenance and statistics are
preserved in fuzz/results/2026-11-05/unwind-static*.json and their listed local
directories. These short runs do not establish production readiness. The roadmap
now gives a direct support/planned/excluded answer for all eight audit areas.

### v0.7: personality/LSDA pointers and focused C++ interoperability

Added bounded CIE z/P/L/R/S parsing, checked personality and LSDA pointer
relocations and accepted compiler exception tables. Section GC now follows
personality/LSDA/typeinfo edges from described live code, retaining ordinary code
collection and dead-FDE tombstones. Unsupported encodings/lengths remain explicit;
LSDA payloads are preserved, not interpreted as a new runtime implementation.

Four exception integration groups pass with GCC and Clang, installed libstdc++
and libgcc: cross-object cleanup/rethrow, -r, fixed/PIE/GC, exceptions across DSOs
in both directions, RTTI/dynamic_cast/typeinfo identity, virtual destruction and
C++ global initialization/shutdown. Fas/GNU/LLD producer/consumer combinations
run the same expected results. GNU/LLD comparisons request --eh-frame-hdr, as
compiler drivers normally do. GNU can retain more code after the tested partial
link; behavior and cleanup agree. No full C++ ecosystem support is claimed.

The DSO matrix exposed valid LLD multi-provider VERNEED tables with all headers
before their auxiliaries. Replaced the interleaving assumption with bounded
interval overlap checks for definition/requirement records and an aggregate
one-million-record parser cap. A separate multi-provider discovery test compares
all three producers/consumers and rejects overlapping records. Augmentation
negative cases and 256 deterministic mutations return controlled results.

### v0.7: relocatable output, driver metadata and library discovery

Implemented positional as-needed policy, static/dynamic library selection and
push/pop state. Direct GCC/Clang and isolated musl compiler-driver tests pass;
GCC still needs `-fno-use-linker-plugin`. Added independent GNU/both hash output
with stable dynsym remapping, and SHA-1/MD5 GNU build IDs over the final image
with the descriptor zeroed. Lookup tables and identifiers have independent
Python/loader tests, including versions, IFUNCs and TLS indices.

GNU thin archives now lazily read external members relative to the archive,
including nested references into regular archives. Missing unselected members
follow GNU ld's policy; installed LLD 22 differs. Selected reads share input
budgets, cache invocation-local containers and receive publication alias checks.
Five thin-archive integration groups and the existing archive suite pass.

`-r` combines objects into ET_REL, retaining unresolved/hidden references,
common symbols, COMDAT, relocation/symbol remaps and metadata. Seven integration
groups cover multi-stage and archived partial output, GCC/Clang debug/TLS/
lifecycle inputs, versions/IFUNCs and Fas/GNU/LLD producers/consumers. Independent
consumers exposed metadata details: GNU/LLD section symbols can refer to consumed
stack/addrsig markers, and sh_link=0 invalidates an address-significance table.
Mixed GCC/Clang input now conservatively preserves address identity under LLD
safe ICF. Repeated/failing/concurrent library calls remain deterministic.

Added RPATH/RUNPATH selection and explicit strict DSO validation. The strict
closure uses selected identities, `-rpath-link` and DSO paths; bounded metadata
parsing validates VERNEED records. It checks strong references, TLS types and
required provider version nodes without creating extra direct dependencies.
Indirect executable callbacks survive GC/export selection. Six discovery groups
compare Fas/GNU/LLD, including transitive runtime chains, path inheritance and
environment precedence, cycles, weak/versioned references, hidden callbacks,
malformed records, input alias protection and FIFO/oversized-read failures.
Default strictness and complete GNU/sysroot search remain qualification work;
indirect references do not trigger another archive extraction pass.

Real GCC slim/fat LTO and Clang full/thin bitcode have explicit unsupported-input
diagnostics in normal/partial/archive links. A roughly 1 GiB NOBITS image is
independently inspected against GNU ld without running or materializing it;
over-limit image rejection and GC removal preserve small output and old files.
The roadmap now explicitly tracks every item from the user's compatibility audit.

The complete release-profile regression suite passes with the isolated musl
sysroot, including all 3,782 compiler checks. Fresh metadata/thin/shared parser
campaigns and partial-link/ELF campaigns each ran 180 seconds per target with no
saved crashes or hangs; reports are preserved under fuzz/results/2026-11-05.
DSO-requirement fuzzing and real-program replays have preserved reports. Short campaigns remain insufficient for production
qualification. The PC stays awake; no new performance comparisons were run.

All 71 recorded real-program cases pass the normal-policy replay. Strict replay
also passes Chibicc, all four Sieve sources, cJSON and Doom. Strict Wolf3D exposed
GNU unique exports in an indirect libstdc++ dependency. Added DSO input support
while preserving normal undefined import binding; the runtime keeps the original
unique definitions. Focused GNU/Fas/LLD consumer and RTLD_LOCAL identity tests
pass, and strict Wolf3D subsequently passes the same rendering harness. ET_REL
GNU unique definitions and full C++ interoperability remain separate work.
The fresh DSO parser and in-memory requirement validation campaigns completed
110,291 and 122,429 executions respectively, with no saved crashes or hangs.
The GNU unique follow-up adds 110,076 parser and 110,233 validation executions
with no saved findings. These snapshots precede the next deterministic path fix.

Strict discovery now expands DT_NEEDED tokens before path classification, checks
unbraced ORIGIN identifier boundaries and keeps provider associations per
referrer. Distinct expanded paths can identify different unversioned DSOs with
the same token SONAME; Fas/GNU/LLD outputs pass loader-backed tests. Versioned
DSOs whose SONAME contains tokens remain unqualified: an exploratory two-provider
case reached a glibc loader assertion. No support claim follows from that case.

### v0.6 acceptance and v0.7 compatibility, 09:25

The declared v0.6 shared-library subset now passes acceptance: added eager
TLSDESC, local/imported/weak descriptors, late-thread TLS, defined IFUNC resolvers
with dependency relocation ordering and canonical local addresses, and multiple
named `.symver` aliases including lazy archive extraction, TLS and IFUNC aliases.
The documented version-hiding policy matches LLD; GNU ld differs in the tested
wildcard/exact-local corner cases. No base-version aliases/static IFUNC claimed.
All 3,782 compiler checks and linker suites passed before moving into v0.7.

Real-program compatibility exposed compressed debug sections and non-PIE GCC
CRT weak references. Added independent bounded OCaml zlib/DEFLATE decoding with
3,550 differential cases, malformed-header/checksum and relocated DWARF tests.
Decoded storage shares the invocation input budget, including lazy archives.
Fixed narrow absolute absent-weak references bind to zero. Added static LE/IE/
DTPOFF TLS and lifecycle array boundaries; runtime startup owns TLS initialization.
Default tests include an independent static startup that installs FS from PT_TLS.
The full suite passes again with musl acceptance enabled, after updating two old
fixtures whose assumption that all static TLS must fail is now obsolete.

An isolated musl 1.2.5 sysroot passes static/fixed/PIE lifecycle/stdio/arguments,
threads/errno and all declared dynamic TLS models, plus late dlopen TLS.
Chibicc's GCC-built compiler linked by fas-ld compiles all 40 upstream C tests;
all pass and match GNU ld. All four original Sieve sources, including zero3,
run using LLVM objects + fas-ld. Native compilation still rejects variadic calls.
Doom's 62 C objects link fixed/PIE and match GNU startup failure behavior;
no WAD/gameplay acceptance claimed. An instrumented Wolf3D snapshot renders
three E1M1 frames with the same framebuffer hash as GNU ld. cJSON v1.7.19 passes
all 21 upstream core/utility test programs with matching GNU outputs.

Corpus scripts preserve source/toolchain hashes and commands; download sources
are pinned by checksum. External project trees and the host sysroot are unchanged.
Results and explicit scopes are under corpus/results/2026-11-05. The PC remains
awake; no performance comparisons were attempted while the desktop is in use.

Fresh 180-second fuzz targets for elf/archive/symver-link and zlib/elf/static-link
saved no crashes or hangs. Reports are preserved under fuzz/results/2026-11-05;
short campaigns remain insufficient for production qualification. Broader driver
compatibility, static glibc, scaling/performance, sustained fuzzing, CI/packaging
and later-platform work remain. Changes remain uncommitted for review.


### v0.6 checkpoint, 08:29

Continued after the reported UI interruptions; local build and test commands
succeeded. The PC remains awake per the user's latest instruction. No new
benchmarks were run while the desktop was in use.

v0.6 is in progress: independent ET_DYN shared output, SONAME, default/protected/
hidden symbol binding and preemption, strict undefined-symbol modes, shared GD/
LD/IE TLS, default RELRO, and bounded version-script export policy/VERDEF emission
are implemented. Version definitions coexist with imported version requirements.
System-loader tests cover GCC and Fas consumers, dlopen lifecycle/callbacks,
pre-existing threads with large late-loaded GD/LD TLS, pointer identity, and
versioned Fas-produced libraries. Independent concurrent/repeated library calls
preserve determinism and recover after expected failures.

AFL++ campaigns found a metadata PLT relocation exception: nonallocatable
relocations could request an unplanned runtime stub. Fixed optional-stub handling
and local/global name selection, with deterministic regression cases. Two saved
crashes and two saved hangs all reproduce that same exception before the fix;
all four replay successfully afterwards. A fresh 180-second shared-link campaign
completed 111,429 executions with zero saved crashes or hangs. Five earlier
parser/static targets and the version target also saved no findings. Reports and
replays are preserved in `fuzz/results/2026-11-05`; snapshots/queues remain in the
documented temporary directories. Short campaigns are not a production gate.
Tool versions and local system-library hashes are recorded in
`test/environment-2026-11-05.json`.

Validation passed: `dune build --profile release @all`, full
`dune runtest --profile release` (3,782 compiler regression checks plus frontend
units and linker suites), OCaml formatting, Python syntax and `git diff --check`.
Current additional integration groups include native9, dynamic3, shared-input7,
hosted3, TLS5, scripts5, library2, hardening3 and shared-output9.

Remaining v0.6 work includes TLSDESC, defined IFUNC resolvers, multiple .symver
output aliases and wider compatibility coverage. Broader release testing,
scaling/optimization, packaging/CI and later-platform support remain roadmap
work. This checkpoint does not establish production readiness or a speedup over
LLD/mold. Changes remain uncommitted for review.

### v0.5 checkpoint, 07:26

Resumed after the usage interruption. User explicitly requested leaving the PC
awake; the previous overnight suspend instructions are superseded. No suspend
timer is registered or being restored. User authorized continuing until finished.

The declared v0.5 subset now includes four traditional TLS code models, PT_TLS,
initialized/zero/over-aligned templates, imported TLS and per-thread isolation.
GCC/Clang and seeded GNU-ld oracle fixtures pass. Minimal implicit INPUT/GROUP/
AS_NEEDED scripts support the installed libc.so script, including archive fixed
points, script cycles/limits and expanded-input output/map alias protection.
Hosted Fas objects run through explicit libc CRT startup with external linker
tool lookup disabled. Concurrent/repeated dynamic library calls reproduce bytes
and maps, including recovery after failed calls.
Current additional suites: native8/dynamic3/shared7/hosted3/TLS5/scripts5/library1.
Linker suites and OCaml formatting pass; final full-repository checks pending.
README/ROADMAP identify the implemented subset and remaining compatibility gates.
Adding bounded AFL++ coverage-guided campaigns before proceeding to v0.6.
No benchmarks run while the user is using the PC.

### v0.5 progress, 04:50

Loader-backed fixed/PIE output, eager PLT/GOT imports, RELATIVE/GLOB_DAT/64 dynamic
relocations, SONAME dependencies, RUNPATH and required default/explicit symbol
versions pass system-loader and GNU-ld oracle tests. SysV/GNU hash input is
validated, including bloom/chain coverage. Shared metadata receives seeded
mutations. Executable callbacks/preemption/export policy and archive roots pass
with section GC. Native Fas external calls now emit PLT32 and run against DSOs
with external linker tools disabled. GCC/Clang hosted programs run against the
installed glibc CRT/libc plus libc_nonshared.a: arguments/environment, stdio,
atexit, constructors/destructors, numeric priority/tie ordering, preinit and
aligned init/fini fragments. Valid symbol-table-free CRT input is supported.
New integration groups: dynamic3, shared7, hosted3; native now7.
v0.5 is still incomplete: required TLS and minimal sysroot scripts remain.
Full repository checks are running; no new benchmarks. 06:00 timer unchanged.

### v0.4 checkpoint, 04:21

Declared freestanding subset implemented and documented. Added basic `.eh_frame`
normalization/validation, combined section, live FDE index and GNU_EH_FRAME header;
GCC/Clang CFI and GDB without frame pointers pass. Empty/terminated records and
dead first contributions are covered. Personality/LSDA and C++ EH remain unsupported.
Five GNU x86 bitmask properties merge against generated GNU ld oracles; malformed
notes/mutations checked. Missing input markings clear CET claims; synthetic
common/predefined objects are excluded from the presence count. LLVM addrsig is
validated and consumed. Compiler now supports -Wl/-Xlinker forwarding; archive
groups and response-file native integration passes.
Full `dune build --profile release @all`, formatting, and `dune runtest --profile
release` passed (3,782 existing regressions plus frontend units and linker suites).
Current suites: base17/archive15/layout5/sections10/PIC4/debug3/unwind5/properties3/
native6 plus object-emission/OCaml units/reentry tests. No new timing benchmarks.
Starting v0.5 with loader-backed closed-world PIE/RELATIVE relocation foundation;
dynamic imports/startup/libc/TLS/versioning still required before v0.5 is complete.
Timer remains 06:00, no postponement yet.

### v0.4 progress, 03:59

Passing additions: validated COMDAT/non-COMDAT groups; first COMDAT group wins,
discarded local symbols omitted, global references canonicalized. Section GC with
entry/`-u`/SHF_GNU_RETAIN roots, group-unit retention, canonical/weak edges and dead
cycle elimination. Common BSS is currently collected conservatively as one unit.
Merge flags/entry sizes validated; whole identical read-only relocation-free
sections deduplicated (individual string/tail merging not implemented).
Static GOT supports GOTPCREL/GOTPCRELX/REX_GOTPCRELX and GOTPC32/GOTPC64, no relaxation;
GCC/Clang PIC -O0/-O2/fno-plt tests pass. New Layout module combines nonallocatable
section contributions with section-relative offsets for DWARF relocations.
LLVM addrsig validated/consumed without ICF. GCC/Clang DWARF4/5 multi-object and
GDB source breakpoint/local-variable/backtrace tests pass, including GC.
Current linker integration counts: base17 + archives15 + layout5 + sections10 +
PIC4 + debug3 + native6, plus object-writer and OCaml boundary/reentry tests.
Next: unwind parser/GC handling/eh_frame_hdr and debugger backtraces without frame
pointers. v0.4 remains incomplete; README matrix needs updating for its new subset.
No benchmarks run after user started using PC. Suspend timer still 06:00.

### v0.3 checkpoint, 03:44

Independent checked ELF ET_REL writer and native backend bridge implemented.
Native `-c` needs no main/startup; native executables now use fas-ld, with additional
objects/archives and a deliberately limited integer/pointer external ABI checked
against GCC. Actual integer/float/vector/argc fixtures pass with tool lookup
disabled. Independent GNU readelf/ld object tests and six native integration
groups pass. Map files/predefined layout symbols pass five independent layout
groups. Full repository suite passed (3,782 checks plus frontend units). Current compiler remains OCaml;
self-hosting is not claimed. Next milestone: v0.4, kept as incomplete until all
COMDAT/merge/GC/PIC/metadata/debug/unwind acceptance cases pass.

User is using the PC from approximately 03:41 onward: any subsequent performance
samples must be labelled as taken under concurrent desktop activity and must not
be used to claim speedups. No new benchmark run is necessary for this checkpoint.

## Overnight session: 2026-11-05, Europe/Warsaw

User authorized continued roadmap implementation until **08:00**. Stop by that
time. Current one-time user timer is `suspend-at-0600.timer`, scheduled for
06:00. **Only while still actively working near 06:00**, create/verify a 07:00
suspend timer and then cancel the 06:00 timer. If still working near 07:00,
create/verify an 08:00 timer and cancel the 07:00 timer. Leave the last timer
intact if usage ends early. Do not install an unconditional background task that
postpones suspension independently of whether this session remains active.

Check local time regularly with `date --iso-8601=seconds`. Timer commands use
`systemd-run --user` and `/usr/bin/systemctl --no-ask-password suspend`; suspension
permission was verified from a user service. Root sudo requires authentication
and is unnecessary. The user session must remain logged in (`Linger=no`).

### Baseline

- v0.1 is implemented under `fas/linker/` and remains uncommitted.
- Release build, 17 linker integration tests, boundary unit tests, and the full
  repository test suite passed; existing regression checks: 3,782.
- Baseline benchmark/report is under `linker/benchmarks/` (257 synthetic objects:
  fas-ld 55.21 ms, LLD 33.89 ms). Do not overwrite the original baseline.
- No compiler integration or native object emission yet.

### Current work: v0.2

Checkpoint at 03:22: v0.2 GNU/SysV archive subset implemented and 32 integration
tests pass (17 base + 15 archive/CLI), plus boundary units and repository suite.
New modules: File_io, Stats, Archive, Input, Resolver, Response_file. CLI supports
library paths/names, groups, whole-archive, response files and JSON statistics.
BSD/thin archives remain explicitly unsupported. GNU oracle exposed a real weak
selection bug: indexed extraction must follow forward passes, deferring newly
needed earlier members. Fixed with pending/deferred indexed sets. Queued members
that become unnecessary stay unparsed. Reentry/concurrency tests also use archives.

Initial v0.2 benchmark JSONs preserve pre-optimization measurements; separate
`*-optimized.json` files measure deferred diagnostics, nonallocating successful
reader bounds checks and a sorted-relocation fast path. Original initial.json is
untouched. Record actual comparison in benchmarks/README.md before v0.3.

Next: v0.3 independent ELF object emission, bridge native backend through the
new linker, native `-c` without main/startup, supported additional static objects,
map files and basic linker-defined symbols. Check real freestanding fixtures;
do not silently broaden native C ABI support (float/aggregate ABI differs).

Implement in this order, keeping passing checkpoints:

1. Generic bounded regular-file input and statistics collection.
2. Weak/common/visibility reading and canonical symbol resolution.
3. Bounded archive reader (GNU/SysV index and long names; BSD formats if tested),
   lazy extraction, deterministic member selection, traditional input order and
   explicit group fixed points. Use an indexed pending-member worklist; avoid
   repeated scans of every archive member.
4. CLI response files, `-L`/`-l`, archive groups and whole-archive flags.
5. Independent GNU ld oracle tests for precedence, extraction/cycles/order,
   malformed archive/index inputs and atomic failure paths. Update docs and
   benchmarks only from actual results.

Planned structure: `File_io`, `Stats`, `Archive`, `Input`, `Resolver`; retain the
existing simple `Static_link.link`/`link_files` API as wrappers. Resolver owns
canonical global definitions and emits a synthetic BSS object for winning common
symbols. Writer emits only winning global definitions and all local symbols.
Unresolved weak references become zero; strong unresolved relocation references
remain errors. Visibility propagates to canonical output symbols.

Later priorities follow `ROADMAP.md`: v0.3 native ET_REL emission/compiler
integration, then v0.4 compiler ELF metadata/GC/PIC/debug/unwind. Preserve the
current compiler path while the independent linker grows. Do not claim full
milestones complete before their acceptance gates pass.
