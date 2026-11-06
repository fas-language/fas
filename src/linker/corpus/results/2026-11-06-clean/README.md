# Fresh Ubuntu and compiler/libc qualification — 2026-11-06

The complete [shared correctness recipe](../../../ci/README.md) passes in a fresh
Ubuntu 24.04.5 userspace with OCaml 5.4.1, Dune 3.24.2, GCC 13.3, Clang 18.1.3,
glibc 2.39, binutils 2.42, GDB 15.1 and Valgrind 3.22. This local container shares
the workstation's Linux 7.0.0-42 kernel. It establishes neither hosted GitHub
Actions execution nor a different kernel's compatibility.

| Selection | Observed result |
| --- | --- |
| Forced linker regressions | 250 Python cases in 42 groups, no skips; OCaml checks pass |
| Scale | All four full-profile groups pass |
| glibc zlib 1.3.1 | Dynamic, static ET_EXEC and static PIE upstream tests and GNU controls pass |
| musl 1.2.5 + zlib | GCC 13 / Clang 18 objects, three linkage modes, 48 final Fas/GNU links and 96 gzip cross-consumer executions pass |
| Chibicc | Both self-host stages, 126 upstream driver checks and 82 same-input GNU runtime comparisons pass |
| SQLite 3.54.1 | 40 C API executions, four native Fas consumers and upstream `main.test` (95 assertions per linker, zero errors) pass |
| Installation | Installed binary matches the qualified binary; assembled fixture links and runs |

[The additional SQLite `extraquick` replay](sqlite-extraquick.json) passes all
1,300 isolated scripts with both retained executables: 398,247 Fas and 398,257
GNU assertions, zero errors. The ten-assertion difference is confined to the
randomized FTS5 big-token test; the selected files match exactly. Fixture and
CLI hashes remain unchanged. This supplements the normal recipe's `main.test`
selection without relabeling it as a weekly/manual full-recipe execution.

The installed Ubuntu linker SHA-256 is
`4385e2ddca5ead07737617155cb48bd35f280d27893db9ef3c9aa1732d958918`.
Its immutable toolchain image is
`sha256:aa87b9d35c03f5128095052cb377c52e776275203270eb0d5f76ed86558537b7`.
[The source manifest](source-manifest.json), [package inventory](packages.tsv),
[runner report](clean-run.json), [selection summary](qualification-summary.json)
and [corpus details](corpus-reports.json) identify the exact snapshot, including
uncommitted changes. The repaired core is committed as `95e48b6`; its source
hashes match the qualified snapshot. The image uses online apt/opam inputs;
recording their resolved versions does not make the build hermetic.

[Workstation regressions](host-regressions.json) pass 3,792 compiler checks and
all 250 linker cases with musl enabled. [Workstation musl qualification](host-musl-zlib.json)
adds GCC 15 / Clang 21 / Clang 22, another 72 final links and 144 gzip cross-consumer
executions. All selected compilers emit native payload objects; configure probes
use musl-gcc/GNU. These checks cover zlib archive consumers, not zlib DSO output,
Clang driver discovery, static TLS relaxation or the C++/LTO ecosystem. Ubuntu and
workstation executable hashes differ; each record identifies its actual build.

The clean environment exposed and helped repair two linker bugs: missing final
`.eh_frame` termination made legacy libgcc scan into unrelated data during static
glibc startup, and nonadjacent `.rodata*` ranges prevented Valgrind 3.22 from
reading debug information. The same compiled C object crashes with the original
Ubuntu-built linker and returns 42 with both the repaired linker and GNU control.
The fixed unwind stream ends inside its section; GDB observes normal termination.
The regression suite now checks that terminator and read-only range adjacency.

Clang 18 also lacks an optional Fas C-import parsing flag, now capability-probed.
The synthetic TLS fixture now initializes its TCB self pointer, and the sparse
NOBITS test allows older GNU as alignment padding while independently requiring
NOBITS storage. Container setup now bounds the file-descriptor limit for Valgrind
and installs `file`, required by Chibicc's upstream tests. These fixture and setup
issues are distinguished from linker defects in [repairs and controls](repairs-and-controls.json).
Three superseded failed clean runs remain recorded; they are not passing evidence.

[Artifact integrity](integrity.json) rechecks original source trees, native
musl inputs/outputs, toolchain files and linker snapshots. [Selection controls](selection-controls.json)
accept the full selection and reject both an incorrect expected case count and
an injected reported skip. A forced one-second timeout stops the runner's own
container; the intentional failure and successful cleanup are recorded separately.

[Four fresh fuzz campaigns](fuzz.json) run for 120 seconds each and complete
338,437 executions with no saved crashes or hangs. [Their provenance](fuzz-provenance.json)
checks the preserved core source against this qualification. These are short,
local campaigns, not sustained release fuzzing or proof of semantic correctness.

Full artifacts remain at
`/home/glonex/.cache/fas-linker-qualification/ubuntu24-qualified-20261106`.
Musl interpreter paths use `/qualification`; replay those binaries with the same
container mount. The prior [seven-project/GCC torture evidence](../2026-11-06/README.md)
qualifies its separately identified older binary and is not relabeled as a full
rerun of this core. Hosted jobs, sustained fuzzing, broader release qualification
and distributable release artifacts remain open. Performance work stays deferred;
no production-readiness or speed claim follows from this checkpoint.
