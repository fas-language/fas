# Partial links, version visibility and semantic qualification — 2026-11-06

This checkpoint fixes partial-link semantics and a symbol-version visibility bug.
The linker remains `0.7-dev`. Ordinary correctness qualification passes; the
fuzz acceptance gate remains **incomplete because one saved timeout needs triage**.
No production release, hosted CI execution or performance result is claimed.
The [compatibility contract candidate](../../../SUPPORT.md) defines the intended
Linux Fas/C boundary, including the source-version-coupled internal OCaml API.

Valid empty archives and selections with no extracted objects now produce reusable
ET_REL objects. No input files remains an error. Undeclared `-u NAME` roots survive
partial stages as strong undefined symbols and select providers from later
archives. This follows GNU's policy; LLD 22's partial-link policy differs.
Regressions cover regular/thin archives, repeated stages, independent final
consumers, weak/defined roots, deterministic ordering and output preservation.

The full fourteen-target weekly fuzz selection found an uncaught `Not_found`
while planning a call through a locally bound spelling of a default-version
symbol. Ordinary valid GCC/assembly objects reproduce the same failure. The
repair propagates visibility across the default-version alias family and tests
the original spelling's import eligibility before sharing a dynamic index.
Fas/GNU/LLD producer/consumer tests verify runtime preemption and exports:
internal/hidden versions stay local, protected versions retain protected
visibility, and the separate older version remains exported. The interim
import-only repair and its interrupted qualification are explicitly superseded.

| Selection | Recorded result |
| --- | --- |
| Workstation root gate | 3,792 compiler checks; 264 Python linker cases in 44 groups; no skips |
| Fresh Ubuntu shared recipe | All 264 linker cases, eight scale/corpus/semantic reports, staged install and source/binary identity checks pass |
| Generated valid programs | 64 seeds; 3,072 final links/executions and 256 partial links; independent Python oracle and Fas/GNU/LLD consumers |
| Fixed CI semantic selection | Eight seeds; 384 final links/executions and 32 partial links; every required pipeline/mode/consumer checked |
| SQLite | Both linkers pass the same 1,300 `extraquick` scripts: 398,223 Fas / 398,249 GNU assertions, zero errors |
| Six popular C projects | Lua 5.1.5, bzip2 1.0.8, libpng 1.6.47, libarchive 3.8.11, curl 8.14.1 and Redis 7.2.8 pass their selected upstream tests with GNU controls |
| Current-binary C relinks | All 355 retained ELF outputs are byte-identical to the images executed in that upstream campaign |
| Shared-library interoperability | Bzip2, libpng, libarchive and curl pass both GNU-consumer/Fas-library and Fas-consumer/GNU-library checks |
| Archives and guard controls | Source rebuild and linked fixture pass; independent repackaging is byte-identical; all 13 selection/qualification controls pass |
| Fuzz follow-up | Eight fresh output targets at ten minutes each: zero saved crashes, one untriaged shared-output timeout; fuzz gate incomplete |

The [semantic harness](../../../semantic/README.md) compares direct objects, lazy
archives and both GNU/Fas partial-link orders, with static/static-PIE output and
GC on/off. It varies graph structure, constants, order, alignment, weak/strong
definitions, common BSS, retained/dead sections and GOT/PC-relative access. A
deliberately incorrect Python oracle is rejected after a real successful program
execution. These PIEs use PC-relative access, not a general CRT self-relocator.

The popular upstream runtime campaign began before the visibility repair. Its
original linker identity is retained. The repaired linker then relinked every
retained ELF output and matched the executed bytes exactly; object/archive hashes
and recorded toolchain inputs were checked. This is byte-equivalence evidence,
not a second full upstream runtime campaign. All source files captured for the
six projects match their checksum-pinned archives. Libpng selects 33 tests;
curl reports 1,563 OK per linker under Valgrind. Upstream skips, optional features
and ignored flaky selections remain visible in the full reports. Redis selects
14 core units, excluding its large-memory/cluster corpus.

SQLite's assertion-count difference is confined to randomized `fts5bigtok.test`
(277 versus 303). Both runs pass; their script selections match. The clean
recipe also repeats full scale checks, glibc zlib modes, 48 musl final links and
96 gzip executions, Chibicc self-host/126 driver checks/82 runtime comparisons,
and the SQLite API/native Fas consumer matrix. It uses the pinned Ubuntu 24.04.5
userspace image with GCC 13/Clang 18, glibc 2.39 and isolated musl 1.2.5, sharing
the workstation kernel. The workstation adds newer producer tools.

The [fuzz report](fuzz.json) separates the original fourteen targets, the earlier
enhanced partial-root campaign and the eight final-source output campaigns. The
original symbol-version finding was imported into the final campaign, which
saved no symbol-version crashes. Every run keeps its own target/source hashes,
queues, durations and coverage counters. Across all 23 completed target budgets,
10,544,209 executions are recorded. That aggregate includes the original failing
campaign and does not describe a single final-source passing campaign. AFL edges
are not general source/branch coverage, and ten-minute local budgets are not
continuous hosted fuzzing. The [pending timeout](pending-timeout.json) is neither
dismissed as machine noise nor classified as a confirmed linker hang.

Evidence: [host gate](host-regressions.json), [clean runner](clean-run.json),
[selection summary](qualification-summary.json), [corpora](corpus-reports.json),
[semantics](semantic.json), [popular projects](popular.json),
[byte-equivalent relinks](popular-relink.json), [shared consumers](cross.json),
[original finding](finding.json), [superseded runs](superseded-runs.json),
[partial-link gap probe](partial-probe.json), [release controls](release-controls.json),
[build information](build-info.json) and [integrity](integrity.json).

The [source manifest](source-manifest.json) identifies the tested snapshot. This
evidence and final documentation edits are not retroactively inserted into its
archives. Full logs, inputs, executables, queues and archives remain under
`/home/glonex/.cache/fas-linker-qualification`, especially `ubuntu24-family-20261106`,
`semantic-family-full-20261106`, `popular-semantic-fixed-20261106`,
`popular-bindings-relink-20261106`, `popular-bindings-cross-20261106` and
`fuzz-family-*-20261106`. The initial popular run used download parent directories
instead of extracted source directories; its failed setup remains recorded.

The untriaged timeout, hosted execution, continuous qualification, final contract
review and publication remain open. Performance stays deferred; LTO remains out
of scope and C++ stays at its existing usable interoperability level.
