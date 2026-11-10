# First single-thread optimization batch — 2026-11-10

The largest gain comes from **not sorting already ordered target sections**.
The user-provided idle window measures SQLite with debug information at
404 → 302 ms with its requested build ID, and 307 → 202 ms without an ID.
These are about 25% and 34% less elapsed time, respectively. They do not mean
Fas has reached mold/reld: their no-ID medians in the same window are 24/36 ms.
Parallel parsing and other parallel passes remain deferred.

## Code kept and experiments rejected

1. Preserve the ordered-relocation validation path without a scratch array. For
   unordered inputs, identify target sections whose ranges need sorting and
   copy/sort only those targets, using merge sort. Keep the original relocation
   order for emission and validate overlaps across multiple RELA tables for the
   same target. Targets are checked in section order for diagnostics.
   Zero-width `TLSDESC_CALL` markers retain the previous whole-array heap-sort
   fallback, including its equal-offset validation behavior.
2. Size archive symbol-index tables from the validated entry count, with an
   initial reservation capped at 65,536. Validate the offset-array span before
   reserving. Keep seeded hashing, existing count/name limits, lazy extraction,
   member/weak-symbol ordering and version aliases. Larger valid tables can grow.
3. Load complete SHA-1 input blocks as big-endian 32-bit words; use the previous
   byte/padding path only for the final one or two blocks. Digest, requested
   build-ID style and output bytes remain unchanged; no dependency is added.
4. Return false immediately from canonical IFUNC membership tests when their
   table is empty. This avoids resolving definitions and allocating keys for
   ordinary absolute relocations. Nonempty tables retain their existing path.

The initial whole-array merge-sort experiment is retained as measurement
history, not as the final algorithm. Inspection found just one backwards run
in each large SQLite object's text relocation table: 134 trailing records in
the shell object and 391 in the amalgamation. Sorting all 51,642/249,110
relocations to validate those text ranges was unnecessary; the text tables
contain only 7,576/11,758 records. Most of the rest are ordered debug relocations.

A direct two-field `r_info` decoder was **rejected**. It reduced SQLite
allocation from 21.74M to 19.93M words, but increased instructions by about 1.5%
and did not establish a whole-link speed gain. The 15-sample comparison and
source snapshot remain in the evidence. Lower allocation alone is insufficient.

## Reference builds and comparison controls

Fas starts from `25f6e5be35dc3784124091101df59323031fcda0`; its linker runtime
sources are unchanged from released 1.0.0 source
`3cf2962665405578b7a32a35f25e810e2ff188d5`. The locally compiled baseline SHA-256
is `852f44d2bb25367a42ea2207a5c69398dafbcecde87025e508c9494ccb7f2ab5`.
The preceding [probe](2026-11-07-serial-probe.md) verified its outputs against
the delivered binary. Comparisons below use this local baseline and the same
OCaml 5.4.1 toolchain for the candidates.

Both previously pinned reference repositories now build and pass the 14-input
preflight. They remain separate tools, not Fas dependencies:

| Tool | Revision | Build | Runtime arguments |
| --- | --- | --- | --- |
| mold 3.0.0 | `331ac663668dc0b1f4949403854613b9b1913025` | `cargo +1.98.1 build --release --locked -p mold --no-default-features --features x86_64 -j 2` | `--threads=1 --no-fork` |
| reld | `6ab41368d686c7c2eec570182dd11d7d6ddf1efe` | `cargo +1.98.1 build --release --locked -p reld --bin reld --no-default-features -j 2` | `--engine=reld --threads=1 --no-fork` |
| Ubuntu LLD 22.1.8 | binary recorded in each report | installed system executable | `--threads=1` |

Rust 1.92 rejected the references' minimum compiler requirements. Rust 1.98.1
was installed explicitly; the default toolchain was not changed. The reference
builds finished before timings. reld's native engine is forced, and its LLD
bridge is not compiled in this build. These are warm-cache full links, with no
daemon/state reuse and no fork-based early return.

Build-ID costs are not equivalent merely because flags match. In these pinned
sources, mold's `sha1` style selects a 20-byte BLAKE3-based tree digest, and
reld's `sha1` path produces BLAKE3. These workloads use bare `--build-id`:
mold chooses that 20-byte style, while reld defaults to its 128-bit XXH3 fast
style (chunked for large outputs). Fas keeps its actual SHA-1 algorithm.
Source: [mold build-ID writer](https://github.com/rui314/mold/blob/331ac663668dc0b1f4949403854613b9b1913025/elf/passes.rs#L4154),
[mold style parser](https://github.com/rui314/mold/blob/331ac663668dc0b1f4949403854613b9b1913025/elf/cmdline.rs#L1775),
[reld build-ID writer](https://github.com/forge-ci/reld/blob/6ab41368d686c7c2eec570182dd11d7d6ddf1efe/crates/reld-core/src/elf_writer.rs#L214).
The no-ID cases separate this difference from the main linking comparison.

## Idle-window measurements

The user reported the PC idle for five minutes. The 21-sample, three-warm-up
block started at 17:26:54 CET and finished within that window. Tools ran
sequentially with rotated order. There were no concurrent builds, tests,
profiles or counter collections; only source inspection. No CPU affinity,
governor, cache or suspend setting changed. Background services remained active.
Start/end load averages and every sample are retained. Tiny cases remain
variable; this is not a universal ranking or a latency guarantee.

This block predates the small SHA-1/empty-IFUNC additions and the legacy
zero-width-marker fallback. The latter does not apply to these SQLite objects.
The final source is measured and checked separately below.

**Median elapsed milliseconds**, spawn through exit:

| Workload | Baseline | Whole-array merge + index | Per-target sort + index | mold, 1 worker | reld, 1 worker | LLD, 1 worker |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| 256 objects + startup | 57.59 | 56.66 | 56.64 | 11.33 | 28.20 | 35.22 |
| Archive, 1 of 256 needed | 22.10 | 10.51 | 10.24 | 6.50 | 9.79 | 28.68 |
| Archive, all 256 needed | 70.23 | 72.59 | 74.58 | 17.05 | 12.47 | 29.28 |
| SQLite, debug + ID | 404.25 | 353.29 | 302.48 | 27.72 | 37.33 | 57.48 |
| SQLite, debug, no ID | 307.42 | 259.83 | 201.61 | 23.66 | 35.99 | 54.82 |
| SQLite, stripped + ID | 99.45 | 97.44 | 95.04 | 13.36 | 22.89 | 34.31 |
| SQLite, stripped, no ID | 84.18 | 78.90 | 79.07 | 20.86 | 20.68 | 36.20 |

The sparse-archive elapsed change is much larger than its instruction reduction;
do not extrapolate it as a stable 2x index speedup. Dense archives do not show
a whole-link gain in this block. The algorithm is aimed at unordered relocation
scaling, not a uniform percentage improvement on all inputs. Separate RSS
samples for SQLite debug/no-ID are 96,648 → 92,140 KiB; allocation totals are
cumulative and must not be called peak or live memory.

## Final candidate repeat and validation

The later 21-sample/three-warm-up repeat starts at 17:35:42 CET, after the idle
window was released. Treat this as a shared-PC repeat; do not mix its samples
with the earlier block to calculate improvements. It includes the final
zero-width-marker fallback, SHA-1 block loads and empty-IFUNC guard.

| SQLite workload | Baseline wall ms | Final wall ms | Baseline CPU ms | Final CPU ms |
| --- | ---: | ---: | ---: | ---: |
| Debug + ID | 412.57 | 287.18 | 401.80 | 280.13 |
| Debug, no ID | 301.15 | 198.75 | 300.26 | 197.61 |
| Stripped + ID | 105.26 | 94.96 | 101.87 | 93.37 |
| Stripped, no ID | 77.45 | 72.27 | 76.11 | 70.50 |

The final candidate's ELF bytes match the baseline on all 14 workloads.
Every tool passes each workload's execution check; repeated outputs are
deterministic within each tool. Cross-linker ELF byte identity is not required.
The reports preserve all 21 samples, ranges, exact commands and RSS samples.
The SHA-1-only repeat also retains the no-ID control: timing can vary even
when that path is not executed, which is why the instruction comparison matters.

`dune runtest --profile release -j 2 src/linker/test` passes: 43 integration
groups, 269 Python test methods, and the unit checks. The optional musl gates
run against the retained 1.2.5 sysroot after relocating only its compiler-wrapper
prefixes and loader symlink; the relocation is recorded. glibc/musl static PIE,
TLS descriptors, static/dynamic IFUNC, symbol versions, partial linking, archives,
reentry/concurrent invocations and failure-preserving publication remain covered.
This is a local regression run, not a new multi-platform release qualification.

New regressions exercise shuffled relocation byte identity and overlap rejection,
multiple RELA tables targeting one section, unsigned high-bit relocation types,
and truncated archive offset arrays before index reservation. Existing SHA-1
vectors cover empty input, complete blocks, both padding boundaries, high-bit
data and one million `a` bytes. The previous GNU-observed 18-file SQLite
object/library/script closure was rehashed and is unchanged; it does not prove
that every linker traverses exactly the same files.

A bounded AFL++ smoke run passes 45 seconds each for ELF parsing, archive
parsing and full linking: **83,311 executions, zero saved crashes or hangs**.
The instrumented binary, seeds, queues, source snapshot, report and acceptance
verification are retained. This supplements the existing longer release
campaigns; it is not a replacement for them or a source-coverage claim.

## Hardware counters and remaining work

Separate seven/nine-repeat counter runs are outside the ordinary timing block.
Baseline versus the final candidate on SQLite debug/no-ID executes about
2.239B → 1.283B instructions, approximately **43% fewer**. With ID it is
3.570B → 2.457B, approximately **31% fewer**. Debug/no-ID allocation is
23.159M → 20.495M OCaml words, approximately **12% less**; minor collections
fall from 83 to 73. Major collections remain substantial.

Archive presizing alone reduces the sparse 256-member case from about
62.7M to 49.9M instructions, approximately 20%. The dense case improves only
about 3% at that stage. The bulk SHA-1 change removes about 158M instructions
from SQLite with ID while leaving the no-ID instruction count essentially
unchanged. The empty-IFUNC guard removes about 30M further instructions and
1.24M allocated words on that workload. Counters, phase timings and allocation
are different measurements; none is a whole-link speedup percentage.

Fas still executes roughly 11–12x the native references' instructions in
SQLite debug/no-ID. That is evidence of remaining work, not a measurement of
the fraction attributable to OCaml. The next architecture experiments should
target relocation/symbol representation, collector pressure and repeated hot
lookups, with profiles of this candidate guiding the order. Compact IDs,
flat arrays or retained validated input views are available design choices;
they are not implemented or assumed faster here. Keep the same diagnostics,
resource limits, reentry/isolation and output publication guarantees.

A fresh debug/no-ID profile of 20 links records no lost samples. The relocation
comparator is about 1.3% of sampled self cycles, compared with about 4.9% in
the whole-array merge-sort profile. Marking, sweeping, promotion/allocation,
generic comparison/hashing and relocation emission are prominent in the new
profile. These percentages use different complete-link denominators and are
not isolated operation speedups. The next representation experiment must
measure full-link gains rather than merely moving work between these categories.

The `gc` phase in stats is section reachability, not the OCaml collector.
`emit` includes relocation/unwind and `resolve` can include member parsing;
phase CPU/allocation totals must not be summed. Allocation profiles are not
CPU-time shares. Neither a language port nor parallel work is needed to explain
the algorithmic gain above.

## Evidence and reproduction

Compact reports, commands, counters, tests and source snapshots are under
[results/2026-11-10/serial-optimization](../results/2026-11-10/serial-optimization/).
Full inputs, output/tool snapshots and binary profiles remain under
`/home/glonex/.cache/fas-linker-qualification/serial-optimization-20261110`;
the copied artifact index records their hashes. Input/tool snapshots require
the retained external paths and system library closure; this is not a portable
sysroot bundle. SQLite is the retained 3.54.1 CLI, with runtime SQL result 42.
The manifest also retains the synthetic 1/32/256-object/lazy/dense cases and
native Fas static PIE. Stripping and disabling IDs are explicit diagnostic
variants, not new defaults.

Use [replay.py](../replay.py) with the copied manifest, per-tool arguments above,
`--runs 21 --warmups 3 --output /new/path`, and the recorded binary snapshots.
It records exact commands, SHA-256 identities, runtime checks, repeated output
determinism, CPU samples and separate RSS measurements. Compilation, runtime
checks and GNU-time RSS samples are outside timed links. All staged reports,
including the rejected decoder, are retained rather than selecting only wins.
