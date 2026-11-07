# Small single-thread investigation — 2026-11-07

The first implementation experiment should target **unsorted relocation
validation**, followed by archive-index capacity and build-ID hashing. Member
views and canonical symbol IDs remain candidates, but this probe does not
justify making either the first change. Parallel parsing and other parallel
passes are deferred. **No production linker code changed in this investigation.**

## Scope and measurement

The executable is the delivered fas-ld 1.0.0, source
`3cf2962665405578b7a32a35f25e810e2ff188d5`, binary SHA-256
`895d36ae96b63751965608032d0667b7a2f438f50c9e50a48a0b66f1e31b0af1`.
A local build of the same runtime sources was included as a control; its ELF
outputs match the delivered executable byte for byte on all 11 workloads.
Local probes link against the release-profile library using OCaml 5.4.1.
Controls are Ubuntu LLD 22.1.8 with **`--threads=1`**, and GNU ld 2.46.
Neither reference repository was built or benchmarked.

This is a **shared-PC, exploratory warm-cache investigation**, on a Ryzen 5
2600. The initial load average was 6.24, and the timing blocks show visible
drift. These figures are not a quiet-machine performance acceptance baseline
or a general ranking. CPU time also varies with frequency and cache contention.
No affinity, governor, suspend or cache policy changed.

The replay runner uses 11 samples/tool/workload and two warm-ups, rotating tool
order. SQLite interventions use nine samples and two warm-ups. Compilation,
execution checks, `--stats`, allocation sampling, hardware counters and RSS
measurement are outside the ordinary timed runs. Wall time includes spawn
through exit; CPU is user plus system time from `wait4`. Output paths are reused,
including each linker's own replacement/publication behavior. Full commands,
samples, versions, hashes and checks are retained in the
[baseline report](../results/2026-11-07/serial-probe/baseline/report.json).

Synthetic inputs follow `bench.py`: 64 functions and 64 absolute relocations
per member, archives with 1/32/256 members, either one or every member needed,
plus separate-object cases. The largest archive is approximately 1.6 MB.
Real inputs are the retained upstream SQLite 3.54.1 CLI objects (debug enabled)
and a small native Fas static-PIE fixture from the release compatibility run.
The latter is a startup/control case, not an application scaling benchmark.
SQLite's two large input objects contain 51,642 and 249,110 relocations.

## End-to-end observations

Selected first-pass medians, **CPU milliseconds**:

| Workload | Fas release | LLD, one worker | GNU ld |
| --- | ---: | ---: | ---: |
| One generated object plus startup | 3.56 | 23.11 | 3.82 |
| 32 objects plus startup | 15.38 | 22.14 | 9.84 |
| 256 objects plus startup | 104.03 | 45.01 | 82.54 |
| Archive, one of 256 members selected | 18.60 | 27.63 | 4.29 |
| Archive, all 256 members selected | 111.19 | 33.01 | 57.51 |
| SQLite CLI, debug and SHA-1 build ID | 491.50 | 59.84 | 106.38 |
| Small native Fas static PIE | 3.31 | 19.56 | 3.78 |

Tiny links have a substantial process-initialization component. A separate
`--version` control measured 2.00/13.68/1.67 ms CPU for Fas/LLD/GNU. This is
not a link and must not be subtracted to construct adjusted link timings.
It helps explain why tiny-case rankings cannot predict scaling.

Separate five-repeat `perf stat` runs on SQLite record approximately **3.57
billion instructions for Fas versus 211 million for LLD**, and 1.98 billion
versus 205 million cycles. That supports investigating executed work, rather
than attributing the gap solely to desktop scheduling. It does not measure
how much faster equivalent native code would be, or assign a percentage of
the gap to OCaml. All counter samples are retained, including cache misses.

An untimed SQLite stats run allocates 23,159,218 OCaml words, approximately
176.7 MiB cumulatively; its separate peak-RSS sample is 96,364 KiB. Cumulative
allocation is not live memory or RSS. Its inclusive CPU spans include parse
223.5 ms, emit 256.8 ms, relocate 20.8 ms and write 8.2 ms. Do not sum them:
`emit` contains relocation and `resolve` can contain archive-member parsing.
The phase named `gc` is **section reachability**, not the OCaml heap collector.

## Isolated checks

### Relocation sorting

Cycle sampling highlights the relocation comparator and array-sort helpers.
The comparator itself accounts for about 9% of the SQLite sampled user cycles.
Inspection shows the reader already avoids sorting ordered relocations, but
both large SQLite objects require the fallback used for overlap validation.

The [microprobe](serial_micro.ml) compares `Array.sort` with
`Array.stable_sort`, using the same comparator and pre-parsed relocation arrays.
Each operation includes copying the source array. It rotates alternatives
within seven samples, calibrates batch sizes and measures CPU/allocation after
a full collection outside each timed batch. I/O and initial parsing are excluded.
It verifies equal sorted keys and unchanged original arrays; this does not
replace malformed-input or end-to-end regression tests for an eventual change.

| Relocations | Current heap sort | Merge sort | Allocated words, heap → merge |
| --- | ---: | ---: | ---: |
| 51,642 | 15.49 ms | 7.31 ms | 258,233 → 241,316 |
| 249,110 | 91.85 ms | 41.40 ms | 1,245,583 → 1,029,042 |

This is approximately 2.2x for the larger **sorting operation**, not the complete
link. Keep the already-sorted fast path and all overlap checks. The first
implementation candidate is a fallback-only change, measured against 1.0 with
byte-identity and ordered/unsorted/overlapping-relocation regressions.

### Archives

The sparse archive stats confirm one extracted member, 65 linked relocations
and 717,733 allocated words. Most parse time is container/index preparation.
Its CPU profile is dominated by generic hash lookup/comparison, table growth
and heap collection; merely making ELF-member decoding faster misses that work.

In the rotated microprobe, parsing the already-resident 256-member container
and index takes **7.56 ms**, while copying every member's bytes takes **0.56
ms**. The latter allocates 170,645 words. These independent operations are
not a decomposition of an entire CLI link; they show why removing member
copies alone is a weak first target for these small members.

Rebuilding just the validated, pre-decoded 16,384-entry name index gives:

| Table | Initial capacity request | CPU ms | Allocated words |
| --- | ---: | ---: | ---: |
| Generic | 256 | 4.35 | 146,770 |
| Generic | Entry count | 2.57 | 131,083 |
| String-specialized, seeded | 256 | 3.59 | 146,770 |
| String-specialized, seeded | Entry count | 2.40 | 131,083 |

All variants preserve random seeding and produce equal logical tables. This
probe excludes name decoding, index validation and per-member export lists.
The sized generic table is the simpler first candidate; the additional
specialization gain needs stronger evidence. A production capacity change must
validate claimed counts/ranges before allocation and preserve resource budgets.
No GNU archive-order, weak-symbol or lazy-member policy should change.

### Debug information and build IDs

The SQLite self profile attributes approximately 25% of sampled user cycles
to the SHA-1 digest/byte loops. An intervention copies the input objects and
uses `objcopy --strip-debug`, and independently overrides `--build-id` with
`--build-id=none`. Every output still passes the SQL execution check.

CPU medians from this separate block:

| SQLite inputs/options | Fas | LLD, one worker | GNU ld |
| --- | ---: | ---: | ---: |
| Debug, SHA-1 ID | 428.01 ms | 49.11 ms | 81.79 ms |
| Debug, no ID | 322.44 ms | 47.09 ms | 74.16 ms |
| Stripped inputs, SHA-1 ID | 105.66 ms | 30.09 ms | 50.72 ms |
| Stripped inputs, no ID | 88.07 ms | 29.10 ms | 47.22 ms |

The repeated original case is faster than the first pass, illustrating why
these blocks must not be mixed for precise speedup claims. Stripping changes
the workload, relocation count and output size; suppressing the build ID changes
requested output. These are diagnostic interventions, **not proposed defaults**
or improvements to claim for the linker. Preserve debug data and ID semantics.
The existing SHA-1 allocation improvements are already in 1.0; the remaining
hashing CPU cost warrants a separate serial experiment.

## Next work and evidence

1. Try only the unsorted-relocation fallback change. Check byte-identical valid
   outputs, diagnostics/overlap rejection and performance across tiny, synthetic
   and real inputs. Then run the relevant existing regressions.
2. Test bounded archive-index presizing independently. Keep its parser checks
   and extraction semantics, and compare sparse/dense archives and real libc
   archives. Do not extrapolate index-only savings to whole-link percentages.
3. Profile a serial SHA-1 implementation experiment with digest identity at
   block boundaries and on existing outputs. Keep the requested build-ID style.
4. Reassess name lookups, canonical IDs and compact relocation storage using the
   remaining profiles. Plan a quiet-PC before/after baseline once there is a
   candidate; repeat full compatibility acceptance before an optimized release.

There is no basis here for promising 2x over LLD or for blaming a fixed portion
of the gap on OCaml. There is evidence for specific serial work to remove.

## Reproduction and evidence

[Compact evidence](../results/2026-11-07/serial-probe/) includes every CLI sample,
stats, hardware counters, flat CPU reports, top allocation stacks, microprobe
samples, tool identities and the observed GNU SQLite library/script closure.
That closure is GNU resolution evidence; the replay tool still does not infer
every linker's implicit dependencies. The original corpus reports supply
source/build provenance. Full input/tool snapshots and binary CPU profiles are
retained under
`/home/glonex/.cache/fas-linker-qualification/serial-probe-20261107`;
[artifact-index.json](../results/2026-11-07/serial-probe/artifact-index.json)
records their hashes. Artifacts require those retained paths; they are not a
portable sysroot bundle.

Prepare another retained small corpus:

```sh
python3 src/linker/benchmarks/research/prepare_serial_probe.py \
  --output /path/to/new-inputs \
  --sqlite-build /path/to/qualified/sqlite/build \
  --fas-fixtures /path/to/backend-v2-compatibility-final
python3 src/linker/benchmarks/replay.py /path/to/new-inputs/manifest.json \
  --output /path/to/new-replay --runs 11 --warmups 2 \
  --tool Fas-release=/path/to/released/fas-ld \
  --tool LLD=/usr/bin/ld.lld-22 --tool-arg LLD=--threads=1 \
  --tool GNU=/usr/bin/ld
```

The optional real-fixture arguments may be omitted. Build `serial_micro.ml`
against `fas_linker.cmxa` using the include paths in the retained
[build command](../results/2026-11-07/serial-probe/micro/build-command.json),
then pass an archive and ordinary objects. It needs OCaml 5.4's seeded-table
interface. CPU-profile commands and the short repeat driver are also retained;
allocation sampling uses the existing `allocation_profile.ml`. Profilers affect
execution and their wall times are not ordinary benchmark samples.

Validation here consists of 56 linked-executable behavior checks, per-tool
repeated-byte determinism, all 11 delivered/local Fas image comparisons, and
the microprobe assertions. Production sources are unchanged; this research is
not a new production qualification run.
