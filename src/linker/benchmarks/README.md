# Linker performance evidence

## IFUNC reference allocation checkpoint

The next untimed profile showed static IFUNC stub lookup allocating canonical
definition keys and option closures for ordinary relocations. The planner now
records trampoline indices once per symbol; emission uses integer array lookups.
The table is allocated only when a static link has live IFUNCs. Removing the
option closure on the empty path also reduces dynamic/static-PIE allocation.

Relative to the SHA-1 checkpoint, the static Chibicc link allocates 814,438 fewer
words (7.6%). The other three replay workloads drop by 0.5–1.7%. All four output
images remain byte-identical and pass their runtime checks. A GNU/LLD differential
fixture also verifies a weak IFUNC overridden by an ordinary strong function,
a weak declaration resolving to an IFUNC, and missing weak references, with GC
and partial links. These are allocation results, without new speed or peak-RSS
claims. [Pinned evidence](results/2026-11-05/allocation-ifunc-references.json)
includes tool/source hashes and the focused IFUNC test log.

## Build-ID allocation checkpoint

Untimed OCaml 5.4 Memprof sampling found SHA-1 schedule/state boxing accounted
for approximately 35%/39% of allocated-word samples in the two dynamic Chibicc
links below. The implementation now uses masked immediate integer words on the
supported 64-bit host. Exact `--stats` counters fall from 4,197,750 to 2,726,996
words (35%) for stage 1 and 4,702,235 to 2,838,387 (40%) for stage 2. Both output
images are byte-identical. The static Chibicc/cJSON replay recipes omit build IDs
and remain byte-identical with unchanged allocation counts. These observations
do not measure elapsed-time gains or change the LLD comparison below.

Validation includes 717 deterministic binary inputs compared with Python's
`hashlib.sha1`, padding-boundary/high-byte unit vectors, image-level build-ID
checks and the full release suite with isolated musl: 243 Python cases across
41 groups, OCaml checks and 3,792 compiler regression checks, no skips.
[Allocation evidence](results/2026-11-05/allocation-sha1.json) pins binaries,
source overrides, the replay manifest and raw local profile hashes. Full samples
and commands remain under `/tmp/fas-allocation-profile-20261105`.

The optional [allocation sampler](allocation_profile.ml) reports estimated
allocated words by call stack, not CPU time or retained memory. Build it outside
the source tree (OCaml 5.4 used for this checkpoint), then pass ordinary linker
arguments with a temporary output path:

```sh
dune build --profile release src/linker/fas_linker.cmxa
mkdir -p /tmp/fas-allocation-sampler
cp src/linker/benchmarks/allocation_profile.ml /tmp/fas-allocation-sampler/
ocamlopt -g -I +unix -I _build/default/src/linker \
  -I _build/default/src/linker/.fas_linker.objs/byte \
  -I _build/default/src/linker/.fas_linker.objs/native \
  unix.cmxa _build/default/src/linker/fas_linker.cmxa \
  /tmp/fas-allocation-sampler/allocation_profile.ml \
  -o /tmp/fas-allocation-sampler/profile
/tmp/fas-allocation-sampler/profile -o /tmp/profile-program input.o
```

## Real-program and archive baseline, 2026-11-05

With the user leaving the PC idle, recorded 21 samples per tool/workload after
three warm-ups, rotating tool order each round. Wall time includes spawn/exit;
CPU uses wait4. Peak RSS comes from a separate GNU-time run. Inputs are warm;
no cache eviction, affinity/governor change or worker override was used. Fas has
one worker; GNU ld 2.46 and LLD 22.1.8 use defaults. Mold is not installed.
The CPU is the same Ryzen 5 2600, with the `schedutil` governor.

The first optimization reuses bytes for repeated positional inputs within each
invocation. Parsing/member state and logical file/byte budgets remain separate.
Every before/after real-program output is byte-identical and passes its runtime
check. Chibicc checks `--help`; cJSON runs its upstream parse-examples test from
the fixture directory. The corpus reports separately establish wider behavior.

| Link workload | Fas before | Fas read cache | GNU ld | LLD | Fas peak RSS before → after |
| --- | ---: | ---: | ---: | ---: | ---: |
| Chibicc stage 1, dynamic | 45.50 ms | 39.44 ms | 18.19 ms | 20.88 ms | 28,536 → 19,284 KiB |
| Chibicc stage 2, COPY imports | 46.40 ms | 35.17 ms | 17.81 ms | 21.04 ms | 27,504 → 18,996 KiB |
| Chibicc, static glibc | 96.59 ms | 95.53 ms | 79.93 ms | 34.08 ms | 39,488 → 39,512 KiB |
| cJSON, static PIE | 85.60 ms | 82.43 ms | 54.20 ms | 35.54 ms | 40,164 → 38,400 KiB |

Chibicc's two dynamic links show approximately 13%/24% lower median wall times,
23%/21% fewer allocated words and 32%/31% lower sampled peak RSS. Static Chibicc
has no repeated inputs and shows no meaningful gain. The static-PIE case has one
cache hit. Raw ranges/samples matter: these are warm-cache measurements on one
machine, without a general speedup or 2x-LLD claim. Fas remains slower than LLD on
these real workloads. Packed symbols/relocations, archive-index costs and parallel
work remain performance work.

Before that change, synthetic runs with the same 21/3 method give:

| Corpus | Linked object count | Fas | GNU ld | LLD |
| --- | ---: | ---: | ---: | ---: |
| Separate objects | 257 | 59.20 ms | 47.22 ms | 25.77 ms |
| Separate objects | 769 | 184.20 ms | 145.92 ms | 51.30 ms |
| Archive, every member needed | 257 | 75.13 ms | 38.66 ms | 24.24 ms |
| Archive, every member needed | 1,025 | 329.92 ms | 160.01 ms | 54.56 ms |
| Archive, one member needed | 2 (1,024-member archive) | 53.58 ms | 12.90 ms | 33.04 ms |

The last archive contains 1,024 generated members plus one explicit startup
object; two objects are linked. Counts describe members rather than physical
input files. These runs precede the cache change; each supplies every physical
file once, so no read-reuse gain is expected. They reveal archive/index and
large-input costs that need work beyond repeated IO.

Raw reports, manifests, tool/input/output hashes, runtime checks and allocation
counts are under `results/2026-11-05/`. Full artifacts remain in the reports'
listed `/tmp` paths. Sources and toolchain closures are pinned by the associated
corpus reports; the replay harness hashes explicit argv files and does not claim
to discover every implicit `-l`/script dependency itself.

Replay a manifest from a preserved corpus with a new artifact directory:

```sh
python3 src/linker/benchmarks/replay.py /path/to/manifest.json \
  --output /tmp/fas-link-replay-new \
  --tool fas-ld=/path/to/fas-ld --tool GNU=/usr/bin/ld \
  --tool LLD=/usr/bin/ld.lld-22 --runs 21 --warmups 3
```

`--verify-only` links/checks each workload without collecting timings. Workloads
declare `name`, link `cwd`, an `args` list containing `-o PATH`, and a `check`
with optional `args`, `cwd`, `stdin` and expected `exit`. The runner snapshots
tools/harness/manifest, retains outputs, checks repeated-output determinism and
rejects changed explicit input bytes. Each corpus needs its own correct runtime
check; this does not replace compatibility tests.

## Initial static linking baseline

Recorded 2026-11-05 on an AMD Ryzen 5 2600, with warm inputs on tmpfs. The Fas
linker was built with `dune build --profile release linker/bin/main.exe` before
the source migration. Commands below use the current `src/linker/` layout;
historical JSON files retain their original paths and hashes.
One warm-up and seven measured runs per tool/corpus; tool order rotates each
round. Each corpus includes one startup object. No section GC, ICF, or build ID
was requested; each linker uses its default worker count. Every executable is
checked by execution outside timed runs.

| Input objects | Input bytes | Relocations | fas-ld median | GNU ld median | LLD median |
| --- | ---: | ---: | ---: | ---: | ---: |
| 2 | 6,056 | 65 | 2.52 ms | 3.07 ms | 15.87 ms |
| 33 | 168,912 | 2,049 | 13.11 ms | 10.41 ms | 19.12 ms |
| 257 | 1,359,824 | 16,385 | 55.21 ms | 55.17 ms | 33.89 ms |

GNU ld is Ubuntu Binutils 2.46; LLD is Ubuntu 22.1.8. Mold was not installed.
Wall time includes spawning the linker and waiting for its exit. CPU time is
collected with `wait4`; peak RSS uses a separate untimed GNU time invocation.
In the largest case those peak-RSS measurements were 16,636 KiB for fas-ld,
23,060 KiB for GNU ld, and 66,916 KiB for LLD.

Timing spread is material: fas-ld's largest case ranges from 45.96 to 69.64 ms.
The [raw report](initial.json) contains every sample, output sizes, environment
metadata, ordered input hashes, and executable hashes. Outputs have different
layouts and sizes. This small synthetic corpus does not establish general
performance parity or ecosystem compatibility. Cold-cache, archive-heavy,
debug-heavy, and parallel-scaling measurements remain outstanding.

Reproduce from the `fas` directory:

```sh
dune build --profile release src/linker/bin/main.exe
python3 src/linker/bench.py _build/default/src/linker/bin/main.exe --output /tmp/fas-linker-bench.json
```

## v0.2 archive and allocation checkpoint

Same machine/methodology, with separate object, all-members-needed archive and
one-member-needed archive corpora. Raw pre-optimization reports:
[objects](v0.2-objects.json), [all members](v0.2-archives-all.json),
[lazy](v0.2-archives-lazy.json). Follow-up reports:
[objects](v0.2-objects-optimized.json),
[all members](v0.2-archives-all-optimized.json),
[lazy](v0.2-archives-lazy-optimized.json). Matching corpus hashes were checked.
The original baseline above remains unchanged.

| 257-object corpus | fas-ld before | fas-ld after | GNU ld after | LLD after | Allocated before → after |
| --- | ---: | ---: | ---: | ---: | ---: |
| Separate objects | 61.36 ms | 41.99 ms | 43.98 ms | 24.79 ms | 58.22 → 22.71 MiB |
| Archive, all 256 members used | 66.93 ms | 62.69 ms | 33.60 ms | 22.76 ms | 66.35 → 28.29 MiB |
| Archive, one member used | 13.34 ms | 12.75 ms | 4.13 ms | 18.78 ms | 4.44 → 4.29 MiB |

The changes defer successful-path diagnostic formatting, avoid repeated context
string construction in checked reader fields, and skip relocation sorting/copying
when input relocations are already ordered. Allocation reductions are clearer
than the noisy timing differences: the small object cases were slightly slower
in the follow-up. No general speedup or 2x-LLD result is established. The lazy
case still parses/allocates the archive index but parses only one ELF member.

Allocation/phase counters come from an extra untimed `--stats` run. Allocation
bytes are allocated OCaml words × 8 on this host, not peak live memory. Phases
are inclusive (`resolve` includes extracted-member parsing, `emit` includes
relocation), so their totals must not be summed. Collection counts are sampled;
the code uses current domain counters for allocations without forcing GC.

```sh
python3 src/linker/bench.py _build/default/src/linker/bin/main.exe --statistics
python3 src/linker/bench.py _build/default/src/linker/bin/main.exe --archive all --statistics
python3 src/linker/bench.py _build/default/src/linker/bin/main.exe --archive lazy --statistics
```
