# fas-ld in Fas: design

The OCaml linker (`src/linker`, about 8,900 lines) was built to be correct:
one thread, OCaml values for every section and symbol, files read into
strings, the output built in memory and written once. It is the
specification. This one is built to be fast on a modern many-core CPU from
the first line, and it must still produce **the same bytes**.

## Contract

- **Same command line, same output bytes, same diagnostics and exit
  codes** as `src/linker/bin/main.exe`. The integration suite
  (`src/linker/test/*_integration.py`) already takes the linker binary as
  an argument; it runs unchanged against this one. On top of that a
  differential harness links every test input with both linkers and
  compares the output bytes.
- **The output never depends on the thread count.** Every parallel step
  either writes each item's result to that item's own place or merges in a
  fixed order. Tested by linking with 1, 2, 3 and N threads and comparing
  bytes.
- **Same limits as the OCaml linker** until scale tests justify raising
  them (they are work limits, and their rejections are part of the
  contract).
- Built freestanding from `lib/` (no libc), so it starts in microseconds
  and owns its memory.

## Pipeline

Passes run one after another; each pass is a `pool_for` over a flat array
(files, sections, symbols or relocations). That is mold's structure, and
it is the one that scales: no locks on hot paths, no task graph, and a
pass's cost is visible in `--stats`.

| Pass | Parallel over | Notes |
| --- | --- | --- |
| open inputs | files | `open` + `fstat` + `mmap` read-only; nothing is copied. Archive symbol indexes parsed here. |
| parse objects | objects | section headers, symbol tables and relocation headers decoded into per-object arrays that point into the mapping |
| intern names | symbols | name hashes computed in parallel; names go into a sharded table (shard = hash bits, one shard per worker in turn), so no locks |
| resolve + archive extraction | (serial decisions, parallel parsing) | GNU left-to-right archive semantics decide *which* members are extracted and in what order; that decision loop works on interned ids and is cheap. Each round of newly extracted members is parsed in parallel. |
| COMDAT, merge, GC | groups, sections | group winners by file order; GC marks with atomic flags; merged strings deduplicated per shard with first-occurrence order kept by (file, section, offset) keys |
| scan relocations | sections | GOT/PLT/TLS/copy needs set with atomic OR on per-symbol flag words |
| layout | output sections | sizes and offsets by prefix sums; serial only over output sections (a few dozen) |
| write | input sections | output file sized with `fallocate`, mapped, and every input section copied and relocated straight into the map by whichever worker claims it; synthetic sections (symtab, strtab, GOT, dynamic tables) built in parallel |
| build-id | — | must match the OCaml output, so it is the same MD5 / SHA-1 over the same bytes; SHA-1 uses SHA-NI where the CPU has it |
| publish | — | `fchmod`, `rename`, exit without freeing anything |

## Data layout

- **Struct of arrays with `u32` indices**, never per-symbol heap objects:
  an object is a row in `files`, its sections are a range in `sections`,
  its symbols a range in `symbols`. Hot loops touch only the columns they
  need.
- **Input bytes are never copied** until they are written into the output
  map. Names are `Bytes` into the mapping.
- **One arena per worker**, freed by process exit. The heap's lock is
  only for rare, large allocations.
- **Atomics only where workers meet**: symbol flag words, GC marks, shard
  claims, chunk counters.

## Compared with mold and reld

| | mold | reld (wild fork) | fas-ld |
| --- | --- | --- | --- |
| Pass structure | serial passes of parallel-for | same | same |
| Archive semantics | parallel liveness from roots (not GNU order) | GNU-compatible | GNU left-to-right, byte-identical to the OCaml linker |
| Symbol table | concurrent hash map, CAS on owner | sharded maps | sharded by hash, per-worker shards, deterministic merge |
| Output | mmap, parallel copy+relocate | mmap and buffered, byte-identical | mmap, parallel copy+relocate |
| Determinism | commutative ops / canonical sort | byte identity is its test oracle | byte identity with the OCaml linker and across thread counts |
| Build-id | BLAKE3 tree, parallel | — | MD5/SHA-1 like the OCaml linker (serial, SHA-NI) |
| Runtime | C++, mimalloc, TBB | Rust, rayon | Fas, no libc, own pool and arenas |
| Early exit | forks: parent exits when output is ready | — | later, once everything else is fast |

The deliberate differences: GNU archive order (so the output matches the
OCaml linker and GNU ld's semantics), and build-id hashes that match the
oracle instead of a parallel tree hash.

## Stages

1. **Threads in `lib/`** (done: `lib/thread.fas`).
2. **Skeleton:** command line, inputs, objects-only static executables;
   byte-identical on the simple integration tests.
3. **Static links in full:** archives, groups, COMDAT, GC, merge sections,
   `.eh_frame`, TLS, static IFUNC, build-id, map files, compressed debug.
   This is what `fas` itself uses by default; the driver can then opt in
   (`FAS_LD`).
4. **Static PIE and `-r`.**
5. **Dynamic executables, shared libraries, symbol versions, version
   scripts, copy relocations.**
6. **Default:** the driver calls this linker; the OCaml one stays as the
   oracle until v1.0.

Each stage ends with the integration suite for its features passing
against this binary, the differential harness clean, and a benchmark
against the OCaml linker, GNU ld and lld (mold and reld when installed).
