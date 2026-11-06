# Coverage-guided linker fuzzing

From the `fas` directory, with OCaml, Dune, AFL++, Python 3 and GNU as/ar/GCC:

```sh
python3 src/linker/fuzz/run.py --output /tmp/fas-linker-fuzz --seconds 180
```

The destination must be new. It preserves the instrumented target, source
snapshot, generated seeds, AFL logs/queues/crashes/hangs and `report.json`.
Hosted uploads wrap this directory in `linker-fuzz.tar.gz` to retain AFL's colon
filenames; the report is also uploaded separately. The optional two-second
manual selection checks artifact delivery only and does not qualify a release.
The duration applies to each of fourteen sequential targets; `--modes shared link`
selects a subset. One worker runs at reduced scheduling priority. This is a
correctness campaign, not a timing benchmark. Reproduce a finding with:

```sh
/tmp/fas-linker-fuzz/target.exe shared /path/to/finding
```

The `fuzz` Dune profile instruments the linker library and harness using OCaml's
native `-afl-instrument`. The harness catches only `Fas_linker.Error.Error`.
`AFL_CRASH_EXITCODE=2` makes uncaught OCaml exceptions findings as well as native
signals. Objects, DSOs, archive containers/members and implicit script grammar
have separate campaigns. Scripts are parsed without opening named files.
The `link` and `shared-link` targets exercise static/DSO emission with and without
section GC from in-memory objects. The `version` target parses policy, classifies
sample names and emits a DSO's version metadata. The `symver-link` target supplies
named version nodes while mutating object aliases. The `zlib` target exercises
bounded debug-section decompression independently. No target executes emitted
programs or fuzzes the system loader. A campaign exits unsuccessfully when AFL
saves findings; inspect the preserved report even when the runner exits nonzero.
The `metadata-link` target exercises GNU/both hash styles, SHA-1/MD5 build IDs,
version aliases and GC. Thin archive containers, including GNU's 15-character
member-reference form, have seeds, but parser fuzzing
does not open external members; deterministic integration tests cover that IO.
The `partial-link` target exercises ET_REL combination, symbol/relocation remaps,
groups and metadata without executing emitted code, with and without an
undeclared forced undefined root. Valid-program runtime behavior is checked
separately by the [semantic generator](../semantic/README.md).
The `dependency` target parses DSO requirements and validates an in-memory
dependency graph; it never searches for or opens paths supplied by mutations.
The `static-pie-link` target exercises interpreter-free ET_DYN emission with and
without GC, including bootstrap GOTPCRELX rewrites, relative pointers and IFUNC
inputs. It does not execute emitted images or exercise a libc self-relocator.
The `copy-link` target mutates DSO metadata, builds bounded in-memory direct data
references, and exercises COPY/GOT/dynamic planning for fixed and PIE output. It
does not publish or execute images. Seeds include aligned BSS, read-only/RELRO
data, pointer initializers and distinct symbol versions. At most 32 objects of
1 MiB each are selected, and the DSO export count is capped at 65,536.
Seeds also include personality/LSDA unwind metadata, static IFUNC references,
SystemTap notes, repeated GNU properties and GNU/LLD multi-provider version tables.

Reuse a previous campaign's saved queues with a newly built target:

```sh
python3 src/linker/fuzz/run.py --output /tmp/fas-fuzz-followup --modes elf link \
  --import-corpus /tmp/fas-linker-fuzz --seconds 180
```

The old campaign remains intact. Selected modes import nonempty queue inputs up
to the harness's 1 MiB limit, deduplicate them by SHA-256 and preserve their hashes
in the new report. `--import-corpus` can be repeated to combine saved campaigns.

Each input is bounded to 1 MiB. The link target additionally limits sections to
256, individual section/common sizes to 1 MiB. These restrictions reduce fuzzer
resource abuse; large production limits need separate tests. OCaml requires
`-m none` because its virtual address reservations exceed traditional AFL limits.
Every case has a two-second timeout. No system CPU/core-dump settings are changed.
The runner permits the host's external core collector; signal crashes can be
delayed or classified as hangs on such hosts, so both need triage. Uncaught OCaml
exceptions are captured through exit status independently of the core collector.

Report execution counts, elapsed time, discovered edges, corpus size and every
finding. AFL bitmap occupancy is not source/branch coverage. Short campaigns
cannot substitute for long-running CI fuzzing, semantic differential testing,
failure injection or large-program compatibility tests.

The [semantic checkpoint](../corpus/results/2026-11-06-semantic/README.md)
records a repaired symbol-version finding and a later untriaged timeout.
The [thin-archive follow-up](../corpus/results/2026-11-06-thin/README.md) triages
that timeout and a new archive timeout as not reproducible, with retained inputs
and separate 600-second follow-ups. Their causes remain unknown; keep both on
the watchlist. Original failing and interrupted campaigns retain their identities.
Bounded local follow-ups do not complete sustained or hosted qualification.

References: [OCaml fuzzing manual](https://ocaml.org/manual/5.4/afl-fuzz.html),
[AFL++ environment variables](https://aflplus.plus/docs/env_variables/).
