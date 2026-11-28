# Toolchain architecture contract

This file fixes the architectural choices that are expensive to change once
150k+ lines depend on them: data layout, memory, and the boundaries between
stages. Every component (fcc, the self-hosted Fas front end, the backend,
fas-as, fas-ld) follows it. Micro-optimization is out of scope here and
still waits for the optimizing backend.

The contract is proven on fcc first, because fcc has the largest real-world
corpus and gcc and clang to compare against. The self-hosted Fas front end
is then written once, to the proven contract.

## 1. How speed is measured before our optimizer exists

Our native backend does not optimize yet, so tool binaries built with it say
nothing about the design. Until the optimizing backend beats it, LLVM -O2 is
the stand-in optimizer for measurement:

```sh
FAS_ORACLE=llvm FAS_CC=clang-22 FAS_OPT=opt-22 FAS_LLC=llc-22 \
  _build/default/bin/main.exe --backend=llvm -O2 cc/main.fas -o fcc-llvm
```

`FAS_ORACLE=llvm` is set for that one command only. A design choice counts
as proven when the LLVM -O2 build meets the budgets below. Releases and
tests keep using the native backend.

Benchmarks run single-threaded, pinned to one core, median of 3 runs, with
`-std=gnu23 -O0`. Peak memory is the maximum resident set size from
`/usr/bin/time -f %M`. The reference corpus is the amalgamations sqlite3.c,
duktape.c and lua (onelua), plus the 28 projects in `test/cc_run`.

## 2. Budgets

| Stage | Budget | Today (fcc-llvm, sqlite3.c) |
|---|---|---|
| Front end (preprocess, parse, sema): `-fsyntax-only` | ≥ 5× faster than the faster of gcc and clang | 0.49 s vs gcc 0.46 s (1×) |
| Front end peak memory | ≤ ½ of gcc's peak RSS | 184 MB vs gcc 84 MB (2.2×) |
| Backend, optimizing | ≥ 5× faster than LLVM -O2 at matching code quality (optimizer design: optimizer-lab) | not built yet |
| Backend, non-optimizing | ≥ 1M Fir instructions/s per thread | ~330k/s (OCaml) |

Concretely, the front-end budget means sqlite3.c in ≤ 0.09 s, duktape.c in
≤ 0.065 s and onelua in ≤ 0.034 s on this machine. The same budgets apply to
the Fas front end, measured against fcc on comparable code size.

Each budget is checked by a script in `test/` that writes a JSON report.
The report is tracked over time and is not a pass/fail gate, because timing
is noisy. A change that makes a budget clearly worse needs a reason in its
commit message.

## 3. Data layout

These rules come from the profile of fcc-llvm on sqlite3.c. 12.6% of the
time is in `pa_is`, which compares token text with C strings. 6.2% is in
hash-map lookups. About 8% is in kernel page faults and page zeroing.

1. **Tokens carry an integer kind for every keyword and punctuator.** The
   lexer classifies once, and the parser switches on `kind` or tests a
   keyword ID. No stage compares spellings with string compares after
   lexing. Spellings are kept only for diagnostics, stringizing and pasting.
   The keyword set depends on `-std` and GNU mode, and is resolved once
   per translation unit into a table indexed by interned ID.
2. **Every name is interned once into a 32-bit ID.** Later stages index
   arrays by ID instead of hashing strings. Per-name state lives in a
   parallel array indexed by ID. Today that already holds the macro slot,
   and it also holds the innermost ordinary and tag bindings.
3. **Scopes are binding stacks, not hash maps.** Declaring a name pushes a
   binding onto its ID's slot and records the ID in the scope's undo list.
   Leaving the scope pops those bindings. Lookup is one array index. This
   replaces the per-scope `Map[u32, addr]` tables.
4. **Hot nodes are small and rare data lives in side tables.** An AST or
   IR node holds its kind, location, type ID and child links, in at most 32
   bytes. Floating bits, wide integer limbs, attributes, bit-field details
   and similar rare payloads go in side tables keyed by node index. Today
   `Ast` is about 150 bytes.
5. **References are 32-bit indices into arenas, not 64-bit pointers.** This
   applies to types, nodes, symbols, locations and spellings. Indices halve
   the memory of link-heavy structures and make dumps and caches
   position-independent.
6. **Memory comes from arenas sized to the work.** Each translation unit
   and each function has its own region. Memory is allocated in large
   chunks and released as a whole, never node by node. Function-level
   regions are reused for the next function, so pages are faulted in once.
   File buffers are mapped read-only, not copied.
7. **No per-token or per-node heap allocation, and no quadratic paths.**
   Anything proportional to input size must be linear. This includes
   include-guard detection, macro expansion with hide sets, initializer
   lists and switch tables. A pathological input gets a test in the
   corpus.

## 4. Stage boundaries

1. **The pipeline streams at function granularity.** The front end hands a
   function to the backend as soon as its body is checked. The backend
   optimizes and emits it while the front end parses the next one.
   Declarations are processed in a pre-pass, so order-independent languages
   (Fas) still stream.
2. **The front end and backend talk through a binary, in-process Fir
   builder.** It is one API, used by fcc and the Fas front end alike. Calls
   append instructions to the current function's arena. A finished function
   is handed over as a pointer and does not go through text. Fir text
   becomes a dump and debug format, produced by a consumer of the same API
   (`-save-temps`, `--emit-ir`). The text parser stays for tests and the
   OCaml oracle. Today fcc prints Fir text and `fas-fir` parses it again;
   that disappears once the backend is in Fas.
3. **The backend emits machine code directly into object-file sections.**
   Assembly text exists only for `-S` and for diffing against fas-as.
   fas-as stays the assembler for user `.s` and inline `asm`.
4. **The linker can take objects in memory.** When the toolchain builds an
   executable in one process, finished sections go straight to fas-ld's
   input structures without writing `.o` files. Separate `.o` files remain
   fully supported.
5. **One worker pool runs every stage.** Workers have no fixed front-end
   or backend role. The pool starts on the main thread alone and wakes
   workers only once a file is large or a second import appears. Each
   worker has a work-stealing deque: it runs its own newest work first,
   so an IR slab it just filled is compiled while still in its cache, and
   idle workers steal the oldest work. Workers spin briefly before
   sleeping. Work kinds, highest priority first:
   - **Load and scan.** Read a file into the source arena with `read`
     (not `mmap` for small files), followed by 64 zero bytes so the lexer
     needs no end check. Scan only the file's head for imports (the
     language requires file imports at the top) and queue unseen files.
   - **C header import.** One job per (header, macro state), shared by
     every file that uses it. Large headers never stall other work.
   - **Backend.** Compile one IR slab into thread-local code chunks, then
     recycle the slab.
   - **Front end.** Parse and check, large files before small ones.
6. **Fas checking runs in two phases with one barrier.** All files in a
   `use` closure share one namespace and imports may be cyclic, so
   waiting on a file's imports is not enough. Phase A collects every
   file's top-level declarations in parallel. Phase B, after the barrier,
   checks and lowers function bodies fully in parallel. Types and
   constants resolve on demand, each with a state (not started, in
   progress, done) that also detects cycles, since constants can call
   functions while compiling. Generic instances are deduplicated in one
   shared table keyed by the generic and its type arguments; the first
   worker to ask compiles the instance.
7. **Slabs bound memory.** The front end packs several small functions
   into one IR slab of about 32–64 KB before handing it over; a huge
   function is handed over alone. A worker that finds no free slab
   compiles a queued slab itself before continuing, so memory stays
   bounded and nothing deadlocks.
8. **Output is deterministic.** Code chunks, diagnostics and generic
   instances are ordered by a stable key (file index, function index,
   generic key), never by which thread finished first.
9. **The link is parallel.** Symbols are resolved first. A prefix sum
   over the sorted chunks assigns final addresses, then all workers copy
   chunks and patch relocations into the output buffer in parallel.
   `.eh_frame`, DWARF and the build-ID hash are built per chunk as well.
   External `.o` and `.a` inputs enter the same structures.
10. **-O2 inlining is still open.** A callee may not be compiled yet when
    its caller is. The likely shape: stream at -O0 and -O1, and at -O2
    keep small inline candidates in a summary table or schedule by call
    graph. The optimizer design (optimizer-lab) decides.
11. **Stage-local data never leaks across the boundary.** The backend
   receives Fir plus the type and symbol tables it references. It never
   sees AST nodes. That keeps C-specific front-end code from affecting the
   backend, and the reverse.

## 5. Order of work

1. Add a front-end speed and memory report script, `test/cc_speed.sh`,
   built on the bench recipe above. It also records perf's top symbols.
2. Bring fcc to the data layout in §3, one rule at a time, as follows.
   The full test suite (cc_run, projects, Csmith, random C, cc_types) must
   pass after every step.
   - token kinds and keyword IDs (rule 1);
   - binding-stack scopes (rule 3);
   - small AST nodes with side tables (rule 4);
   - arena and region sizing (rule 6).
3. Define the binary Fir builder API (§4.2). Back fcc's emitter with it,
   with text as one consumer.
4. Write the self-hosted Fas front end to this contract, built with LLVM
   -O2 while our optimizer does not exist yet.
5. Write the Fas backend in streaming form: first the fast non-optimizing
   path, then the optimizer passes designed in optimizer-lab. The OCaml
   backend is the oracle until the new one matches it.

Changing a rule in this file needs a measurement that shows why, run with
the method in §1.
