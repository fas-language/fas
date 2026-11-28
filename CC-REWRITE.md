# fcc rewrite handbook

This is the working guide for rewriting the C compiler (fcc) step by step,
with speed designed in from the first line. It covers where to work, how to
build, which tests check what, how speed is measured, and how to review each
step.

## 1. Where to work and what to commit to

| What | Value |
| --- | --- |
| Worktree | `/home/glonex/fas-experimental-backend/cc-rewrite` |
| Branch | `cc-rewrite` (local, created from `backend-v2` at `4f691e4`) |
| Push to | `origin cc-rewrite` when you want a backup: `git push -u origin cc-rewrite` |
| Never push to | `main`; `backend-v2` only after a verified merge (section 8) |

The other worktrees in `/home/glonex/fas-experimental-backend` belong to
other work: `fas-backend`, `fas-quick-wt`, `wt-ld-script`, and
`wt-cc-inline`. Leave them alone.

**Suggested layout.** Write the new compiler in a new directory, `cc2/`,
next to the current `cc/`. The old fcc then stays usable for three things:
- an extra oracle beside gcc and clang (diff `-E` output, diagnostics, and
  so on);
- the kernel and linker tests, which keep building with it;
- a parts bin. Copy a module over only after you have read it and accepted
  it.

Share `cc/include/` (fcc's own `stddef.h`, `stdarg.h`, …) through
`FCC_INCLUDE` until `cc2` needs its own copy. When `cc2` passes everything,
it replaces `cc/` in one commit.

**Commits.**
- One reviewed step per commit.
- Stage explicit paths (`git add cc2/lex.fas`), never `git add -A`.
- Don't commit these untracked files: `interop.bin`, `test/interop.bin`,
  `editors/zed/{extension.wasm,grammars,target}`, and `fas_refuse_crashes/`.
- Never rewrite history that you have pushed.

## 2. Building

One-time and after any change to the OCaml compiler (`src/`):

```sh
cd /home/glonex/fas-experimental-backend/cc-rewrite
dune build -j 3
```

`_build/default/bin/main.exe` is the Fas compiler (written in OCaml). Every
Fas program is built with it.

**Native backend** (fast to build, slow code; use while iterating):

```sh
_build/default/bin/main.exe cc2/main.fas -o _build/fcc2
```

**LLVM -O2** (the build every speed number must come from):

```sh
FAS_ORACLE=llvm FAS_CC=clang-22 FAS_OPT=opt-22 FAS_LLC=llc-22 \
  _build/default/bin/main.exe --backend=llvm -O2 cc2/main.fas -o _build/fcc2-llvm
```

- `--backend=llvm -O2` lowers Fas to LLVM IR and optimizes it with
  `opt-22`/`llc-22`.
- `FAS_ORACLE=llvm` is a hidden switch that allows the LLVM pipeline. Set
  it on that one command only, never `export` it. Releases and the test
  tiers use the native backend unless they say otherwise.
- `FAS_CC=clang-22` is the C compiler and linker driver the LLVM path uses.

Using the compiler:

```sh
export FCC_INCLUDE=$PWD/cc/include   # fcc's own resource headers
_build/fcc2-llvm -E -P input.c
_build/fcc2-llvm -fsyntax-only input.c
_build/fcc2-llvm -c input.c -o input.o
_build/fcc2-llvm input.c -o prog
```

How the current fcc produces machine code, and what `cc2` can reuse:
- It writes Fir, our IR. Fir text is specified in `src/fir/SPEC.md`.
- It runs `FCC_FIR` (default `_build/default/tools/fas_fir/fas_fir.exe`)
  to turn Fir into an object file.
- It links with `FCC_LD` (default `_build/fas-ld`; build it with
  `_build/default/bin/main.exe ld/main.fas -o _build/fas-ld`).

Look at `cc/gen_driver.fas` for the exact calls. A new front end can emit
the same Fir and get the whole backend and linker for free.
`ARCHITECTURE.md` §4.2 plans a binary Fir builder API to replace the text
hand-off later.

Fas language reference: `LANGUAGE.md`. Standard library: `lib/` (see
`lib/README.md`) and `std/`. Known Fas compiler gaps: `../Fas bug reports.md`.
Add any new one you hit there.

## 3. Machine rules (this PC)

- `/tmp` is a 7.8 GB RAM disk. Start every shell that runs tests with
  `ulimit -f 524288`. The test scripts already do this.
- Run heavy jobs (tiers, kernel builds, speed runs) through the shared lock,
  capped so the desktop stays responsive:

  ```sh
  heavy() {
    flock /home/glonex/fas-experimental-backend/.heavy.lock \
      systemd-run --user --scope -q -p MemoryHigh=6G -p MemoryMax=8G \
      nice -n 10 ionice -c2 -n7 "$@"
  }
  heavy env TIER_JOBS=3 bash test/tier.sh quick
  ```

- Use at most 3 parallel test jobs (`TIER_JOBS=3`, `--jobs 3`); never more
  than 6.
- Delete large trial build directories when you're done with them. The disk
  has about 60 GB free.

## 4. Tests: what checks what

Every script picks the compiler under test in this order:
1. `FCC=/path/to/binary`, if set (prebuilt; fastest for iterating);
2. otherwise `FCC_MAIN=cc2/main.fas`, which the script builds;
3. otherwise the default `cc/main.fas`.

The corpora live outside the repo in `../cc-tests` (12 GB, already
downloaded; `cc-tests/fetch.sh` restores them). Cached gcc/clang reference
outputs live in `cc-tests/refcache`, so only fcc is rerun.

### By rewrite stage

Each stage has its own suite, so you can test a stage as soon as it exists:

| Stage you have | Script | What it compares |
| --- | --- | --- |
| Lexer + preprocessor (`-E`) | `bash test/cc_pp.sh --jobs 3` | `-E` token spellings and line markers vs clang-22 over the C corpora; known differences in `test/cc_pp/expected_diffs.txt` |
| Parser + semantics (`-fsyntax-only`) | `bash test/cc_parse.sh` | accept/reject decisions vs clang-22 on `test/cc_parse/*.c` (the `reject_*.c` cases must fail) plus the frozen corpus; known differences in `test/cc_parse/expected_diffs.txt` |
| Code generation (`-c`, link, run) | `bash test/cc_run.sh --manifest --strict --jobs 3` | builds and runs about 3,986 test programs (c-testsuite, gcc torture, …) and compares output and exit code with clang-22; current baseline 3,977 pass; known failures in `test/cc_run/expected_failures.txt` |
| | `bash test/cc_run.sh --regressions` | directed regression programs in `test/cc_run/regressions/` |
| | `bash test/cc_run.sh --flags` | driver flag handling |
| | `bash test/cc_run.sh --stream` | streamed Fir output |
| | `bash test/cc_run.sh --random --count 100 --jobs 3` | random UB-free C programs vs clang |
| | `bash test/cc_run.sh --csmith --jobs 3` | Csmith differential |
| | `bash test/cc_run.sh --abi` | Microsoft x64 ABI interop |
| Real projects | `bash test/cc_run.sh --projects --jobs 3` | builds 28 projects (sqlite, lua, zlib, redis, curl, … plus postgresql, cpython, openssl, perl, git) and runs their own test suites |
| | `bash test/cc_debug.sh` | DWARF debug info |
| | `bash test/cc_i386.sh` | `-m32`/`-m16` cross compilation |
| | `bash test/cc_kernel.sh fcc defconfig` | Linux 6.12 defconfig built by fcc boots in QEMU (long; see `test/cc_kernel/README.md`) |

`test/cc_types.sh` builds white-box probes from the old fcc's own modules
(`cc/test/*.fas`). It doesn't apply to `cc2`, so `tier.sh` skips it when
`FCC_MAIN` is set to anything else.

### Tiers

```sh
FCC_MAIN=cc2/main.fas TIER_JOBS=3 bash test/tier.sh quick   # ~6 min
FCC_MAIN=cc2/main.fas TIER_JOBS=3 bash test/tier.sh push    # ~13-15 min
```

- **Quick:** dune build and test, the strict manifest, regressions, flags,
  stream, random C, and cc_types.
- **Push:** quick plus 23 projects.
- **Milestone:** adds the 5 large projects, a native-built fcc, the kernel
  boot, Csmith, and the fuzzers.

Each run prints its log path under `_build/tiers/`. The tiers build fcc
with LLVM -O2 themselves.

**Until `cc2` can generate code,** run only the stage scripts above. The
tiers need a full compiler.

## 5. Speed

```sh
bash test/cc_speed.sh                                  # current fcc
FCC_MAIN=cc2/main.fas bash test/cc_speed.sh            # your rewrite
SPEED_BASELINE=1 FCC_MAIN=cc2/main.fas bash test/cc_speed.sh   # rewrite and old fcc side by side
SPEED_MODE=-E bash test/cc_speed.sh                    # preprocessor only
SPEED_PERF=1 ... bash test/cc_speed.sh                 # plus perf top symbols
```

- It builds fcc with LLVM -O2 and pins every run to one core (CPU 2).
- It takes the median of 3 runs and the peak RSS.
- It compares against gcc and clang-22 with `-std=gnu23 -O0` on
  `sqlite3.c` and `duktape.c`.
- It writes a JSON report to `_build/speed/<stamp>/report.json`.
- `vs_best_cc` is (faster of gcc and clang) ÷ tool. Above 1 means faster
  than both.

Budgets in `ARCHITECTURE.md` §2 (you may reset them; they should be your
numbers):

| Stage | Budget |
| --- | --- |
| Front end (`-fsyntax-only`) | ≥ 5× faster than the faster of gcc and clang |
| Front-end peak memory | ≤ ½ of gcc's |

The data-layout rules there (§3) come from profiling the current fcc:
- integer token kinds instead of string compares;
- interning;
- binding-stack scopes;
- small AST nodes with side tables;
- arenas.

### First measurement, current fcc (4f691e4), `-fsyntax-only`

| Input | fcc | gcc | clang-22 | fcc vs best | fcc peak RSS | gcc peak RSS |
| --- | --- | --- | --- | --- | --- | --- |
| sqlite3.c | 0.63 s | 0.55 s | 1.16 s | 0.87× | 180 MB | 91 MB |
| duktape.c | 0.40 s | 0.35 s | 0.73 s | 0.87× | 116 MB | 68 MB |

So the current fcc is a little slower than gcc and about 2× faster than
clang. It uses twice gcc's memory. The budget needs about 6× more speed.
perf's top symbols on sqlite3.c:

| Share | Symbol | Where it comes from |
| --- | --- | --- |
| 18.5% | `pa_is` | the parser comparing token text against strings |
| 5.2% | `lex_next` | the lexer |
| 5.1% | `map_get` on byte strings | hash lookups |
| 4.9% | `pa_peek_at` | parser lookahead |
| 7.4% | `expand_next` + `expand_raw` | macro expansion |
| ~5% | kernel page faults and page zeroing | memory growth |

## 6. Reviewing each step (you + the model council)

Correctness is decided by the oracles, not by any model's opinion. A step
is acceptable only when all of these hold:

1. **Tests.** The stage scripts for every stage `cc2` has reached pass
   with no new entries in `expected_failures.txt` or `expected_diffs.txt`.
   Any change to those files is explained line by line in the commit
   message.
2. **Speed.** A `cc_speed.sh` report is attached for every change to the
   lexer, preprocessor, parser, or semantics. A step that makes a number
   clearly worse says why.
3. **Size.** The step does one thing. As a rule of thumb, the diff is
   small enough to read in one sitting (a few hundred lines).

Cheats to look for, because models do these under pressure:
- special-casing test file names, test macros, or known inputs;
- calling `gcc`/`clang` (or the old `cc/` binary) from inside the compiler
  to produce an answer;
- deleting, skipping, or weakening tests, timeouts, or `--strict`;
- adding entries to expected-failure lists to turn red into green;
- `TODO`/fallback paths that silently accept input the compiler doesn't
  understand;
- copy-pasted near-duplicate functions instead of one shared one;
- speed claims without a `cc_speed.sh` report from the LLVM -O2 build.

## 7. Reading list

- `ARCHITECTURE.md`: speed method, budgets, and data-layout rules.
- `cc/DESIGN.md`: the current fcc's structure (preprocessor, parser,
  driver).
- `src/fir/SPEC.md`: Fir, the IR a front end emits.
- `LANGUAGE.md`: the Fas language.
- `test/TIERS.md`: the tiers.

## 8. Merging back

When `cc2` replaces `cc` (or at any milestone you want to keep):
1. Push tier green with `FCC_MAIN=cc2/main.fas`, and the speed report
   attached.
2. Merge `backend-v2` into `cc-rewrite` first if it has moved, and rerun.
3. Fast-forward `backend-v2` to the result: `git push origin
   cc-rewrite:backend-v2` (only if `origin/backend-v2` is an ancestor).
