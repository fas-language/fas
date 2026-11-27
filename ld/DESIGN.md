# fas-ld in Fas: design

The OCaml linker (`src/linker`, about 8,900 lines) is the specification:
single-threaded, byte-exact, with strict work limits and exact diagnostics.
`fas-ld` (`ld/*.fas`) is a freestanding Fas implementation (no libc, built on
`lib/`) designed for many-core CPUs and in-process embedding while remaining
**byte-identical** to the OCaml linker across all thread counts for their
shared modes. Executable linker scripts use ld.lld as their reference.

## Contract

- **Same command line, same output bytes, same diagnostics and exit codes**
  as `_build/default/src/linker/bin/main.exe`. Verified by `test/fas_ld.sh`,
  which runs every OCaml integration test in `src/linker/test/dune` through
  a shadow wrapper (`test/fas_ld/shadow_ld`) and runs a seeded dynamic/shared
  differential fuzz pass (`test/fas_ld/ldfuzz_dyn.py`).
- **Thread-count independence:** `FAS_LD_THREADS` sets the worker pool size
  (default: one per CPU). Parallel passes write into per-item slots and merge
  or report errors in deterministic input order.
- **Same limits and diagnostics** as the OCaml linker.
- **Freestanding:** no libc; uses `lib/` syscalls, mmap, and futex-based
  thread pool (`lib/thread.fas`).

## Module Map

| File | Responsibility |
| --- | --- |
| `ld/main.fas` | CLI entry point, pool lifecycle, dispatch to `link_and_publish` or `partial_link_and_publish`. |
| `ld/api.fas` | In-process library API (`ld_link`), `LdVfs` virtual filesystem & in-memory output, `ld_reset` re-entrancy cleanup, and incremental early-parse session (`ld_session_begin`, `ld_add_input`, `ld_finish`). |
| `ld/util.fas` | Diagnostics (`msg`/`die`/`refuse`), library-mode non-local exit (`fas_ld_setjmp`/`fas_ld_longjmp`), checked arithmetic, LE readers/writers, `KeyIndex`. |
| `ld/cli.fas` | Command-line and `@response` file parsing (`parse_command_line_argv`, `Config`). |
| `ld/elf.fas` | Parallel-safe x86-64 and i386 relocatable object parser. ELF64 also supports SDT notes and zlib-compressed debug. |
| `ld/shared.fas` | Parallel-safe ELF64 x86-64 `ET_DYN` shared-object reader (`shared_is`, `shared_parse`, SysV/GNU hash & verdef/verneed/versym validation, `sh_find_location`). |
| `ld/archive.fas` | SysV/GNU and thin archive reader (including nested thin members), `-l` library search, lazy GNU extraction (`extract`). |
| `ld/script.fas` | Bounded linker script lexer/parser (`INPUT`, `GROUP`, `AS_NEEDED`, `SEARCH_DIR`, `OUTPUT_FORMAT`) and `--sysroot` normalization. |
| `ld/lscript.fas` | Executable linker-script parser, input section selection, script symbols, and convergent address assignment. |
| `ld/limage.fas` | Scripted ELF file offsets, PHDRs, section fills, symbol table, and retained RELA (ELF64) or REL (ELF32) records. |
| `ld/shdeps.fas` | `$ORIGIN`/`${ORIGIN}` expansion, transitive `DT_NEEDED` loading (`-rpath-link`, `DT_RUNPATH`, inherited `DT_RPATH`), and link-time validation. |
| `ld/resolve.fas` | Input staging, parallel `.o`/`.so` parsing with input-order diagnostics, global symbol resolution, COMDAT, commons, predefined symbols, `shdeps_load`. |
| `ld/verscript.fas` | Version script parser and glob/exact visibility policy. |
| `ld/symver.fas` | `.symver` expansion, versioned aliases, and `shared_output_prepare`. |
| `ld/tls.fas` | TLS layout validation, `_TLS_MODULE_BASE_`, thread-offset computation. |
| `ld/unwind.fas` | `.eh_frame` normalization and `.eh_frame_hdr` generation. |
| `ld/gc.fas` | Parallel edge construction and worklist `--gc-sections`. |
| `ld/copy.fas` | Executable `R_X86_64_COPY` relocation planning (`<COPY storage>`). |
| `ld/relax.fas` | Static-PIE `GOTPCRELX` relaxation. |
| `ld/property.fas` | `.note.gnu.property` merging and validation. |
| `ld/buildid.fas` | Deterministic SHA-1 / MD5 GNU build-id generation. |
| `ld/ifunc.fas` | Static `R_X86_64_IRELATIVE` / `.rela.iplt` generation. |
| `ld/dynamic.fas` | `.interp`, `.dynstr`, `.dynsym`, `.hash`/`.gnu.hash`, `.rela.dyn`, `.plt`, `.dynamic`, `.gnu.version`/`_r`/`_d`, lifecycle arrays. |
| `ld/layout.fas` | Segment & section layout, RELRO/TLS segments, alignment and offsets. |
| `ld/link.fas` | GOT preparation, `SHF_MERGE` deduplication, parallel symbol-value / blit / relocation passes, ELF emission and atomic publication. |
| `ld/partial.fas` | `-r` relocatable output (`partial_link_and_publish`). |
| `ld/map.fas` | `-Map` deterministic section/symbol map output. |
| `ld/stats.fas` | `--stats` JSON timing/allocation/counter reporting with parse-failure rollback. |
| `ld/zlib.fas` | Inflate decoder for `SHF_COMPRESSED` debug sections. |

## Pipeline & Threading

Passes execute serially on the main thread; data-parallel passes fan out over
`pool_for(pool, n, grain, fn, ctx)`:

1. **Read & stage** (`resolve.fas`): Inputs are read in command-line order
   (consulting `LdVfs` before the filesystem). Relocatable `.o` and `.so`
   inputs are staged with a monotonic input sequence number `seq`.
2. **Parallel parse** (`parse_flush`): Staged `.o` (`elf_parse`) and `.so`
   (`shared_parse`) files are parsed in parallel on `pool`. Workers write
   errors into per-input `msg` buffers without printing; the main thread
   walks results in `seq` order so the first error and `--stats` snapshot
   match the serial OCaml linker.
3. **Resolve** (`process_inputs` + `resolve`): Serial GNU left-to-right walk
   over objects, archives (with parallel lazy member parsing), shared
   libraries (`--as-needed`, group fixed points), COMDAT, commons, predefined
   symbols, and transitive `DT_NEEDED` loading (`shdeps_load`).
4. **Emit passes** (`build_image`):
   - `tls_prepare` -> `symver_plan` -> `shared_output_prepare` -> `shdeps_validate`
   - `unwind_normalize` -> `gc_sections` (parallel edge build) -> `copy_prepare`
   - `check_non_alloc_references` -> `tls_validate` -> `relax_static_pie`
   - `properties_prepare` -> `build_id_prepare` -> `got_prepare` -> `unwind_prepare`
   - `ifunc_prepare` (static) or `dyn_prepare` (static-PIE / dynamic / shared)
   - `merge_plan` -> `layout_make` -> `set_predefined`
   - Parallel `values_job`, parallel `blit_job`, parallel `relocate_job`
   - `unwind_emit`, `ifunc_emit`, `dyn_emit`, symtab/headers, `build_id_emit`
5. **Publish**: Either captured in memory (`LdVfs.in_memory`) or atomically
   written to disk (`file_write_atomic`).

## Executable linker scripts

`-T PATH` and `--script=PATH` select the static x86-64 or i386 script path.
Select i386 with `-m elf_i386`, `OUTPUT_FORMAT("elf32-i386")`, or
`OUTPUT_ARCH(i386)`. Repeated `-m` options select the last emulation and
must agree with the script when it specifies the target.
`ENTRY` sets the entry symbol, with `-e` taking precedence. `-q` or
`--emit-relocs` retains relocation records against output symbols and sections.
`--discard-none` retains local symbols. `--orphan-handling=error` reports
unassigned input sections as link errors. By default, ELF32 places unassigned
non-allocated debug and comment sections at address zero. In BIOS scripts
with consecutive `.text32` and `.text64` areas, an executable `.inittext`
orphan goes after the code-end assignments and before the data alignment.
`-S` or `--strip-debug` removes non-allocated debug sections and their
symbols and relocations before scripted layout. Other unsupported orphan
placement returns exit 3.

The parser recognizes the x86-64 kernel's `OUTPUT_FORMAT`, `OUTPUT_ARCH`,
`PHDRS`, and `SECTIONS` directives. It supports `PT_LOAD` and `PT_NOTE`,
section addresses, `AT`, `ALIGN`, `(INFO)`, fixed fill patterns, `LONG`, and
`/DISCARD/`. Input descriptions use the `*` file selector with section
patterns, optionally wrapped in `KEEP` and `SORT`. Integer expressions can
reference symbols, the location counter, section addresses and sizes.
Assignments and referenced `PROVIDE` definitions participate in symbol
resolution. Address assignment iterates until sections and script symbols
stabilize, then checks assertions and applies relocations using the final
addresses. The symbol table preserves each script symbol's output section
and type where the expression carries them.

The script path supports Linux defconfig's final link and static i386
executables. ELF32 supports `R_386_32`, `R_386_PC32`, `R_386_PLT32`, `R_386_16`,
`R_386_PC16`, `R_386_8`, and `R_386_PC8`. Unsupported
directives and combinations return exit 3. These include `MEMORY` and
`OVERLAY`, named input-file patterns, `COMMON`,
TLS, IFUNC, unrelaxed GOT relocations, and script mode combined with shared
output, maps, or `-r`. Static executables support `--gc-sections`. The
collector roots `KEEP` inputs and definitions referenced by script
expressions. Archives listed under `--whole-archive`
are supported. An ELF64 script can produce `-pie --no-dynamic-linker` output
when it discards dynamic metadata and every allocated relocation can be
resolved without a dynamic loader. Relaxation of GOT references to
section-relative script symbols uses their final layout. Lazy extraction in this mode requires
script symbol assignments and references to be disjoint from the archive
index. The supported explicit metadata layout
places `.symtab`, `.strtab` and `.shstrtab` in order after content. The
linker uses its existing whole-image build-ID algorithm and omits ld.lld's
synthetic `.comment` section.

`bash test/fas_ld_script.sh` checks the supported layout, script symbols,
retained relocations, entry override, thread determinism and diagnostics.
`KERNEL_LD=fas bash test/cc_kernel.sh fcc defconfig` runs the scripted
final-link path and boots the result.

## In-Process Library API & VFS (`ld/api.fas`)

For embedding `fas-ld` in a single-process toolchain (C/Fas compiler + backend
+ linker without touching disk):

- **`LdVfs`**: Maps virtual paths to caller-owned `Bytes` (`vfs_add`,
  `vfs_has`, `vfs_get`, `vfs_clear_output`, `vfs_free`). Every file read,
  existence probe (`-l`, linker scripts, `DT_NEEDED`), and `realpath` check
  consults the VFS before falling back to the host filesystem. Setting
  `vfs.in_memory = true` returns the linked image in `(vfs.out_data, vfs.out_size)`
  (and optional `-Map` output in `vfs.map_data/map_size`) without writing disk.
- **`ld_link(argv addr, argc u64, vfs handle[LdVfs]) i32`**: Runs a complete
  link using `argv[0..argc)` and `vfs`. Diagnostics (`die`/`refuse`) and
  `--help`/`--version` propagate through Fas `fails` functions. The entry point
  maps the termination reason to exit code `0`, `1`, or `3`, with diagnostic
  text captured in `vfs.diag`. Worker jobs still report through per-item
  state, so the main thread chooses the first diagnostic after the join.
- **Re-entrancy (`ld_reset`)**: Every module-level `var` across `ld/*.fas`
  is freed and reset before and after each `ld_link` call so repeated links
  in one process produce identical bytes and do not leak per-link tables.
- **Incremental early-parse session (`LdSession`)**:
  - `ld_session_begin(s, vfs)` initializes a session.
  - `ld_add_input(s, name, data)` registers `(name, data)` in `vfs` and
    eagerly parses relocatable ELF objects (`elf_parse`) as soon as they arrive
    from upstream compiler threads.
  - `ld_finish(s, argv, argc)` runs `ld_link`; `read_input` consumes the
    pre-parsed `Obj` records (`ld_take_preparsed`) in `argv` order without
    re-parsing, guaranteeing byte-identical output and identical error order.
