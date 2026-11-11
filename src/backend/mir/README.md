# Machine IR backend (roadmap v0.4)

The native x86-64 backend: it reads Fir directly and goes through a small
machine IR. It replaced the old Ir-based backend (`native_gen.ml` and its
passes, removed). For now the goal is **correctness only**: the
instruction selection is simple, the allocator is simple, and an
independent checker verifies every allocation. Optimizations come later
and must not change this pipeline's structure.

```
Fir --Fir_scalarize--> Fir (no vectors)
    --Fir_wide--> Fir (integers <= 64 bits)
    --Mir_isel--> Mir (virtual registers)
    --Mir_ra----> allocation --Mir_check--> ok
    --Mir_emit--> X64 encoder, .eh_frame (Mir_cfi), -g: .debug_* (Mir_dwarf),
                  ELF via Native_object
```

| File | Role |
|---|---|
| `../../fir/fir_scalarize.ml` | Fir → Fir without vectors (see its header). A vector is a set of scalar lanes; one indexed at a run-time lane, or built by `insert`, lives in a fresh stack slot instead, so code size stays linear in the lanes. Vector parameters become their lanes, and vector results come back through hidden pointer parameters. This is the old backend's convention, not the SysV vector ABI. |
| `mir.ml` | The machine IR: x86-64 instructions over virtual (`V n`) and physical (`G r`, `X i`) registers, frame slots, blocks; `uses_defs`, `clobbers`, `map_regs`, printers. |
| `mir_isel.ml` | Fir → Mir. One Fir block is one Mir block with the same number; extra blocks (edges, loops, `va_arg` branches) are appended. SysV calls, parameters, returns and `va_*` live here. Two choices keep the code from being absurdly slow without any optimization pass: a static alloca's address is folded into memory operands (`[rbp-n]`) instead of occupying a register, and a local only ever loaded and stored whole, by one scalar type, lives in a virtual register (`promotable_allocas`; Mir needs no SSA, so this is just a table lookup). |
| `mir_ra.ml` | Fixed-interval linear scan: one hull interval per virtual register (from per-register liveness), no splitting; spilled registers live in a slot and go through reserved scratch registers (see the header comment). |
| `mir_check.ml` | Verifies an allocation without trusting the allocator: an abstract machine over registers and slots, iterated to a fixpoint. |
| `../native/` | What stays of the old backend: the x86-64 encoder (`x64.ml`), ELF writing (`elf64.ml`, `native_object.ml`, `elf_attrs.ml`) and the Fas runtime sources (`native_runtime.ml`, `runtime/`). |
| `mir_runtime.ml` | Appends the correctly rounded `fma` helpers (`backend/native/runtime/fma.fas`) to a module that uses `fma`: baseline x86-64 has no FMA instruction. |
| `mir_emit.ml` | Frame layout, encoding, data sections, `_start`, the object. |
| `mir_driver.ml` | The driver's entry (`--backend=native`): compiles the Fir module the front end lowered. |
| `mir_cfi.ml` | The `.eh_frame`: one CIE, and one FDE per function from the frame events `Mir_emit.func` records (see "Unwind info"). |
| `mir_dwarf.ml` | `-g`: `.debug_abbrev`, `.debug_info` and `.debug_line` (DWARF 5) from the line rows `Mir_emit.func` records (see "Debug info"). |

## Running it

- `fas --backend=native ...`: every native build (objects, static and
  hosted executables, any `-O` level) goes through this backend via
  `mir_driver.ml` (Fas lowers straight to Fir). No optimization: the `-O`
  level changes nothing here. `bench/native/sieve*.fas` ran about 1.8x
  slower than the removed old backend at `-O2` (it had mem2reg, constant
  folding, inlining, LICM, unrolling); code built to be constant-folded,
  such as `pz3_sys.fas`, about 10x slower.
- `FAS_MIR_REGS=n` gives the allocator only n registers per class, to make
  small programs spill.
- `fas-fir file.fir`: compile and link through this backend; `-c` writes
  an object (`--mir` is still accepted and does nothing).
- `FAS_MIR_DUMP=1` prints the selected and the allocated Mir to stderr.
- `sh test/fas_fir.sh` (`FAS_FIR_TOOL` names the tool): the Fir
  conformance programs.
- `fir_fuzz.exe`: random modules compared with clang -O2.
- `sh test/native_determinism.sh`: compiles every Fas test program and
  Fir program twice (the second time from another directory, into another
  file name, with OCaml's hash tables randomized) and requires
  byte-identical objects and executables.
- `sh test/native_debug.sh`: `-g` leaves every test program's code alone,
  and the line tables decode to the expected file:line rows (checked with
  `llvm-dwarfdump --verify` and gdb too, when installed). CI runs it, the
  determinism test, the checker and the Fir programs
  (`.github/workflows/native-backend.yml`).
- `test/mir_check_test.ml` (in `dune runtest`): allocates every function of
  the Fir test corpus, checks it, then mutates each allocation (drop a
  reload, move an operand to another register) and requires the checker to
  catch at least 95% of the mutants.

## Frame

```
[rbp+16 ...]   incoming stack arguments (Bin)
[rbp+8]        return address
[rbp]          saved rbp
               saved callee-saved registers (pushes)
               frame slots (Bslot), each at most 16-aligned
               ...
[rsp ...]      outgoing arguments (Bout), a multiple of 16 bytes
```

Dynamic `alloca` (`Salloc`) lowers rsp by a multiple of 16 and returns
the block just above the outgoing-argument area, so `Bout` stays at rsp.
Alignments above 16 over-allocate and round the address up. The epilogue
restores rsp from rbp, so `stackrestore` and dynamic allocations need no
bookkeeping.

## Unwind info

Every frame has the shape above, so the CFI is fixed: after `push rbp` the
CFA is rsp+16 and rbp is saved at CFA-16; after `mov rbp, rsp` the CFA is
rbp+16 for the rest of the body, whatever rsp does (dynamic allocas); the
i-th callee-saved push saves its register at CFA-24-8i. Each return is
bracketed by `remember_state` before its epilogue and `restore_state`
after the `ret`, with CFA = rsp+8 after `pop rbp`. `_start` has an FDE with
an undefined return address, where unwinders stop. FDEs point at their code
with a PC32 relocation against the local symbol `.Lfas.text` (the start of
`.text`) plus the function's offset, so a weak function that is overridden
still has the FDE of its own copy. `test/fas_fir/c/unwind.fir` walks the
stack with the libgcc unwinder from a C callback.

## Debug info

With `-g` (`fas -g --backend=native`, `fas-fir -g`) the object gets DWARF 5
line tables, like the LLVM path's line-tables-only debug info. Fir
locations are always there (the Fas front end writes them;
`Fir_scalarize` and `Fir_wide` copy them to what they emit), so `-g` only
decides whether they are written out and can never change the code:
`test/native_debug.sh` checks that every test program's allocated
sections, relocations and symbols are the same with and without it.

`Mir_isel` puts a `Loc n` pseudo-instruction (no code) before each Fir
instruction or terminator whose location differs from the last one in the
block; `Mir_emit.func` records the code offset of each. A location whose
code is empty (a jump that falls through) is dropped in favour of the next
one at the same offset. `Mir_dwarf` then writes one compile unit (named
after the file of the lowest location id, the main source file), one
subprogram per function (frame base rbp) and one line-program sequence per
function: its first row is the function's first byte with the first
location's line, and the row of that location itself is marked
`prologue_end`, so a breakpoint on a function stops after its frame is
built. Addresses are R_X86_64_64 relocations against `.Lfas.text`; the
offsets of `.debug_abbrev` and `.debug_line` in `.debug_info` are
R_X86_64_32 relocations against local symbols at their starts, so linkers
can concatenate the sections of several objects. The `fma` runtime helpers
have no lines.

## Block addresses

Unlike the old backend, which lowered `indirect_br` to a `switch` over
small tokens, a `blockaddr` here is the block's real code address: each
taken block gets a local symbol `.Lfas.blk.<fn>.<n>`, read RIP-relative in
code and through an `R_X86_64_64` relocation in data. `indirect_br` is
`jmp r11`.

## Named sections and thread-local storage

A global with `section "s"` goes into its own section `s` (SHF_ALLOC, plus
SHF_WRITE unless every global in it is read-only; SHT_NOBITS for an
all-zero `.bss` or `.bss.*`). Globals in one section are laid out in module
order, so a registry walked between the linker's `__start_s` and
`__stop_s` sees them in that order.

`thread_local` globals go into `.tdata` (initialized) or `.tbss` (zero),
with SHF_TLS, and their symbols are STT_TLS (also undefined ones). The
address of a thread-local is the thread pointer plus the variable's
offset, which never touches the flags:

```
tp  = mov fs:[0]                   ; Tp: fs:0 holds the thread pointer itself
off = mov [rip + x@GOTTPOFF]       ; Tlsoff, pic code: objects, PIEs (initial-exec)
off = mov imm32 x@TPOFF32          ; Tlsoff, -no-pie executables (local-exec)
adr = lea [tp + off]
```

A native executable has no libc to create the TLS block, so `_start` does
it (`tls_setup` in `mir_emit.ml`): it finds PT_TLS through `__ehdr_start`,
mmaps the block plus a 64-byte TCB, copies the `.tdata` image, stores the
TCB self-pointer and sets fs with `arch_prctl(ARCH_SET_FS)`. Without a
PT_TLS segment it does nothing. `test/fas_fir/tls.fir` checks that path;
`test/fas_fir/c/tls.fir` checks initial-exec from several C threads.

## Executables

`fas --backend=native` and `fas-fir` link a static position-independent
executable by default (ET_DYN, linked at 0, no PT_INTERP), from pic code:
every symbol is reached RIP-relative or through the GOT, and data pointers
become R_X86_64_RELATIVE relocations. No dynamic loader runs, so `_start`
applies them itself before anything else (`relocate_self` in
`mir_emit.ml`): it finds `_DYNAMIC` and `__ehdr_start` RIP-relative (the
runtime address of `__ehdr_start` is the load bias), walks DT_RELA and
DT_RELASZ, and writes `bias + addend` at `bias + offset` for each entry.
Any other relocation kind, or a DT_REL/DT_RELR table, stops the program
with `ud2` rather than run it half-relocated. `fas -Wl,-no-pie` (or
`-Wl,-static`, or any other output-mode flag passed to the linker) and
`fas-fir --no-pie` give a fixed-address ET_EXEC from non-pic code instead. `test/fas_fir.sh`
runs every program both ways.

Hosted executables (the program uses libc, C imports, C sources or link
inputs) are linked by the Fas linker too: the C compiler is asked only for
its link plan (`-###`: CRT files, interpreter, search paths) and compiles
`.c` inputs; no LLVM tool runs. They are dynamic PIEs by default, like the
LLVM path's (`-Wl,-no-pie` and `-Wl,-static` as usual). Finding the host's
CRT and libraries without any C compiler is the linker's "compiler-
independent hosted SDK configuration" (`src/linker/ROADMAP.md`).

## Not supported yet

Each of these is reported as `native backend: ...`, never miscompiled:

- atomic vector loads and stores, and vectors passed through `...`;
- a `thread_local` global in a named section, the address of a
  `thread_local` in an initializer, and general-dynamic TLS (an object with
  thread-locals uses initial-exec, so it cannot go into a library loaded
  with `dlopen`);
- aggregates aligned above 16, or with fields other than integers ≤ 64 bits,
  pointers, f32 and f64;
- 4-byte data relocations;
- inline asm that puts bytes in another section, defines a global symbol,
  aligns to more than 16 bytes or refers to a label in another asm
  statement (each statement is assembled on its own section and spliced
  into `.text`, see the inline asm part of `mir_emit.ml`); `asm goto` and
  module-level asm.
