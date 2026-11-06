# Machine IR backend (roadmap v0.4)

A second native x86-64 backend that reads Fir directly through a small
machine IR. It replaces `backend/native/native_gen.ml` once it covers
everything that backend does. For now the goal is **correctness only**: the
instruction selection is simple, the allocator is simple, and an
independent checker verifies every allocation. Optimizations come later
and must not change this pipeline's structure.

```
Fir --Fir_scalarize--> Fir (no vectors)
    --Fir_wide--> Fir (integers <= 64 bits)
    --Mir_isel--> Mir (virtual registers)
    --Mir_ra----> allocation --Mir_check--> ok
    --Mir_emit--> X64 encoder, .eh_frame (Mir_cfi), ELF via Native_object
```

| File | Role |
|---|---|
| `../../fir/fir_scalarize.ml` | Fir → Fir without vectors (see its header). A vector is a set of scalar lanes; one indexed at a run-time lane, or built by `insert`, lives in a fresh stack slot instead, so code size stays linear in the lanes. Vector parameters become their lanes, and vector results come back through hidden pointer parameters. This is the old backend's convention, not the SysV vector ABI. |
| `mir.ml` | The machine IR: x86-64 instructions over virtual (`V n`) and physical (`G r`, `X i`) registers, frame slots, blocks; `uses_defs`, `clobbers`, `map_regs`, printers. |
| `mir_isel.ml` | Fir → Mir. One Fir block is one Mir block with the same number; extra blocks (edges, loops, `va_arg` branches) are appended. SysV calls, parameters, returns and `va_*` live here. Two choices keep the code from being absurdly slow without any optimization pass: a static alloca's address is folded into memory operands (`[rbp-n]`) instead of occupying a register, and a local only ever loaded and stored whole, by one scalar type, lives in a virtual register (`promotable_allocas`; Mir needs no SSA, so this is just a table lookup). |
| `mir_ra.ml` | Fixed-interval linear scan: one hull interval per virtual register (from per-register liveness), no splitting; spilled registers live in a slot and go through reserved scratch registers (see the header comment). |
| `mir_check.ml` | Verifies an allocation without trusting the allocator: an abstract machine over registers and slots, iterated to a fixpoint. |
| `mir_runtime.ml` | Appends the correctly rounded `fma` helpers (`backend/native/runtime/fma.fas`) to a module that uses `fma`: baseline x86-64 has no FMA instruction. |
| `mir_emit.ml` | Frame layout, encoding, data sections, `_start`, the object. |
| `mir_driver.ml` | The driver's entry (`--backend=native`): converts the old Ir to Fir and compiles it here (`FAS_NATIVE_LEGACY=1`: `Native_gen`, until it is removed). |
| `mir_cfi.ml` | The `.eh_frame`: one CIE, and one FDE per function from the frame events `Mir_emit.func` records (see "Unwind info"). |

## Running it

- `fas --backend=native ...`: every native build (objects, static and
  hosted executables, any `-O` level) goes through this backend via
  `mir_driver.ml` (old Ir -> Fir by `Fir_of_ir`). No optimization: the `-O`
  level changes nothing here. `bench/native/sieve*.fas` run about 1.8x
  slower than the old backend at `-O2` (it has mem2reg, constant folding,
  inlining, LICM, unrolling); code built to be constant-folded, such as
  `pz3_sys.fas`, about 10x slower. `FAS_NATIVE_LEGACY=1` selects the old backend.
- `FAS_MIR_REGS=n` gives the allocator only n registers per class, to make
  small programs spill.
- `fas-fir --mir file.fir`: compile and link through this backend;
  `--mir -c` writes an object.
- `FAS_MIR_DUMP=1` prints the selected and the allocated Mir to stderr.
- `FAS_FIR_TOOL=<a wrapper running fas-fir --mir> sh test/fas_fir.sh`: the
  Fir conformance programs (16/16 pass; `vectors.fir` and `c/unwind.fir`
  are `--mir` only).
- `fir_fuzz.exe -mir`: random modules compared with clang -O2.
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

## Block addresses

Unlike the old backend, which lowered `indirect_br` to a `switch` over
small tokens, a `blockaddr` here is the block's real code address: each
taken block gets a local symbol `.Lfas.blk.<fn>.<n>`, read RIP-relative in
code and through an `R_X86_64_64` relocation in data. `indirect_br` is
`jmp r11`.

## Not supported yet

Each of these is reported as `native backend: ...`, never miscompiled:

- atomic vector loads and stores, and vectors passed through `...`;
- named sections and thread-local globals (`Fir_to_ir.symbols` refuses them);
- aggregates aligned above 16, or with fields other than integers ≤ 64 bits,
  pointers, f32 and f64;
- 4-byte data relocations.
