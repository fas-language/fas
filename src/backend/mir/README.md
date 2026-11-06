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
    --Mir_emit--> X64 encoder, ELF via Native_object
```

| File | Role |
|---|---|
| `../../fir/fir_scalarize.ml` | Fir → Fir without vectors (see its header). A vector is a set of scalar lanes; one indexed at a run-time lane, or built by `insert`, lives in a fresh stack slot instead, so code size stays linear in the lanes. Vector parameters become their lanes, and vector results come back through hidden pointer parameters. This is the old backend's convention, not the SysV vector ABI. |
| `mir.ml` | The machine IR: x86-64 instructions over virtual (`V n`) and physical (`G r`, `X i`) registers, frame slots, blocks; `uses_defs`, `clobbers`, `map_regs`, printers. |
| `mir_isel.ml` | Fir → Mir. One Fir block is one Mir block with the same number; extra blocks (edges, loops, `va_arg` branches) are appended. SysV calls, parameters, returns and `va_*` live here. |
| `mir_ra.ml` | Spill-everywhere allocation with a block-local cache (see the header comment). |
| `mir_check.ml` | Verifies an allocation without trusting the allocator: an abstract machine over registers and slots, iterated to a fixpoint. |
| `mir_runtime.ml` | Appends the correctly rounded `fma` helpers (`backend/native/runtime/fma.fas`) to a module that uses `fma`: baseline x86-64 has no FMA instruction. |
| `mir_emit.ml` | Frame layout, encoding, data sections, `_start`, the object. |

## Running it

- `fas-fir --mir file.fir`: compile and link through this backend;
  `--mir -c` writes an object.
- `FAS_MIR_DUMP=1` prints the selected and the allocated Mir to stderr.
- `FAS_FIR_TOOL=<a wrapper running fas-fir --mir> sh test/fas_fir.sh`: the
  Fir conformance programs (14/14 pass; `vectors.fir` is `--mir` only).
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

## Block addresses

Unlike the old backend, which lowered `indirect_br` to a `switch` over
small tokens, a `blockaddr` here is the block's real code address: each
taken block gets a local symbol `.Lfas.blk.<fn>.<n>`, read RIP-relative in
code and through an `R_X86_64_64` relocation in data. `indirect_br` is
`jmp r11`.

## Not supported yet

Each of these is reported as `mir backend: ...`, never miscompiled:

- atomic vector loads and stores, and vectors passed through `...`;
- named sections and thread-local globals (`Fir_to_ir.symbols` refuses them);
- aggregates aligned above 16, or with fields other than integers ≤ 64 bits,
  pointers, f32 and f64;
- 4-byte data relocations.
