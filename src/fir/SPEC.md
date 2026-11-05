# Fir — the Fas intermediate representation (draft 0)

Fir is the IR every Fas front end emits (Fas `lower.ml` today, a C front end later). The native backend and the LLVM emitter consume it.

It replaces `Ir` in four steps: a converter from `Ir`, then native codegen on Fir, then the LLVM emitter on Fir, then `lower.ml` emitting Fir directly.

Design goals, in order:

1. Easy to optimize and lower. SSA everywhere: every value has exactly one definition, and that definition dominates every use.
2. Fast. Data lives in dense arrays indexed by integer ids, with no polymorphic compare or hashing in hot paths.
3. Easy to emit. Front ends put variables in stack slots (`alloca`, `load`, `store`) and leave SSA construction to the `mem2reg` pass.
4. Target-neutral. Nothing here is x86 specific; targets own their calling convention and legalization.
5. Deterministic and thread-ready. No global mutable state: a module's tables are built once, and functions can later be compiled independently.

## 1. Types

```
ty ::= iN            integer of N bits, 1 <= N <= 8_388_608 (C23 _BitInt limit)
     | f16 | f32 | f64 | f80
     | ptr | ptr(K)   opaque pointer, address space K (default 0)
     | <N x T>        vector, N >= 1, T an integer, float or pointer type
```

- **Integers have no signedness.** Operations decide how to read their bits.
- **Legal widths.** The target legalizes widths it does not support natively:
  - x86-64 handles i1, i8, i16, i32, i64 and i128 (as register pairs);
  - wider integers become 64-bit limbs; odd widths are promoted.
- **f80** is the x87 80-bit format, stored in 16 bytes with 16-byte alignment on x86-64.
- **Aggregates are not types.** Structs and arrays exist only as memory: an `alloca` or global of some size and alignment, accessed through `ptradd`, `load` and `store`. Front ends compute layouts.
- **Calls are the exception.** They carry ABI types, described in section 7.

## 2. Values

A value is an integer id local to its function, with exactly one definition:

- an instruction result;
- a block parameter (this is how phis are written);
- a constant (see below).

Constants are values too. Each function interns them, so equal constants share one id:

```
iN <int>                     integers of any width, held as arbitrary precision
fN <bits>                    floats, held as exact bit patterns
null ptr / poison T / zero T
<N x T> [c0, c1, ...]        vector constants
@sym                         address of a global or function (ptr)
blockaddr(@fn, ^bb)          for computed goto
```

**Poison** is the only "undefined" value: there is no `undef`.
- An operation that receives poison produces poison, except `select` on its unchosen arm and `freeze`.
- Branching on poison, dereferencing it, or dividing by it is undefined behaviour.
- `freeze x` returns x if x is not poison, otherwise an arbitrary but fixed value.

## 3. Functions and blocks

- **Functions:** a function has a signature, attributes and blocks. The first block is the entry block.
- **Blocks:** a block has parameters (typed values), a list of instructions, and exactly one terminator.
- **Block arguments replace phis.** Each edge passes arguments to the target block's parameters.
- **The entry block's parameters are the function parameters.**
- **Dominance:** every use must be dominated by its definition. A block argument counts as a use at the end of the predecessor.

## 4. Instructions

Notation: `%r = op ty a, b`. Flags appear in brackets.

### Integer arithmetic (scalar or vector)
| Op | Flags | Notes |
|---|---|---|
| `add sub mul` | `nsw nuw` | wrap-around unless flagged; a flag violation yields poison |
| `udiv sdiv urem srem` | `exact` (div) | divide by 0 is UB; `sdiv MIN, -1` is UB |
| `shl` | `nsw nuw` | shift amount ≥ width yields poison |
| `lshr ashr` | `exact` | |
| `and or xor` | | `or disjoint` allowed (no common bits) |
| `smin smax umin umax`, `abs` | | `abs` has `poison_min` flag |
| `smulh umulh` | | high half of the full product |
| `sadd_sat uadd_sat ssub_sat usub_sat` | | |
| `sadd_ovf uadd_ovf ssub_ovf usub_ovf smul_ovf umul_ovf` | | two results: the value, and an `i1` overflow flag |
| `ctz clz` | `zero_poison` | |
| `popcnt bswap bitrev` | | |
| `fshl fshr` | | funnel shifts; rotate = funnel of x with x |

### Float arithmetic
| Op | Notes |
|---|---|
| `fadd fsub fmul fdiv frem fneg` | flags: `contract` (allows FMA formation), `fast` |
| `fma fabs sqrt floor ceil ftrunc roundeven copysign fmin fmax` | `fmin`/`fmax` follow IEEE minimumNumber/maximumNumber |

### Comparison and selection
- `icmp eq|ne|slt|sle|sgt|sge|ult|ule|ugt|uge` produces `i1` (or `<N x i1>` for vectors).
- `fcmp` takes the 14 LLVM predicates, ordered (`o*`) and unordered (`u*`), plus `ord` and `uno`.
- `select c, a, b` takes a scalar `i1`, or `<N x i1>` per lane.
- `freeze x`.

### Conversions
- `trunc zext sext`
- `fptrunc fpext`
- `fptosi fptoui`: out-of-range input produces poison. The `_sat` forms saturate instead.
- `sitofp uitofp`
- `ptrtoint inttoptr`
- `bitcast`: same size, never involving pointers.

### Memory
| Op | Notes |
|---|---|
| `alloca size, align` | `size` is a constant or a value (VLA). Constant allocas belong in the entry block. |
| `load T, p` | attributes: `align`, `volatile`, atomic ordering, alias tag |
| `store T v, p` | same attributes as `load` |
| `ptradd p, off` | `p + off`, where `off` is an integer value or constant. Form `ptradd p, idx * scale + disp`; flag `inbounds`. |
| `memcpy memmove memset` | size is a value or constant; `volatile` flag |
| `atomic_rmw op, p, v` | op: `xchg add sub and or xor min max umin umax fadd fsub`; has an ordering |
| `cmpxchg p, expected, new` | two results: the old value and an `i1` success flag; separate orderings for success and failure; `weak` flag |
| `fence ordering` | |

- **Orderings:** `relaxed acquire release acq_rel seq_cst`. Non-atomic is the default.
- **Alias information** is attached to loads, stores and memory calls:
  - `noalias` scopes, derived from C `restrict` and Fas parameters;
  - a type-based alias tag, ignored when the module turns strict aliasing off.

### Vectors
`extract v, i` · `insert v, x, i` · `shuffle a, b, [mask]` (mask entries are lanes, or `-1` for poison) · `splat x` · `reduce add|mul|and|or|xor|smin|smax|umin|umax|fmin|fmax v` · `reduce fadd_ordered` · `masked_load` · `masked_store` · `compress` · `expand`

`compress v, mask` packs the lanes of `v` whose mask bit is set into the low lanes, in order, and fills the remaining lanes with zero. `expand` is its inverse.

### Calls and system
- `call sig callee(args)`
  - `callee` is either `@sym` or a value.
  - Attributes: `noreturn`, `returns_twice`, `nounwind`, `tail`.
  - Results follow the signature.
- `syscall n, args` (Fas, Linux).
- `trap`, which doesn't return; the block continues with `unreachable`.
- `dbg_value var, v` records a variable location; it has no semantic effect.

Every instruction and every terminator may carry a source location id (`!loc N`), which indexes the module's location table of (file, line, column). Id 0 means no location.

## 5. Terminators

```
jump ^bb(args)
br c, ^t(args), ^f(args)            [weight optional]
switch v, ^default(args), [k0: ^b0(args), ...]
indirect_br p, [^b0, ^b1, ...]      targets are possible destinations
ret v0, ...
unreachable
```

## 6. Globals

A global has:

- a name;
- linkage: `local`, `global` or `weak`;
- visibility: `default`, `hidden` or `protected`;
- an optional section;
- an alignment;
- flags: `readonly` and `thread_local`;
- either a size with no initializer (zero-filled) or an initializer.

An initializer is a sequence of byte runs and relocations `{offset, @sym, addend, width}`.

Functions have the same linkage and visibility fields, plus attributes: `inline`, `noinline`, `cold`, `noreturn`, and the Fas extensions.

## 7. Signatures and the ABI

A signature lists parameter and result ABI types and a `variadic` flag:

```
abi ::= T                            a scalar or vector type
      | agg{size, align, [(off, T)...]}   aggregate passed by value; the field list
                                     is the flattened scalar layout the target
                                     classifies (SysV: eightbyte classes)
      | ext(T, zext|sext)            argument/result extension for narrow ints
```

Inside a function an aggregate is always a pointer (`ptr`), whatever its ABI:

- An `agg` parameter's value is a pointer to memory holding the argument. The callee may read it but must not write it or let it escape; it copies the bytes first if it needs to.
- An `agg` argument is a pointer to memory holding the value. The call reads the bytes at the call; the caller may change that memory afterwards.
- An `agg` call result is a pointer to a temporary that the call site owns, valid until the function returns. `ret p` for an `agg` result copies the bytes at `p` out.
- The target lowering decides whether the bytes travel in registers or on the stack, and inserts the copies.

Front ends never apply target ABI rules themselves.

## 8. Text format

The printer and parser round-trip exactly. Example:

```
global @counter : 8, align 8, local, zero

func @sum(ptr %p, i64 %n) -> i64 {
^entry(%p: ptr, %n: i64):
  jump ^loop(0, 0)
^loop(%i: i64, %acc: i64):
  %c = icmp ult i64 %i, %n
  br %c, ^body, ^done(%acc)
^body:
  %a = ptradd %p, %i * 8
  %x = load i64, %a, align 8
  %acc2 = add i64 %acc, %x
  %i2 = add [nuw] i64 %i, 1
  jump ^loop(%i2, %acc2)
^done(%r: i64):
  ret %r
}
```

## 9. In-memory representation

Per function:

- **Dense tables** (growable arrays): values (definition and type), instructions (opcode record), blocks (parameters, instruction id list, terminator), constants (interned).
- **Replace-all-uses-with** works through value aliases: rewriting `v` to `w` records `alias v = w`. Readers resolve aliases, and `compact` removes them. This makes RAUW O(1) without maintaining use-lists on every mutation.
- **Use lists, dominator trees and loop forests** are analyses computed on demand into flat arrays (CSR), and invalidated by a change counter.
- **Ids** are dense and never reused within a pass. `compact` renumbers them in program order, which makes output deterministic.

## 10. Migration

Until lower.ml emits Fir directly, Fir is produced from the old `Ir` (LLVM-shaped) module:

- `Fir_of_ir.convert` turns old Ir into Fir: phis become block parameters, typed GEPs become `ptradd` chains, aggregate SSA values become pointers to entry-block stack copies, and `llvm.*` intrinsic calls become Fir opcodes.
- `Fir_to_ir.convert` goes back. It exists only for testing and is not tuned for code quality.
- `Fir_hook.apply` sits between the front end and `--backend=native`:
  - `FAS_FIR=check` converts, verifies, and checks that print → parse → print is stable, then compiles the original Ir.
  - `FAS_FIR=roundtrip` also converts back and compiles that, so the native tests check the forward converter semantically.
  - `FAS_FIR_DUMP=path` writes the Fir text, and `FAS_FIR_IRDUMP=path` writes the converted-back Ir as LLVM text.
  - Without `FAS_FIR` the hook does nothing.

The v0.4 machine IR consumes Fir directly; at that point the old native passes and `Fir_to_ir` go away.
