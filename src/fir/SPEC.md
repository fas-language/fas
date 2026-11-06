# Fir — the Fas intermediate representation

Fir is the IR that front ends emit and backends consume. Today Fas produces it, through `Fir_of_ir`. A C front end, or any other language, can write it directly as text and compile it with `fas-fir`.

This file defines the IR and its text format. It is checked against the real printer (`fir_print.ml`), parser (`fir_parse.ml`) and verifier (`fir_verify.ml`). §12 says what the native x86-64 backend compiles today. The programs in `test/fas_fir/` are working examples, run by `test/fas_fir.sh`.

Contents:

1. Model
2. Types
3. Values and constants
4. Modules, globals, functions
5. Signatures and the ABI
6. Instructions
7. Terminators
8. Undefined behaviour and poison
9. Text format reference
10. Program startup and syscalls
11. Writing a front end (Braun SSA or stack slots)
12. Native backend support status
13. The `fas-fir` tool
14. In-memory representation (for the OCaml side)

---

## 1. Model

- **SSA:** every value has exactly one definition, and that definition dominates every use.
- **Block parameters instead of phis.** A block lists typed parameters, and each edge into the block passes arguments for them. The entry block's parameters are the function's parameters.
- **No aggregate values.** Structs and arrays exist only in memory: `alloca`, globals, `ptradd`, `load`, `store`. The front end computes layouts (sizes, alignments and field offsets). Calls are the one place aggregates appear, as ABI descriptions (§5).
- **Integers have no signedness.** Each operation decides how to read the bits: `sdiv`/`udiv`, `slt`/`ult`, and so on.
- **Target-neutral by design.** Only the triple names the target, and the current backend only supports `x86_64-unknown-linux-gnu`.

## 2. Types

```
ty ::= iN              integer, 1 <= N <= 8388608     (i1 i8 i16 i32 i64 i128 ...)
     | f16 | f32 | f64 | f80
     | ptr | ptr(K)    opaque pointer, address space K (0 if omitted)
     | <N x T>         vector of N lanes; T is an integer, float or pointer type
```

- There is no `void` type. A function with no results has the result list `()`.
- `i1` is the boolean. It is stored in memory as one byte holding 0 or 1.
- Sizes in memory:
  - i1 and i8 take 1 byte, i16 takes 2, i32 takes 4, i64 takes 8, i128 takes 16;
  - wider integers round up to 8-byte limbs;
  - f80 takes 16 bytes;
  - ptr takes 8 bytes;
  - `<N x i1>` packs to ceil(N/8) bytes, and other vectors take N × the element size.
- **Odd widths in memory** (any `iN` whose size above is more than N bits, such as `i7`, `i33` or `i100`): a `store` writes the value zero-extended to the full size, and a `load` ignores the bits above N. A front end must not rely on what a load sees in those bits when something other than a store of the same type wrote them.

## 3. Values and constants

A value is one of:

- an instruction result;
- a block parameter;
- a constant.

In text, named values are written `%name`. A name is bare `[A-Za-z0-9_.$]+` or quoted (`%"x y"`).

**Constants are written inline, always with their type:**

| Text | Meaning |
|---|---|
| `i64 7`, `i32 -1`, `i8 0x7f` | Integer. Decimal (may be negative) or `0x` hex; any width. Bits above the width must be zero once sign-adjusted. |
| `f64 0x3ff0000000000000`, `f32 0x40490fdb` | Float, given as its exact IEEE bit pattern in hex. There are no decimal float literals. |
| `ptr null` | The null pointer. |
| `i32 poison` | Poison (§8). There is no `undef`. |
| `i64 zero`, `<4 x i32> zero` | All-zero value of any type. |
| `<2 x i64> [i64 1, i64 2]` | Vector constant. Each lane is a constant. |
| `ptr @sym`, `ptr @sym+16`, `ptr @sym-8` | Address of a global or function, plus a byte addend. |
| `ptr blockaddr(@f, 3)` | Address of block 3 of `@f`, for computed goto. Blocks are numbered in order of appearance, the entry being 0. `@f` must be defined in this module, and the block must not be the entry or take parameters (verified). |

**A global's address is a typed constant.** Write `load i64 ptr @counter, align 8`, not `load i64 @counter`. A bare `@counter` gives the error "expected a type".

Value ids are local to a function. The same constant written twice is the same value.

## 4. Modules, globals, functions

A module is, in order:

```
triple "x86_64-unknown-linux-gnu"          required, first
strict_aliasing false                      optional (default: true)
loc 1 "file.c" 12 5                        optional location table: id, file, line, column
alias_tag 1 "int" 0                        optional TBAA tags: id, name, parent id (0 = root)
global ...                                 globals
alias ...                                  symbol aliases
func ... / declare ...                     functions
```

`;` starts a comment that runs to the end of the line. Order inside a module doesn't matter for references: globals, functions and blocks may be named before they're defined.

### Globals

```
global @name size N align A LINKAGE [VISIBILITY] [section "s"] [readonly] [thread_local] = INIT

LINKAGE    ::= local | global | weak
VISIBILITY ::= hidden | protected             (default visibility is not written)
INIT       ::= zero                            N zero bytes (.bss)
             | extern                          defined elsewhere; size/align describe it
             | { PIECE, PIECE, ... }           the pieces must add up to exactly N bytes
PIECE      ::= bytes "..."                     raw bytes; escapes \"  \\  \xx (two hex digits)
             | zeros K                         K zero bytes
             | reloc @sym[+-addend] w8         8-byte absolute address of sym+addend
             | reloc @sym[+-addend] w4         4-byte address (not supported natively yet)
             | blockaddr(@f, N)                8 bytes: the constant ptr blockaddr(@f, N) (§3)
```

Examples:

```
global @msg     size 6  align 1 local readonly = {bytes "hello\0a"}
global @counter size 8  align 8 local = zero
global @table   size 24 align 8 global readonly = {reloc @msg w8, reloc @msg+2 w8, zeros 8}
global @errno   size 4  align 4 global thread_local = zero
global @environ size 8  align 8 global = extern
```

- `local` symbols are file-private, like C `static`. `global` symbols are exported. `weak` symbols can be overridden at link time: a strong definition in another object wins, and every reference, including calls and loads in this module, reaches the winner. So a weak function is never inlined. A weak `extern` global or weak `declare` that nothing defines has address 0 (`ptrtoint` of it is 0), as with C's `__attribute__((weak))` declarations.
- Visibility is the ELF `st_other` value: `hidden` symbols are not exported from the shared object or executable they end up in, `protected` ones are exported but not preemptible.
- `readonly` places the global in .rodata.
- `thread_local` places it in .tdata/.tbss. Its address (`ptr @errno`) is per-thread.
- **Verifier rules:**
  - the alignment is a power of two;
  - the initializer's byte count equals `size`;
  - no symbol is defined twice.

### Aliases

```
alias @name LINKAGE [VISIBILITY] = @target [+ N]
```

A second name for a function or global defined in this module, as C's `__attribute__((alias("target")))`. With `+ N` (globals only, `0 <= N <= size`) it names the address N bytes into the global. Linkage and visibility are the alias's own: `alias @api global = @impl` exports a `local` function under another name, and the target keeps its local symbol. A reference to `@name` (`ptr @name`, `call ... @name(...)`, `reloc @name w8`) is a reference to that symbol, so a `weak` alias another object overrides is overridden for this module's calls too. An alias of a function is called with the target's signature.

```
alias @memcpy_alias global = @my_memcpy
alias @second       local  = @pair + 8
alias @hook         weak hidden = @default_hook
```

- **Verifier rules:** the target is a function or global *defined* in this module, not an alias (so there are no cycles); a function alias has no addend; an addend stays inside the global; the alias name is defined nowhere else.

### Functions

```
func @name (ABI, ...) -> (ABI, ...) LINKAGE [VISIBILITY] [ATTRS] [display "src name"] {
^entry(%a: T, ...):
  ...instructions...
  TERMINATOR
^other(%p: T, ...):
  ...
}

declare @name (ABI, ...) -> (ABI, ...) LINKAGE        a function defined elsewhere

ATTRS ::= any of: inline noinline cold noreturn
```

- **The first block is the entry block.** Its parameters must match the signature's parameter types, where an `agg` parameter is a `ptr` (§5). Nothing may branch to the entry block, so if you need a loop header at the top, add a second block.
- **Each block is a label, then zero or more instructions, then exactly one terminator.**
- **Labels** are written `^name`. A block with no parameters is written `^name:`, and one with parameters `^name(%x: i64, %y: ptr):`.
- **Any instruction or terminator may end with `!loc N`**, where N is an id from the `loc` table.
- **Dominance:** a value can be used only where its definition dominates the use. A block argument is a use at the end of the predecessor block. Unreachable blocks are not checked.

## 5. Signatures and the ABI

```
SIG ::= (ABI, ABI, ... [, ...]) -> (ABI, ...)       a trailing "..." makes it variadic
ABI ::= T                        scalar or vector, passed as is
      | zext T | sext T          a narrow integer that the ABI widens (C char/short/_Bool)
      | agg(SIZE, ALIGN, [OFF: T, OFF: T, ...])
                                 aggregate by value, with its flattened scalar layout
```

Inside the function, **an aggregate is always a `ptr`:**

- **`agg` parameter:** the entry-block parameter is a `ptr` to the argument bytes. The callee may read them, but it must not write them or let the pointer escape. Copy the bytes first if you need to.
- **`agg` argument:** you pass a `ptr` to the value's bytes. The call reads them at the moment of the call.
- **`agg` result:** the call yields a `ptr` to a temporary that lasts until the caller returns. In the callee, `ret %p` copies the bytes out of `%p`.
- **The backend does the classification** (SysV eightbytes, registers or stack). A front end never applies target ABI rules itself.

How the native backend classifies an `agg` (x86-64 SysV, the same as clang for the same C struct):

- **Fields** are taken from the `[OFF: T, ...]` list, so the list must name every scalar the C struct holds, with nested structs and arrays flattened (`int a[5]` is five `i32` fields). Padding is simply not listed.
- **Up to 16 bytes:** each 8-byte half ("eightbyte") is SSE when it holds only `f32`/`f64` fields, otherwise INTEGER. The value goes in one or two registers: INTEGER halves in the next GPRs, SSE halves in the next XMM registers. If not all of its halves fit in the registers that are left, the whole value goes on the stack, and later scalar arguments still use the remaining registers. Results come back in `rax`/`rdx` and `xmm0`/`xmm1` the same way.
- **Over 16 bytes, or a field that is not naturally aligned:** MEMORY class. An argument is copied onto the stack. A result is written to memory that the caller provides (a hidden first argument in `rdi`, returned again in `rax`).
- **Not classified:** a vector, `i128` or `f80` field, an `ALIGN` above 8 (except the `agg(16, 16, [0: i64, 8: i64])` form of `__int128` below), or an eightbyte with no fields. Between Fir functions in one module such an `agg` still works (it is passed by address). At an external boundary (a `global` or `extern` function) the backend refuses it.
- **A struct through `...`:** a variadic argument has no ABI in the signature, so the call names it: `call (i32, ...) -> (i64) @f(i32 1, agg(16, 8, [0: i64, 8: f64]) %p)`. The operand is a `ptr` to the bytes, as for any `agg` argument, and it is classified by the rules here. Without the `agg(...)` prefix a `ptr` is passed as a pointer. The prefix is only allowed on variadic arguments (not the fixed ones, whose ABI the signature gives) and only on a `ptr` operand. The callee reads it with `va_arg agg(...)` (§6).

**Integers that are not 1, 8, 16, 32 or 64 bits wide** in a signature (the native backend, x86-64 SysV):

- **Up to 64 bits** (`i7`, `i33`, ...): passed in the next wider of i8/i16/i32/i64, with the bits above N zero. For N up to 32, `sext iN` instead sign-extends from bit N−1 (clang passes signed `_BitInt(N)` that way and the callee relies on it); for N from 33 to 63, `sext` changes nothing, since there the psABI leaves the bits above N unspecified. `zext iN` is the same as plain `iN`. On receipt the bits above N are always ignored. This matches clang's `_BitInt(N)` for N up to 64: unsigned as plain `iN`, signed as `sext iN`.
- **65 to 128 bits:** as `__int128`, i.e. as `agg(16, 16, [0: i64, 8: i64])`: two GPRs, or 16-byte aligned on the stack; the bits above N are zero. clang's `_BitInt(65..128)` is passed differently (8-byte aligned on the stack); a front end that needs that ABI writes `agg(16, 8, [0: i64, 8: i64])` and passes the value in memory.
- **Above 128 bits:** MEMORY class, like an `agg` of ceil(N/64) `i64` fields (on the stack as an argument, through sret as a result), the bits above N zero when sent and ignored when received (the psABI leaves them unspecified). This matches clang's `_BitInt(N)` for N > 128.
- `va_arg` of such an integer reads it as the call passed it (§6).

`zext i8` and `sext i16` change only what the other side may assume about the upper bits. The value itself is still an `i8` or `i16`. For C:

- `_Bool` → `zext i1`
- `char` / `signed char` → `sext i8`
- `unsigned char` → `zext i8`
- `short` → `sext i16`
- `unsigned short` → `zext i16`
- `int` → `i32`

**Every call site states the signature it calls with.** For example, `call (ptr, ...) -> (i32) @printf(%fmt, i64 1)`. The call must pass at least the fixed parameters, with matching types, and it defines exactly as many values as the signature returns.

## 6. Instructions

Text form: `%r = OPCODE [FLAGS] RESULT_TYPE operands...`. An instruction with no result has no `%r =` part, and one with two results is written `%a, %b = ...`. In this section, "ty" means the result type, which is printed right after the opcode and its flags.

### Integer arithmetic (scalar or vector of integers)

| Text | Meaning | Flags |
|---|---|---|
| `add sub mul ty a, b` | Wrapping arithmetic. | `nsw nuw`: overflow makes the result poison |
| `udiv sdiv urem srem ty a, b` | Division. Dividing by 0 is UB, and so is `sdiv/srem MIN, -1`. | `exact` (udiv/sdiv): a nonzero remainder makes the result poison |
| `shl lshr ashr ty a, b` | Shifts. A shift amount ≥ the width gives poison. | `nsw nuw` (shl), `exact` (lshr, ashr) |
| `and or xor ty a, b` | Bitwise operations. | `disjoint` (or): the operands share no set bits |
| `smin smax umin umax ty a, b` | Minimum and maximum. | |
| `smulh umulh ty a, b` | The high half of the double-width product. | |
| `sadd_sat uadd_sat ssub_sat usub_sat ty a, b` | Saturating arithmetic. | |
| `ctz clz popcnt ty a` | Count trailing zeros, leading zeros, or set bits. | `zero_poison` (ctz, clz): an input of 0 gives poison |
| `bswap ty a` | Byte swap. The width must be a multiple of 16. | |
| `bitrev ty a` | Bit reversal. | |
| `abs ty a` | Absolute value. | `poison_min`: abs(MIN) gives poison instead of MIN |
| `fshl fshr ty a, b, amt` | Funnel shifts. A rotate is `fshl x, x, n`. | |
| `%v, %o = sadd_ovf (ty, i1) a, b` | Arithmetic with overflow check: the wrapped value plus an `i1` overflow flag. Also `uadd_ovf ssub_ovf usub_ovf smul_ovf umul_ovf`. | |

Flags are written right after the opcode: `add nsw i32 %a, %b`. The verifier rejects flags that don't apply to an instruction.

### Float arithmetic (scalar or vector of floats)

| Text | Meaning |
|---|---|
| `fadd fsub fmul fdiv frem ty a, b` | IEEE arithmetic. `frem` is C `fmod`. |
| `fmin fmax ty a, b` | IEEE minimumNumber / maximumNumber. |
| `copysign ty a, b` | The magnitude of a with the sign of b. |
| `fneg fabs sqrt floor ceil ftrunc roundeven ty a` | Unary operations. `fneg` only flips the sign bit. |
| `fma ty a, b, c` | a·b+c with a single rounding. |

All of these allow the flags `contract` (may be fused into an FMA) and `fast`.

### Comparison, selection, conversion

| Text | Meaning |
|---|---|
| `icmp PRED i1 a, b` | Integer or pointer compare, with `PRED ∈ eq ne slt sle sgt sge ult ule ugt uge`. The result type is `i1`, or `<N x i1>` for vectors. **The type printed is the result type, `i1`, not the operand type.** |
| `fcmp PRED i1 a, b` | Float compare, with `PRED ∈ oeq ogt oge olt ole one ord ueq ugt uge ult ule une uno true false`. In an `o*` predicate a NaN makes the result false, and in a `u*` predicate it makes the result true. Flag `fast` is allowed. |
| `select ty c, a, b` | `c ? a : b`. `c` is `i1`, or for vectors `<N x i1>` per lane. |
| `freeze ty a` | `a` if it isn't poison, otherwise some fixed arbitrary value. |
| `trunc zext sext ty a` | Integer width changes: trunc must narrow, zext and sext must widen. |
| `fptrunc fpext ty a` | Float width changes. |
| `fptosi fptoui ty a` | Float to integer, truncating toward zero. An out-of-range input or NaN gives poison. This matches C casts. |
| `fptosi_sat fptoui_sat ty a` | The saturating version: NaN gives 0 and out-of-range values clamp. Fas `as` uses this form. |
| `sitofp uitofp ty a` | Integer to float. |
| `ptrtoint inttoptr ty a` | Pointer ↔ integer. |
| `bitcast ty a` | Same total bit count, no pointers, and the lane count may change, e.g. `<8 x i1>` ↔ `i8`. |

For every conversion the type written is the **result** type: `%w = sext i64 %b` widens `%b` to i64.

### Memory

| Text | Meaning |
|---|---|
| `%p = alloca ptr SIZE, align A` | Stack memory of `SIZE` bytes, where SIZE is an integer value such as `i64 16`, or a variable (`%n`, any integer type, read as unsigned). Put constant-size allocas in the entry block. Memory from an alloca lives until the function returns, or until a `stackrestore` to a point before it. |
| `%s = stacksave` | The current stack position, as a `ptr` for `stackrestore`. Its value is only good for that. |
| `stackrestore S` | Frees every alloca made since the `stacksave` that produced S. Use the pair around a scope that holds variable-size allocas (C VLAs), above all inside loops, where otherwise each iteration grows the stack. |
| `%x = load ty P, MEM` | Load a `ty` from pointer P. |
| `store V, P, MEM` | Store V at P. No type is written, because V carries it. |
| `%q = ptradd [inbounds] ptr P, IDX, SCALE, DISP` | Address arithmetic: P + IDX·SCALE + DISP. IDX is an integer value (read as signed; use i64), and SCALE and DISP are literal integers. For a constant offset write `ptradd ptr %p, i64 0, 1, 24`. |
| `memcpy [volatile] D, S, N` | Copy N bytes; the regions must not overlap. N is an integer value. |
| `memmove [volatile] D, S, N` | Copy N bytes; the regions may overlap. |
| `memset [volatile] D, BYTE, N` | Fill N bytes. BYTE is an `i8`. |
| `%old = atomic_rmw OP ty P, V, MEM` | Atomic read-modify-write, with `OP ∈ xchg add sub and or xor min max umin umax fadd fsub`. MEM must include an ordering. |
| `%old, %ok = cmpxchg [weak] (ty, i1) P, EXPECTED, DESIRED, MEM, fail ORD` | Compare and swap: yields the old value and a success flag. |
| `fence ORD` | ORD is acquire or stronger. |

`MEM` is written as `align A [volatile] [ORDERING] [!alias N] [!noalias [N, ...]]`:

- `ORDERING` is one of `relaxed acquire release acq_rel seq_cst`. When it is omitted, the access is plain and not atomic.
- A load can't have `release` or `acq_rel`, and a store can't have `acquire` or `acq_rel`.
- `!alias N` refers to an `alias_tag` (type-based aliasing). `!noalias` lists scopes; C `restrict` maps here. Both are optional hints, and you can leave them out.

Alignment is a power of two. It is a promise about the address, so writing `align 1` is always safe.

### Vectors

| Text | Meaning |
|---|---|
| `extract ty V, IDX` | Read lane IDX. The result type is the element type. |
| `insert <N x T> V, X, IDX` | Write X into lane IDX. |
| `shuffle <M x T> A, B, [k0, k1, ...]` | Lanes picked from the concatenation A‖B. A mask entry of `-1` gives a poison lane. |
| `splat <N x T> X` | Broadcast X to every lane. |
| `reduce OP T V` | Horizontal reduction, with `OP ∈ add mul and or xor smin smax umin umax fmin fmax fadd_ordered`. |
| `masked_load ty P, MASK, PASSTHRU, align A` | Load only the lanes whose mask bit is set. |
| `masked_store V, P, MASK, align A` | Store only the lanes whose mask bit is set. |
| `compress ty V, MASK` | Pack the selected lanes into the low lanes and zero the rest. `expand ty V, MASK` is the inverse. |

### Calls and system

| Text | Meaning |
|---|---|
| `%r = call [ATTRS] SIG @f(args)` | Direct call. When there are no results, drop `%r =`, and when there are several, write `%a, %b = call ...`. |
| `%r = call [ATTRS] SIG %fp(args)` | Indirect call through a `ptr`. |
| `%r = syscall i64 NR, a1, ..., a6` | Linux syscall. **Every operand is an i64**, including NR, so pointers go through `ptrtoint` first. The result is the raw kernel return value; −4095..−1 means −errno. |
| `va_start P` | Only in a variadic function. Fills the `va_list` at `P` so that it reads this function's unnamed arguments, starting after the fixed ones. |
| `%x = va_arg T P` | Reads the next unnamed argument through the `va_list` at `P` and advances it. `T` is an integer, `ptr`, `f32` or `f64`. Up to 64 bits it reads one 8-byte argument word and keeps its low `T` bits; a wider integer is read as the call passed it (§5: `i65`..`i128` as `__int128`, wider as MEMORY class). |
| `%p = va_arg agg(SIZE, ALIGN, [FIELDS]) P` | Reads the next unnamed argument as a struct by value (SysV va_arg for aggregates: from the register save area when all its eightbytes still fit there, otherwise whole from the overflow area; MEMORY class always from the overflow area). `%p` is a `ptr` to a copy owned by this instruction: it stays valid until the function returns or the same instruction runs again (a loop overwrites it), so copy it out if you need it longer. The fields follow the §5 `agg` rules. |
| `trap` | Abort (ud2). Usually followed by `unreachable`. |
| `dbg_value VAR, V` | Debug info: source variable VAR currently holds V. It has no effect on semantics. |

Call ATTRS are any of `noreturn returns_twice nounwind tail`.

**The `va_list`** is the x86-64 SysV one, 24 bytes with align 8, and the front end allocates it:
- **Layout:** `{ u32 gp_offset; u32 fp_offset; ptr overflow_arg_area; ptr reg_save_area }`. It is the same as C's `va_list`, so a C function can hand its `va_list` to a Fir function (which takes it as a `ptr`) and the reverse.
- **`va_end`** emits nothing.
- **`va_copy(d, s)`** is `memcpy D, S, i64 24`.
- **C's default promotions still apply.** Callers pass `float` as `f64`, and integers narrower than `int` as `i32`, so read them with `va_arg f64` and `va_arg i32`.
- **Aggregates by value** are passed with an `agg(...)` prefix on the call argument (§5) and read with `va_arg agg(...)`. C's `va_arg(ap, struct S)` is `va_arg agg(...) %ap` with the same `agg` the struct would have as a parameter.

## 7. Terminators

Every block ends with exactly one terminator.

| Text | Meaning |
|---|---|
| `jump ^b(ARGS)` | Unconditional jump. Write just `^b` when the block takes no arguments. |
| `br C, ^t(ARGS), ^f(ARGS)[, weight W]` | Conditional branch on an `i1`. W is the probability of the true edge, out of 1000. |
| `switch V, ^default(ARGS), [K: ^b(ARGS), ...]` | Multi-way branch on an integer value. Keys are written without a type and must be distinct. |
| `indirect_br P, [^b1, ^b2, ...]` | Computed goto. The list holds every possible target, and those blocks take no parameters. `P` must be a `blockaddr` of this function for one of the listed blocks, possibly after arithmetic that lands back on one (`&&a + (&&b - &&a)`); anything else is undefined, and the current backends trap. |
| `ret` / `ret V` / `ret V1, V2` | Return. The values must match the result list. |
| `unreachable` | Control never gets here. |

Block arguments must match the target's parameters in number and type. No branch may target the entry block.

## 8. Undefined behaviour and poison

- **Poison** is the only undefined value. Any operation with a poison operand produces poison, with two exceptions:
  - `select` on its unchosen arm;
  - `freeze`.
- **Turning poison into UB:** the following are undefined behaviour:
  - branching on poison;
  - using poison as an address;
  - dividing by poison;
  - storing poison and later reading it as anything other than poison.
- **These produce poison:**
  - violating a flag (`nsw`, `nuw`, `exact`, `disjoint`, `zero_poison`, `poison_min`);
  - a shift amount at or above the width;
  - converting an out-of-range float with plain `fptosi`/`fptoui`.
- **These are UB directly:**
  - dividing by zero, or `sdiv MIN, -1`;
  - loading or storing through an invalid or misaligned pointer;
  - a data race on a non-atomic access;
  - reaching `unreachable`.
- **Mapping C's UB:** C's signed overflow becomes `add nsw`, which is allowed but not required. Unsigned arithmetic uses plain `add`. If you don't want to rely on UB, never set the flags: unflagged Fir is fully defined apart from division and memory errors.

## 9. Text format reference

Lexical rules:

- `;` starts a comment that runs to the end of the line.
- Whitespace and newlines are free-form, except inside strings.
- Names `@g`, `%v` and `^b` are bare `[A-Za-z0-9_.$]+` or quoted `"..."`. Strings use the escapes `\"`, `\\` and `\xx` (hex).
- Integer literals are decimal, which may be negative, or `0x...` hex.

You may reference values and blocks before they are defined. The parser creates placeholders, so you can print blocks in any order as long as the entry block comes first.

`fas-fir --check file.fir` parses, verifies and prints the module back in canonical form. That makes it the quickest way to see whether you produced valid Fir. Error messages look like:

```
fas-fir: x.fir:28: expected a type, got @table                      (parse error, with line)
fas-fir: x.fir: verifier:
@putnum: ^2 inst 11: syscall argument 2 is ptr, expected i64       (verifier: function, block, inst)
```

The verifier's `^2` and `inst 11` are internal numbers: block 2 is the third block, and instruction 11 is the twelfth instruction created in the function.

## 10. Program startup and syscalls

- **There is no libc.** `fas-fir` links a static, freestanding x86-64 Linux executable.
- **`_start` calls `@main` and exits with its result.** The call is `exit_group(main(argc, argv))`. `@main` must be defined, and it is declared as either of:

  ```
  func @main () -> (i32) global { ... }
  func @main (i32, ptr) -> (i32) global { ^e(%argc: i32, %argv: ptr): ... }
  ```

- **Everything else is a syscall.** The numbers you need most on x86-64 Linux:

  | Syscall | Number |
  |---|---|
  | read | 0 |
  | write | 1 |
  | open | 2 |
  | close | 3 |
  | lseek | 8 |
  | mmap | 9 |
  | munmap | 11 |
  | exit_group | 231 |
  | openat | 257 |

  A minimal write:

  ```
  %pi = ptrtoint i64 ptr @msg
  %n  = syscall i64 i64 1, i64 1, %pi, i64 6     ; write(1, msg, 6)
  ```

- **Linking other objects:** extra objects on the `fas-fir` command line are linked in. `test/fas_fir.sh` links `putnum.fir` into every test this way. Objects can come from:
  - another `fas-fir -c`;
  - `fas-native -c` on a Fas file. Only functions inside `extern "C" { ... }` are visible; everything else in Fas is private. For example, `extern "C" { fn hello(x u64) void { ... } }` is called from Fir as `call (i64) -> () @hello(i64 42)`, with `declare @hello (i64) -> () global` at the top.

## 11. Writing a front end (Braun SSA or stack slots)

There are two valid ways to produce Fir. You can start with the first and change later without touching the backend.

### A. Stack slots (simplest)

- Give every local variable an `alloca` in the entry block, and use `load`/`store` for every read and write.
- Blocks need no parameters, apart from the entry block's.
- The native backend promotes the slots to registers itself (mem2reg). This is what Fas does today: run `FAS_FIR=check FAS_FIR_DUMP=out.fir fas-native x.fas` and read `out.fir`.

### B. Direct SSA (Braun et al. 2013, "Simple and Efficient Construction of SSA Form")

Block parameters are the phis. The algorithm works per function:

- **Variable map:** keep `current_def[var][block]`.
- **`write(var, block, value)`** sets `current_def[var][block] = value`.
- **`read(var, block)`:**
  - if the block has a local definition, return it;
  - if the block is not sealed (predecessors may still be added), add a new block parameter, record it as an incomplete phi, and return it;
  - if the block has exactly one predecessor, recurse into that predecessor;
  - otherwise, add a block parameter, write it first (this breaks cycles), then pass `read(var, pred)` from each predecessor as the edge argument, and remove the parameter if it turns out trivial.
- **Seal a block** once all its predecessors are known: after the loop's back edge for a loop header, and right away for straight-line code. Sealing completes its incomplete phis.
- **Trivial parameter:** every incoming argument is the parameter itself or one value X. Replace all uses with X and drop the parameter.

In Fir this means:

- **Edge arguments live on the predecessor's terminator.** A phi operand for predecessor P is an argument on P's `jump`/`br`/`switch` to this block. Adding a parameter to block B means adding one argument to every terminator that targets B.
- **A block that is reached twice from the same predecessor** (`br c, ^b, ^b`) needs a forwarding block on one of the edges, since each edge carries its own arguments.
- **Keep the function in memory until it's finished, then print it.** The text format needs the final parameter list of each block. Because forward references are allowed, print order doesn't matter beyond entry-first.
- **Replacing all uses:**
  - Easiest: give each value one level of indirection, a `value_id → value_id` forwarding table like Fir's own `Alias`, and resolve it when printing.
  - The alternative is to keep use lists.
- **Variables whose address is taken** (`&x` in C) must stay in memory, so give them an `alloca` and treat them as in approach A. A per-function pre-scan that marks names appearing after `&` is enough. Arrays and structs always live in memory anyway.
- **What C constructs need from Fir:**

  | C construct | How to build it in Fir |
  |---|---|
  | `&&` / `\|\|` | Branches plus a merge block that takes an `i1` parameter. |
  | `?:` | Either `select` when both arms are side-effect free, or branches plus a merge parameter. |
  | `switch` | `switch`. |
  | `goto` and labels | Ordinary blocks. Seal a label's block only when the function ends, or once all gotos to it are known. |
  | `&&label`, `goto *p` (GNU) | `ptr blockaddr(@f, N)` and `indirect_br %p, [...]` listing every label whose address is taken in the function. A `static void *t[] = {&&a, ...}` is a global initialized with `blockaddr(@f, N)` pieces (§4). Such labels' blocks take no parameters, so keep the variables live across them in `alloca`s. |
  | VLA `T a[n]` | At the start of the VLA's block, `%s = stacksave`; then `%a = alloca ptr %bytes, align A` with `%bytes = n·sizeof(T)`; on every exit from the block (including `break`, `continue`, `goto` out of it), `stackrestore %s`. `alloca(n)` is the same alloca without the pair. `sizeof a` is the `%bytes` value. |
  | `...`, `va_list`, `va_arg` | A variadic signature (`(i32, ...)`). `va_list ap` is a 24-byte `alloca`, `va_start(ap, last)` is `va_start %ap`, and `va_arg(ap, T)` is `va_arg T %ap`, and for a struct `T`, `va_arg agg(...) %ap` (a `ptr` to a copy). A struct passed to a variadic function is written `agg(...) %p` in the call's argument list. A `va_list` parameter is a `ptr`, since C arrays decay to pointers. See §6. |

### Struct, array and pointer access

- **Field `s.f`:** `ptradd ptr %s, i64 0, 1, OFFSET_OF_F`.
- **Array element `a[i]`:** `ptradd ptr %a, %i64, SIZEOF_ELEM, 0`, after a `sext i64` of a narrower int index.
- **Pointer difference `p - q`:** `ptrtoint` both pointers, `sub`, then `sdiv exact` by the element size.
- **Struct copy `s = t`:** `memcpy %s, %t, i64 SIZE`.

## 12. Native backend support status

**Everything in §2–§7 parses and verifies, but the native backend compiles only a subset.** `fas-fir` reports the rest as `not supported by the native backend yet: ...` or `native backend: ...`. Each gap below has a workaround that needs only supported features.

| Area | Supported natively | Not yet | Workaround |
|---|---|---|---|
| Integer types | Every width. Integers other than i1 i8 i16 i32 i64 are split into i64 limbs before code generation (`Fir_wide`; `fas-fir --legalize` shows the result); division, wide conversions to and from float, and shifts use small helper functions that are added to the module | Vectors of odd-width integers; atomic load/store, `atomic_rmw` and `cmpxchg` on integers wider than 64 bits; `atomic_rmw` add/sub/min/max on odd widths up to 64 | Use i64 lanes; guard wide atomics with a lock. |
| Float types | f32 f64 | f16 f80 | — |
| Vectors | Loads, stores, lane-wise arithmetic, compares, select, `extract`, `insert`, `shuffle`, `splat`, `reduce or` | Other reductions, `masked_load/store`, `compress`, `expand`; vector constants with non-constant lanes | Scalar loops. |
| Integer ops | Everything in §6 for scalars, including `bswap`, `bitrev`, `abs` and the `*_ovf` family (expanded into shifts, masks and compares) | Vector `bswap`/`bitrev`/`abs`/`*_ovf`, `*_ovf` on i1, vector/i1 `smulh`/`umulh` | Scalar loops; for i1 overflow, widen to i8. |
| Float ops | fadd fsub fmul fdiv fneg fabs sqrt floor ceil roundeven fma; scalar fmin fmax copysign ftrunc (fmin/fmax follow §6: NaN loses, -0 < +0) and frem (exact, as C `fmod`, through x87 `fprem`) | Vector frem fmin fmax copysign ftrunc | Scalar loops. |
| fcmp | All 16 predicates on scalars | Predicates other than oeq une olt ole ogt oge on vectors | Lane-wise: combine the six basic predicates, e.g. `ult a,b` = `xor (oge a,b), true`. |
| Conversions | All, including plain and `_sat` fptosi/fptoui | — | — |
| Memory | alloca of any size (constant, or variable with any align), `stacksave`/`stackrestore`, load/store (plain, volatile, atomic), ptradd, memcpy/memmove/memset of any size (constant or variable) and any byte, overlap-safe memmove | — | — |
| Atomics | `atomic_rmw` xchg add sub and or xor, `cmpxchg`, `fence` | rmw min max umin umax fadd fsub | A cmpxchg loop. |
| Calls | Direct and indirect calls; scalar and vector parameters/results with `zext`/`sext`; any number of arguments (stack overflow area); calling variadic functions; `agg` parameters and results with SysV classification, including MEMORY class and sret (§5) | More than one result; `agg` with vector, i128 or f80 fields, or align above 8 (other than the `__int128` form, §5), at an external boundary or through `...` | Pass such structs by pointer. |
| Variadic functions | Definitions, local or exported; `va_start`, `va_arg` of integers of any width, `ptr`, `f32` and `f64` from registers and the stack; structs by value through `...` both ways (`agg(...)` call arguments, `va_arg agg(...)`), including MEMORY class and structs that no longer fit the registers left; reading a `va_list` that C passed in, and the reverse | `va_arg` of an `agg` with vector, i128 or f80 fields, or align above 8 (other than the `__int128` form, §5) | Pass such structs by pointer. |
| Control flow | jump br switch ret unreachable trap; `indirect_br` and `blockaddr` (also in global initializers; lowered to a `switch`: a block address is a small token, unique in the module, not a code address; equality and `&&a - &&b` offset arithmetic behave as with real addresses; the machine-IR backend, `fas-fir --mir`, uses real code addresses instead) | Comparing a block address with a code or data address (they may coincide numerically) | — |
| Globals | Every linkage and visibility, `readonly`, `extern` (also weak: address 0 when undefined), `bytes`/`zeros`/`reloc ... w8` | `reloc ... w4`; `section`; `thread_local` (refused, TLS is planned for v0.5) | Use w8; for TLS, a pointer to per-thread storage passed explicitly. |
| Symbols | `weak` and `hidden`/`protected` on functions and globals, defined or not; `alias` of functions and globals (with an addend) | — | — |
| Startup | `@main` () or (i32, ptr) → i32; static executable; `-c` objects | Shared libraries, libc | Use syscalls. |

Each gap that closes moves from the right column to the left, and `test/fas_fir/` gains a program for it.

## 13. The `fas-fir` tool

```
fas-fir INPUT.fir [-o OUT] [OBJECTS...]      build a static executable (default a.out)
fas-fir -c INPUT.fir [-o OUT.o]              compile to an object file
fas-fir --check INPUT.fir                    parse + verify, print the canonical text
fas-fir --legalize INPUT.fir                 also split wide/odd integers (Fir_wide), print the result
```

- **Input:** `INPUT` may be `-` to read standard input.
- **Installation:** `tools/fas_fir/install.sh` builds and installs `fas-fir` together with `fas-native`, the Fas compiler with `--backend=native` always on, into `~/.local/bin`. The binaries and `std/` are copied to `~/.local/share/fas-native`. Rerun the script after pulling changes.
- **Using `std/sys.fas` from Fas:** the syscall stdlib must be reached through a relative `use` path, since absolute paths are rejected. Link the installed copy into your project with `ln -s ~/.local/share/fas-native/std std`, then write `use "std/sys.fas"`.
- **Seeing what Fas emits:** `FAS_FIR=check FAS_FIR_DUMP=out.fir fas-native prog.fas -o prog` writes the Fir of a Fas program. That file compiles again with `fas-fir out.fir`.

## 14. In-memory representation (for the OCaml side)

Each function holds dense tables (`fir.ml`):

- values (definition + type);
- instructions (op + result values);
- blocks (parameters, instruction ids, terminator);
- interned constants.

Replace-all-uses is O(1) through `Alias` definitions, which readers resolve and `compact` removes. Use lists, dominators (`fir_dom.ml`) and loops are computed on demand. Nothing is global, so functions can be compiled independently.

`Fir_of_ir` and `Fir_to_ir` convert from and to the older LLVM-shaped `Ir`. Today the native backend still consumes `Ir`, through `Fir_to_ir`, and that path is the source of most limits in §12. They go away when native codegen reads Fir directly.
