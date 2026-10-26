# Fas

Fas is a small native language for hot loops, crypto, SIMD and explicit systems code. It is not a general-purpose language: there is no heap or string type, and C is the ecosystem, so you `use` C headers and call them directly. Scalar `f32` and `f64` follow IEEE arithmetic, integer arithmetic wraps, lossless widening is implicit, and the compiler adds no allocation, copies or runtime of its own. It emits LLVM IR and links through Clang.

Every example below is a complete program that compiles and prints exactly the output shown. Fas targets x86-64 Linux, x86-64 Windows and WebAssembly (WASI) with LLVM 22. The [cheat sheet](#cheat-sheet) at the end lists every form, type, operator and builtin.

```sh
fas hello.fas -o hello && ./hello       # FAS_CC, FAS_OPT, FAS_LLC pick clang-22, opt-22, llc-22
fas run hello.fas                       # build, run, delete; program arguments go after --
```

```fas
use "C" <stdio.h>

fn main() i32 {
    printf(c"hello, %s!\n", c"fas")
    return 0
}
```
```text
hello, fas!
```

## Contents

- [Coming from C](#coming-from-c)
- [Values](#values)
- [Floating point](#floating-point)
- [Functions and control flow](#functions-and-control-flow)
- [Structs, arrays and memory](#structs-arrays-and-memory)
- [Generics](#generics)
  - [Const loops](#const-loops)
- [Globals and constants](#globals-and-constants)
- [SIMD](#simd)
- [Talking to C](#talking-to-c)
  - [Headers next to your file](#headers-next-to-your-file)
  - [Libraries with pkg-config](#libraries-with-pkg-config)
  - [C structs by value](#c-structs-by-value)
  - [Assembly](#assembly)
  - [Building a C library in Fas](#building-a-c-library-in-fas)
- [Multiple files](#multiple-files)
- [Known limits](#known-limits)
- [Cheat sheet](#cheat-sheet)
  - [Forms](#forms)
  - [Types](#types)
  - [Operators](#operators)
  - [Conversions](#conversions)
  - [Places through `addr`](#places-through-addr)
  - [Builtins](#builtins)
  - [Builtin cost by CPU level](#builtin-cost-by-cpu-level)
  - [C to Fas](#c-to-fas)
  - [Compiler](#compiler)
  - [Editors](#editors)

## Coming from C

1. **A declaration is `name Type = value`**: `x u32 = 5`. `var` is only for globals.
2. **Lossless integer widening is implicit**, like C's lossless integer conversions: a value can widen when every value of its type fits the destination. Fas never implicitly narrows or changes signedness. Unlike C's integer promotions, `a * b` stays in `u16` when both operands are `u16`. `zext`, `sext` and `trunc` change width explicitly, and `bitcast` reinterprets the same bits. There are no casts.
3. **Untyped literals adapt.** A literal takes the type of its typed peer or destination, otherwise `i32`. A literal that doesn't fit is an error.
4. **Conditions are `bool`**: `if n != 0`, never `if n`. `!`, `&&` and `||` take only `bool`.
5. **Integer arithmetic wraps.** Division by zero and `MIN / -1` trap. Shift counts are taken modulo the bit width.
6. **`&`, `^` and `|` bind tighter than comparisons**, unlike C: `x & 1 != 0` means `(x & 1) != 0`.
7. **Assignment is a statement.** No `x++`, no `a = b = c`, no assignment inside a condition.
8. **Arrays and structs are places, not values.** They are not passed, returned or assigned by value: pass `&x` and use `copy(dst, src)`. Calling a C function that takes or returns a struct by value [still works](#c-structs-by-value).
9. **`addr` is an untyped byte address.** `p + n` moves `n` bytes, `p[T]` is the `T` at `p`, `p[T, i]` is element `i`. Nothing is bounds-checked.
10. **Strings are addresses.** `c"..."` is NUL-terminated. A plain `"..."` passed to an imported C `char *` parameter is an error when no NUL remains, so use `c"..."` there. A later integer length parameter, as in `strncmp`, permits a plain string.
11. **Generic calls infer their arguments from parameter types**: `max(x, 1)` infers `T` from `x`, and the literal takes that type. Inference covers `T`, `handle[T]`, `vec[N, T]` and generic structs behind handles. Every generic parameter must be determined by an argument. There is no inference from literals alone or from the result type. Explicit forms such as `max[u32](x, 1)` always work.
12. **Reading a provably uninitialized local is an error.** `{}` zero-fills an array or struct.
13. **Not in Fas:** heap, strings, threads and atomics, methods, overloading, macros, enums, unions, function types and closures. Function addresses use `addr` and `call_addr`.

**Why there is no typed pointer.** Fas has no `T *` and no `ptr[T]` or `addr[T]`, and this is deliberate. In Fas the type sits on each access (`p[u32, i]`), not on the address.

- **A pointee type is a claim that goes stale.** `uint32_t *p` says "a `u32` lives here" for every later use, even after the memory is reused or read as something else. A type on the access is true where it is written.
- **It would give LLVM facts nobody proved.** In C the pointee type implies alignment (`*p` on a `uint32_t *` assumes 4 bytes) and strict aliasing (an `int *` and a `float *` never overlap). Fas never emits alignment, aliasing or `inbounds` facts the source did not prove. So a raw access may be unaligned, and accesses of different types may overlap.
- **One memory model instead of two.** Without typed pointers there is no `const` pointee, no dereference operator or `.*` chains, no arithmetic scaled by element size next to byte arithmetic, and no pointer casts just to read memory as another type. Address arithmetic is always in bytes.
- **`handle[T]` is not a typed pointer.** It is the one typed address form. It names an `opaque` object, a C record or a specialized generic struct, and it has no arithmetic. It exists to stop mixing up `FILE` and `DIR`, not to describe memory. An `addr[T]` would grow back into `ptr[T]`, so it is rejected.

Use `view r = p[Box]` to name a typed place once and reuse it.

## Values

```fas
use "C" <stdio.h>

fn fnv1a(s addr) u32 {                          // 32-bit FNV-1a hash of a C string
    h u32 = 0x811C9DC5
    for i usize = 0; s[u8, i] != 0; i += 1 {
        h = (h ^ s[u8, i]) * 0x0100_0193            // u32 multiply wraps
    }
    return h
}

fn main() i32 {
    a u8 = 200
    b u8 = a + 100                              // 44: wrapped
    wide u32 = zext[u32](a) + 100               // 300: widened first
    neg i8 = -5
    bits u32 = bitcast[u32](sext[i32](neg))     // fffffffb
    big u64 = 1 << 40                           // the literal 1 is u64 here
    mask u16 = 0b1010_0000 | 0xF                // also 0o17
    ok bool = a > 100 && 'A' == 65              // 'A' is the integer 65
    printf(c"%u %u %x %llu %x %d\n", b, wide, bits, big, mask, ok)
    printf(c"%08x %x %u %u\n", fnv1a(c"fas"), rotl(bits, 8), popcount(bits), clz(wide))
    return 0
}
```
```text
44 300 fffffffb 1099511627776 af 1
bdf81b81 fffffbff 31 23
```

Smaller integers widen on their own when nothing can be lost. `u64` and `usize` are interchangeable, as are `i64` and `isize`. Signedness changes are always explicit:

```fas
use "C" <stdio.h>

fn main(argc i32, argv addr) i32 {
    for i i32 = 1; i < argc; i += 1 {           // argv[addr, i] takes any integer index
        printf(c"%s\n", argv[addr, i])
    }
    n usize = zext[usize](argc)                 // i32 -> usize: explicit, the sign could change
    total u64 = n * 8                           // usize -> u64: no conversion needed
    delta i64 = -3
    back isize = delta                          // i64 -> isize: also free
    as_bits usize = bitcast[usize](delta)       // signed -> unsigned: explicit
    printf(c"%llu %zd %zx\n", total, back, as_bits)
    return 0
}
```
```text
8 -3 fffffffffffffffd
```

A literal must fit its type: `x u8 = 256`, `x u32 = -1` and `ok bool = 1` are errors. Write `0xFFFFFFFF` for all ones and `true`/`false` for booleans.

`>>` is logical on unsigned values and arithmetic on signed ones. `&`, `^` and `|` bind tighter than comparisons, so `x & 1 == 0` means `(x & 1) == 0`, and chains like `a < b < c` are rejected.

## Floating point

Scalar `f32` and `f64` use IEEE 754 binary32 and binary64. Decimal literals round directly to the type they receive from a destination or typed peer. An untyped float literal defaults to `f64`. Write a decimal point or exponent for a float literal: `2.0` is a float, while `2` stays an integer. Integer literals do not silently become floats.

The supported operations are `+`, `-`, `*`, `/`, unary `-`, comparisons and the scalar builtins `sqrt`, `fma`, `floor`, `ceil`, `round` and `abs`. `round` ties to even, so `round(2.5)` is `2.0` and `round(-0.5)` is `-0.0`. `fma` rounds once, and `abs` clears the sign bit, including on NaNs. Builtin arguments use one shared float type, with untyped literals taking that type. A plain `f32` value widens exactly to `f64`. An `f32` arithmetic, unary or builtin result such as `x * x` or `sqrt(x)` remains `f32`, also when you pick a vector lane from it, so explicitly convert an operand before an operation when you need the wider calculation. `cvt[T](x)` handles integer-to-float, float-width, and saturating float-to-integer conversions. Float-to-integer conversion truncates toward zero, and NaN becomes zero. Subnormals are preserved, and separate multiply and add stay separate operations.

`vec[N, f32]` and `vec[N, f64]` support lane-wise arithmetic, comparisons, unary `-`, the float builtins above, `select`, `splat`, lane indexing, constants and memory access, plus the data-movement builtins (`shuffle`, `permute`, `compress`, `expand`, masked loads and stores, gathers and scatters). `cvt` converts between integer and float vectors of the same lane count with the scalar rules, and equal-size vectors can be `bitcast`. Float reductions, `%`, bitwise operations and shifts are rejected, and float vectors never widen implicitly.

```fas
use "C" <stdio.h>

fn main() i32 {
    single f32 = 0.1
    sum f32 = single + 0.2
    wide f64 = single
    printf(c"%08x %016llx\n", bitcast[u32](sum), bitcast[u64](wide))
    return 0
}
```
```text
3e99999a 3fb99999a0000000
```

C `float` and `double` map to `f32` and `f64` everywhere: functions, globals, typedefs and fields, in both directions. A C variadic function such as `printf` takes `f64`. An `f32` argument is rejected, so write `cvt[f64](x)`. `long double`, `_Complex`, `_Float16` and `_Float128` are not supported.

## Functions and control flow

```fas
use "C" <stdio.h>

fn collatz(n u64) u32 {
    steps u32 = 0
    while n != 1 {                              // parameters are mutable locals
        n = if n % 2 == 0 { n / 2 } else { 3 * n + 1 }
        steps += 1
    }
    return steps
}

fn digit(c u8) u32 {
    switch c {                                  // several constants per case, no fallthrough
        case 'I', 'i': return 1
        case 'V': return 5
        case 'X': return 10
        case 'L': return 50
        case 'C': return 100
        default: return 0
    }
}

fn roman(s addr) u32 {
    total u32 = 0
    for i usize = 0; s[u8, i] != 0; i += 1 {
        d u32 = digit(s[u8, i])
        if d < digit(s[u8, i + 1]) {            // IV, XL: subtract
            total -= d
            continue
        }
        total += d
    }
    return total
}

fn main() i32 {
    printf(c"%u %u\n", collatz(27), roman(c"CXLIV"))
    values: for i i32 = 0; i < 100; i += 7 {
        if i == 14 { continue values }
        if i > 30 { break values }
        printf(c"%d ", i)
    }
    printf(c"\n")
    {
        defer { printf(c"registered first, runs last\n") }
        defer { printf(c"runs first\n") }
        printf(c"body\n")
    }
    return 0
}
```
```text
111 144
0 7 21 28 
body
runs first
registered first, runs last
```

- `defer { ... }` runs when its scope exits (end of block, `return`, `break`, `continue`), last registered first.
- `break` and `continue` target the innermost loop, even inside a `switch`. `break name` and `continue name` target a surrounding labeled loop.
- **Function addresses.** `&f` is the `addr` of any non-generic function, usable at runtime and in constant tables. `call_addr[R](p, args...)` calls through it with the C ABI, where `R` is the result type. A call through an address the compiler can prove wrong (null, not a function, wrong signature) is an error. Any other address is trusted.

```fas
use "C" <stdio.h>

fn twice(x i32) i32 { return 2 * x }
fn negate(x i32) i32 { return -x }

const OPS arr[2, addr] = {&twice, &negate}     // a dispatch table

fn main() i32 {
    for i usize = 0; i < len(OPS); i += 1 {
        printf(c"%d ", call_addr[i32](OPS[i], 21))
    }
    printf(c"\n")
    return 0
}
```
```text
42 -21 
```
- Statements end at a newline or `;`. A function without a result is declared `void`. Every path of any other function must `return`.
- A long expression continues on the next line when the line ends in a binary operator, or while inside `(` `)`. A line that starts with an operator never joins the previous one.
- An expression statement must be a call. A call's result may be dropped, but any other unused expression is an error.

```fas
fn inside(x i32, lo i32, hi i32) bool {
    return x >= lo &&
        x < hi
}

fn main() i32 {
    total i32 = 1 +
        2 +
        3
    inside(total, 0, 10)
    if inside(total, 0, 10) { return total - 6 }
    return 1
}
```

## Structs, arrays and memory

```fas
use "C" <stdio.h>

const BUFFERSIZE usize = 1024

struct Point { x i32, y i32 }

struct Box @align(16) {
    corners arr[2, Point]
    name arr[4, u8]
}

fn area(b addr) i32 {                           // aggregates travel by address
    view r = b[Box]                             // names that storage, no copy
    return (r.corners[1].x - r.corners[0].x) * (r.corners[1].y - r.corners[0].y)
}

fn main() i32 {
    words arr[BUFFERSIZE / sizeof[i32], i32] = {}
    box Box = {{{1, 2}, {4, 6}}, {'b', 'o', 'x', 0}}
    other Box = {}                              // every byte zero
    copy(other, box)                            // whole-place copy
    other.corners[1].x = 11
    printf(c"%d %d %s\n", area(&box), area(&other), &other.name)
    printf(c"%zu %zu %zu\n", sizeof[Box], alignof[Box], offsetof[Box, name])

    bytes arr[8, u8] = {1, 2, 3, 4, 5, 6, 7, 8}
    p addr = &bytes
    printf(c"%x %x %x\n", p[u32], p[u32, 1], (p + 2)[u16])   // little-endian
    p[u16, 3] = 0xBEEF                          // stores work the same way
    printf(c"%x %x %zu\n", bytes[6], bytes[7], len(bytes))
    return 0
}
```
```text
12 40 box
32 16 16
4030201 8070605 403
ef be 8
```

- **Initializers** are positional and list every field and element, or name fields with `{.field = value}` in any order and leave the rest zero, padding included. `{}` means all zero. One brace list cannot mix named and positional entries.
- **Size expressions** may use constant arithmetic and layout queries, for example `arr[BUFFERSIZE / sizeof[i32], i32]`. A length is a `usize`, like `const N usize = ...`.
- **Places through an `addr`**: `p[T]`, `p[T, i]` and `p[S].field`. They use alignment 1, are never bounds-checked, and different types may alias the same bytes.
- **`view v = place`** names existing storage: a local, field, element or raw place.
- **`view xs = p[T, ..]`** is an element view of unknown length: `xs[i]` is exactly `p[T, i]`, and `&xs[i]` is its address. Use it to name a buffer from C once:

```fas
use "C" <stdio.h>
use "C" <stdlib.h>

fn main() i32 {
    n usize = 5
    buf addr = malloc(n * sizeof[u32])
    if buf == null { return 1 }
    defer { free(buf) }
    view squares = buf[u32, ..]                 // no length, no bounds check
    for i usize = 0; i < n; i += 1 { squares[i] = trunc[u32](i * i) }
    printf(c"%u %u\n", squares[2], squares[4])
    return 0
}
```
```text
4 16
```
- **`@align(N)`** raises a struct's alignment. It is the only annotation.
- **`opaque Name`** declares a type you only hold as `handle[Name]`, a typed address with no arithmetic. It is mostly for C types like `FILE`.

## Generics

```fas
use "C" <stdio.h>

struct Ring[N const usize] {
    head usize
    count usize
    items arr[N, u32]
}

fn push[N const usize](r addr, x u32) void {
    view q = r[Ring[N]]
    q.items[(q.head + q.count) % N] = x
    if q.count < N {
        q.count += 1
    } else {
        q.head = (q.head + 1) % N               // full: overwrite the oldest
    }
}

fn max[T](a T, b T) T {
    return if a > b { a } else { b }
}

fn double[SHIFT const bool](x u32) u32 {
    if SHIFT {                                  // decided per specialization
        return x << 1
    }
    return x * 2
}

fn main() i32 {
    r Ring[4] = {}
    for x u32 = 1; x <= 6; x += 1 {
        push[4](&r, x)
    }
    newest usize = (r.head + r.count - 1) % 4
    printf(c"oldest %u, newest %u, %zu bytes\n", r.items[r.head], r.items[newest], sizeof[Ring[4]])
    printf(c"%d %u %u\n", max[i32](-3, 2), max[u8](200, 7), double[true](21))
    return 0
}
```
```text
oldest 3, newest 6, 32 bytes
2 200 42
```

Type parameters are `[T]`, const parameters `[N const usize]` (any integer type, or `bool`). Each distinct argument list is compiled once. An `if` whose condition is decided by the const parameters keeps only the selected branch in that specialization. If the kept branch always returns (or breaks or continues), the statements after it are dropped from that specialization too, so `double[true]` never compiles `return x * 2`, and recursive dispatch such as `if C == 1 { return leaf[N](x) }` creates no instances for the dropped calls.

### Const loops

`for const i T in A..B { ... }` writes the body out once for each value `A`, `A + 1`, ... `B - 1`, as straight-line code: there is no runtime loop, and in each copy `i` is a constant. Use it where you would otherwise copy a block by hand or write a recursive generic: unrolled kernels, per-lane or per-shift code, a dispatch over const instances.

```fas
use "C" <stdio.h>

fn spin[R const u32](x u32) u32 {
    return (x << R) | (x >> (32 - R))
}

fn mix(x u32) u32 {
    for const i u32 in 0..4 {
        x = spin[i * 5 + 3](x) ^ (x + i)         // each copy calls its own spin
    }
    return x
}

fn sum[N const usize](xs addr) u64 {
    t u64 = 0
    for const i usize in 0..N {                 // bounds may use const parameters
        t += xs[u64, i]
    }
    return t
}

fn shift_by(x u64, s u32) u64 {
    for const k u32 in 1..4 {
        if s == k { return x << k }             // runtime dispatch to constant shifts
    }
    return x
}

fn main() i32 {
    xs arr[4, u64] = {1, 2, 3, 4}
    for const i usize in 0..3 {
        if i == 0 { continue }                  // skips the rest of this copy
        widths arr[i, u8] = {}                  // `i` sizes an array
        printf(c"copy %zu: %zu bytes\n", i, len(widths))
    }
    printf(c"%08x %llu %llu\n", mix(1), sum[4](&xs), shift_by(5, 2))
    return 0
}
```
```text
copy 1: 1 bytes
copy 2: 2 bytes
25114d8a 10 20
```

- `T` is the index type, any integer type. `A` and `B` are compile-time constants of that type: constants, `len`, `sizeof` and const parameters all work, but a runtime value does not. The range is half-open, so `A >= B` writes no copies (the body is then only name-checked, like a generic nobody calls), and `B` must itself fit `T`: `for const i u8 in 0..256` is out of range, use `u16`.
- In each copy `i` can size arrays, be a generic argument (`f[i](x)`, `spin[i * 5 + 3]`), index constants, and decide an `if`, which keeps only the taken branch in that copy, the same pruning as in generics.
- Each copy is its own scope: locals declared in the body are fresh in every copy and nothing leaks out, and `i` is gone after the loop. `i` may not reuse the name of a local or of an enclosing constant index.
- `continue` skips the rest of the current copy. `break` is an error, since there is no loop to leave, but labeled `break name` / `continue name` to an enclosing runtime loop work. A `for const` loop cannot itself have a label.
- An error inside a copy names the iteration, and an error that does not depend on `i` is reported once.
- One loop may write at most 4096 copies, and every copy counts toward the program's expanded-node budget, so nesting cannot blow up silently.

<!-- error -->
```fas
fn f(x u32) u32 {
    for const i u32 in 0..5 {
        if i == 3 { x = x / (i - 3) }
    }
    return x
}
```
```text
note: in iteration `i = 3`
```

This is the replacement for unroll hints: if you want a loop unrolled, write it as a `for const`. Otherwise the optimizer decides.

## Globals and constants

```fas
use "C" <stdio.h>

struct Item { name addr, price u32 }

const TAX u32 = 1 << 3                          // scalar constant
const MENU arr[3, Item] = {                     // read-only table
    {c"tea", 3},
    {c"cake", 5},
    {c"soup", 7}
}

var orders u32                                  // global, starts zeroed
var history arr[4, u32] = {}

fn order(i usize) u32 {
    history[orders % 4] = MENU[i].price
    orders += 1
    return MENU[i].price + TAX
}

fn main() i32 {
    for i usize = 0; i < len(MENU); i += 1 {
        printf(c"%-5s %u\n", MENU[i].name, order(i))
    }
    printf(c"%u orders, last %u\n", orders, history[2])
    return 0
}
```
```text
tea   11
cake  13
soup  15
3 orders, last 7
```

- Top-level initializers are compile-time values, emitted as data: no code runs before `main`.
- An `addr` slot in a constant table may hold `c"..."`, `null`, `&GLOBAL`, `&GLOBAL.field`, `&GLOBAL[k]`, or `&f` for any non-generic Fas function.
- A `const` of struct type, an array of structs or nested arrays (`const GRID arr[2, arr[3, i32]] = {{1, 2, 3}, {4, 5, 6}}`) is read-only static data, whether or not it holds addresses. Index and read it at run time. `len` works in constants, but another constant's initializer cannot read its slots.
- Globals are private to the program unless declared inside `extern "C" { }`.

## SIMD

```fas
use "C" <stdio.h>
use "C" <string.h>

const LANES vec[16, u8] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15}

fn upper(s addr, n usize) void {                // ASCII upper-case, 16 bytes per step
    for i usize = 0; i < n; i += 16 {
        left u8 = if n - i < 16 { trunc[u8](n - i) } else { 16 }
        live vec[16, bool] = LANES < splat(left)        // false past the end
        v vec[16, u8] = masked_load[u8](s + i, live, splat(0))
        lower vec[16, bool] = (v >= splat('a')) & (v <= splat('z'))
        masked_store[u8](s + i, live, select(lower, v - splat(32), v))
    }
}

fn count_above(xs addr, n usize, limit u32) u32 {
    count u32 = 0
    for b usize = 0; b < n / 8; b += 1 {
        hits vec[8, bool] = xs[vec[8, u32], b] > splat(limit)   // block b: 8 lanes
        count += popcount(bitcast[u8](hits))                       // 8 bool lanes = 1 byte
    }
    for i usize = n / 8 * 8; i < n; i += 1 {   // scalar tail
        if xs[u32, i] > limit { count += 1 }
    }
    return count
}

fn main() i32 {
    v vec[4, u32] = {10, 20, 30, 40}
    w vec[4, u32] = v * splat(2) + splat(1)
    big vec[4, bool] = w > splat(30)
    r vec[4, u32] = shuffle(w, w, {3, 2, 1, 0})
    k vec[4, u32] = compress(w, big)
    printf(c"w %u %u %u %u, reversed %u %u %u %u, compressed %u %u %u %u\n",
        w[0], w[1], w[2], w[3], r[0], r[1], r[2], r[3], k[0], k[1], k[2], k[3])
    printf(c"sum %u, max %u, any %d, all %d, sat %u\n",
        reduce_sum(w), reduce_max(w), any(big), all(big), add_sat(w, splat(0xFFFFFFF0))[0])

    text arr[32, u8] = {}
    strcpy(&text, c"hello, vector world")
    upper(&text, strlen(&text))
    data arr[20, u32] = {}
    for i usize = 0; i < 20; i += 1 {
        data[i] = trunc[u32](i * 7)
    }
    printf(c"%s, %u above 50\n", &text, count_above(&data, 20, 50))
    return 0
}
```
```text
w 21 41 61 81, reversed 81 61 41 21, compressed 41 61 81 0
sum 204, max 81, any 1, all 0, sat 4294967295
HELLO, VECTOR WORLD, 12 above 50
```

- **`vec[N, T]`** holds integer or `bool` lanes, up to 256 lanes and 2048 bits. Operators work lane by lane, and comparisons give `vec[N, bool]`.
- **`splat(x)` and `{...}`** take their vector type from the destination or a typed vector peer.
- **Masks** combine with `& | ^ !`. `select`, `any`, `all` and `popcount(bitcast[u8](mask))` consume them.
- **Memory**: `p[vec[N, T]]` loads and stores exactly `N × sizeof(T)` bytes at alignment 1. The masked, gather and scatter forms are in the [builtin table](#builtins).

## Talking to C

C headers, integer and float macros, structs and `static inline` functions are imported directly. A `use "C" <<TAG` block embeds C source that is compiled with your program.

```fas
use "C" <stdio.h>
use "C" <stdlib.h>
use "C" <string.h>
use "C" <<C
#define NAME_MAX_LEN 16
struct user { int id; char name[NAME_MAX_LEN]; };
C

extern "C" {
    fn add(a i32, b i32) i32 { return a + b }   // a body inside extern "C" exports it
    fn compare(a addr, b addr) i32 {
        x i32 = a[i32]
        y i32 = b[i32]
        return if x < y { -1 } else if x > y { 1 } else { 0 }
    }
}

fn main() i32 {
    u user = {}                                 // a C struct is an ordinary Fas struct
    u.id = 42
    strncpy(&u.name, c"ada", zext[usize](NAME_MAX_LEN - 1))
    printf(c"%s #%d, %d\n", &u.name, u.id, call_addr[i32](&add, 2, 3))

    nums arr[5, i32] = {5, -2, 9, 0, 3}
    qsort(&nums, len(nums), sizeof[i32], &compare)   // &f of an exported function
    printf(c"%d %d %d\n", nums[0], nums[2], nums[4])

    buf addr = malloc(32)
    if buf == null { return 1 }
    defer { free(buf) }
    memset(buf, '.', 31)
    buf[u8, 31] = 0
    printf(c"%zu\n", strlen(buf))
    return 0
}
```
```text
ada #42, 5
-2 3 9
31
```

- **Linkage.** Inside `extern "C" { }`, a `fn` without a body imports a C function and one with a body exports it. A `var` without an initializer imports a C global, and one with an initializer exports it. Everything outside is private.
- **Function pointers** are `addr`. `&f` takes the address of a non-generic Fas function, and `call_addr[R](p, args...)` calls it with the C ABI. Unknown function addresses are accepted without runtime checks.
- **Variadics** (`printf`) promote small integers and `bool` to `int`, as in C.
- **C strings.** A plain `"..."` passed to an imported C `char *` parameter is rejected when no NUL byte remains from that address to the end. Use `c"..."`. A later integer length parameter, as in `strncmp`, is allowed. Variadic arguments such as `printf`'s `%s` value are not checked by this rule.
- **Macro names.** A Fas declaration with the same name as an imported C macro is a duplicate declaration, just like one clashing with an imported function.
- **`errno`** is a function-like macro, so it is invisible. Write `__errno_location()[i32]`.
- **Feature-test macros** (`_GNU_SOURCE`, `_FILE_OFFSET_BITS`, ...) must come before any system header: put that C block first, or pass `-D_GNU_SOURCE`. Defining one after a header was read is an error. Function-like macros such as `CPU_SET` need a `static inline` wrapper in a C block.
- Importing a declaration Fas can't represent is fine, but using it is an error that names it and the reason. The [type mapping](#c-to-fas) is in the cheat sheet.

### Headers next to your file

`use "C" "name.h"` finds a header relative to the `.fas` file, and `<name.h>` finds system headers. Neither needs flags, in builds or in the editor. A single-header library (stb, a JSON parser, ...) is copied next to your file, and one C block compiles its implementation. A quoted `#include` in a block also looks next to the `.fas` file:

<!-- file: json.h -->
```c
int json_depth(const char *text);
#ifdef JSON_IMPLEMENTATION
int json_depth(const char *text) {
    int depth = 0, best = 0;
    for (; *text; text++) {
        if (*text == '{' || *text == '[') { depth++; if (depth > best) best = depth; }
        if (*text == '}' || *text == ']') depth--;
    }
    return best;
}
#endif
```

```fas
use "C" <stdio.h>
use "C" <<C
#define JSON_IMPLEMENTATION
#include "json.h"
C

fn main() i32 {
    printf(c"%d\n", json_depth(c"{\"a\": [1, {\"b\": 2}]}"))
    return 0
}
```
```text
3
```

Do this in one file of the program only, or the library's functions are defined twice. Other files use `use "C" "json.h"`.

### Libraries with pkg-config

A library installed with a `.pc` file (most of them: SDL, zlib, GTK, libpng) is named in the source with `use pkg`. Fas asks `pkg-config` for its flags, so every command (build, `fas run`, `fas check`, the editor) finds the headers and links the library without any options:

```fas
use pkg "zlib"
use "C" <zlib.h>
use "C" <stdio.h>

fn main() i32 {
    text arr[5, u8] = {'h', 'e', 'l', 'l', 'o'}
    printf(c"%lu\n", crc32(0, &text, 5))
    return 0
}
```
```text
907060870
```

- `use pkg "NAME"` sits on its own line, anywhere in the file. It imports no header: you still pick the headers with `use "C"`. Its flags apply to every `use "C"` of that file.
- The string goes to `pkg-config` as one argument, so its own forms work: `"sdl2 gl"` for two packages, `"sdl2 >= 2.26"` for a minimum version.
- Your `-I` and `-D` come before the package's. Link flags are added only when linking an executable.
- Only plain flags are accepted (`-I`, `-isystem`, `-D`, `-U`, `-L`, `-l`, `-pthread`, `-Wl,-rpath,DIR`). Anything else is an error naming the flag.
- A misspelled package is an error at its name, with similar installed packages as `help:`.
- `FAS_PKG_CONFIG` picks another tool (default `pkg-config`), and `PKG_CONFIG_PATH` works as usual.

<!-- error -->
```fas
use pkg "zlibb"
use "C" <zlib.h>
fn main() i32 { return 0 }
```
```text
Package zlibb was not found
```

### C structs by value

Many C APIs pass small structs by value: raylib's `ClearBackground(Color)`, `GetMousePosition()` returning `Vector2`. Fas calls them through a hidden C adapter that clang compiles, so the C calling convention is always clang's own. On the Fas side the struct stays a place:

```fas
use "C" <stdio.h>
use "C" <<C
typedef struct { unsigned char r, g, b, a; } Color;
typedef struct { float x, y; } Vector2;
static void show(Color c) { printf("%d %d %d %d\n", c.r, c.g, c.b, c.a); }
static Vector2 add(Vector2 a, Vector2 b) { Vector2 v = {a.x + b.x, a.y + b.y}; return v; }
C

fn main() i32 {
    bg Color = {245, 245, 245, 255}
    show(bg)                          // argument: a variable, field or array element
    a Vector2 = {1.0, 2.0}
    sum Vector2 = add(a, a)           // result: only in a declaration ...
    sum = add(sum, a)                 // ... or an assignment to a place
    printf(c"%g %g\n", cvt[f64](sum.x), cvt[f64](sum.y))
    return 0
}
```
```text
245 245 245 255
3 6
```

- A by-value argument must be a variable, field or array element of exactly that struct type. A literal such as `show({1, 2, 3, 4})` or a nested call is an error: declare a local first.
- A struct result is used only as `x S = f(...)` or `place = f(...)`. A discarded call or `f(...).x` is an error naming both forms. A declaration cannot pass its own name: `v S = f(v)` is an error. Use a new name, or assign `v = f(v)` to an existing `v`.
- This works for header and C-block functions, `static` or not, structs and unions, packed or aligned. Variadic functions with struct arguments stay unsupported. `&f` of such a function is an error, because Fas calls the adapter, not `f`.
- Fas functions still never take or return structs by value. This is only for calling C.

### Assembly

An assembly unit is linked in, but it declares nothing to Fas. Declare its symbols in `extern "C"`. The example is x86-64 Linux only: it takes its arguments in `rdi`, `rsi` and `edx` as System V does, while Windows passes them in `rcx`, `rdx` and `r8`.

```fas
use "C" <stdio.h>
use "asm" <<S
    .text
    .globl add3
add3:
    leal (%rdi,%rsi), %eax
    addl %edx, %eax
    ret
S

extern "C" {
    fn add3(a i32, b i32, c i32) i32
}

fn main() i32 {
    printf(c"%d\n", add3(1, 2, 3))
    return 0
}
```
```text
6
```

### Building a C library in Fas

<!-- file: checksum.fas -->
```fas
extern "C" {
    fn adler32(data addr, n usize) u32 {
        a u32 = 1
        b u32 = 0
        for i usize = 0; i < n; i += 1 {
            a = (a + data[u8, i]) % 65521
            b = (b + a) % 65521
        }
        return (b << 16) | a
    }
}
```

<!-- file: main.c -->
```c
#include <stdio.h>
#include <string.h>
#include "checksum.h"

int main(void) {
    const char *s = "Wikipedia";
    printf("%08x\n", adler32((void *)s, strlen(s)));
    return 0;
}
```

<!-- cmd: $FAS --emit-header checksum.fas -o checksum.h && $FAS -c checksum.fas -o checksum.o && $CC main.c checksum.o -o app && ./app -->
```sh
fas --emit-header checksum.fas -o checksum.h    # C11/C++ header for every export
fas -c checksum.fas -o checksum.o               # object file; no main needed
cc main.c checksum.o -o app && ./app
```
```text
11e60398
```

The other direction works too: `fas app.fas util.c -o app -lz` compiles `.c` inputs and passes `.o`, `.a`, `.so`, `-l` and `-L` to the linker. `-I`, `-isystem` and `-D` reach C headers, containers and `.c` inputs.

## Multiple files

<!-- file: geom/vec2.fas -->
```fas
struct Vec2 { x i32, y i32 }

fn dot(a addr, b addr) i32 {
    return a[Vec2].x * b[Vec2].x + a[Vec2].y * b[Vec2].y
}
```

<!-- file: app.fas -->
```fas
use "geom/vec2.fas"
use "C" <stdio.h>

fn main() i32 {
    a Vec2 = {2, 3}
    b Vec2 = {4, 5}
    printf(c"%d\n", dot(&a, &b))
    return 0
}
```

<!-- cmd: $FAS app.fas -o app2 && ./app2 -->
```text
23
```

`use "path.fas"` is relative to the file that contains it. The whole `use` closure shares one namespace: there are no modules, qualifiers or visibility. Give the compiler only the root file.

## Known limits

- **Checks.** The compiler rejects only what is provably wrong. When it can't tell, it accepts. Its only runtime checks are the integer division trap and the vector lane-index trap. A trap is `ud2`, so buffered `printf` output not yet flushed is lost.
- **Platform.** x86-64 Linux, x86-64 Windows with `--os windows` (see "Building for Windows") and 32-bit WebAssembly for WASI with `--os wasi` (see "Building for WebAssembly"), with LLVM 22 tools (`clang-22`, `opt-22`, `llc-22`, `llvm-profdata-22`, or the plain names when they are LLVM 22). Portable builds target baseline x86-64 (SSE2).
- **C.** Variadic C functions cannot take or return structs or unions by value, and the address of a function that does (`&f`) cannot be taken. Records with bit-fields, flexible array members, `long double` or `_Complex` fields, or a named field of an unnamed struct type, import only as `handle[T]` (pointers work, fields and values do not), and the error says which part is the cause.
- **Tuning.** `fas perf --tune` picks one build for the whole program, not per function or loop, and does not optimize across the C boundary (no LTO).
- **Huge sources.** One expression with tens of thousands of operators, or a function with tens of thousands of locals, compiles slowly (seconds). Ordinary programs are not affected.
- **Command line.** Options go before the subcommand word: `fas run -O0 file.fas`, not `fas -O0 run file.fas`.

## Cheat sheet

### Forms

<!-- skip -->
```fas
// top level
use "util.fas"                          // another Fas file, relative to this one
use "C" <stdio.h>                       // C header from the system path
use "C" "lib.h"                         // C header relative to this file
// C source, up to a line holding only the tag; nothing may follow <<C
use "C" <<C
static inline int twice(int x) { return 2 * x; }
C
use "asm" "start.S"                     // assembly unit; also use "asm" <<TAG

const N usize = 64                      // compile-time scalar
const TABLE arr[3, u8] = {1, 2, 3}      // read-only static table
const MASK vec[4, u32] = {1, 0, 1, 0}
var count u32                           // mutable global, zero-filled
var seed u32 = 7

struct P { x i32, y i32 }               // fields separated by `,` or newlines
struct Line @align(64) { bytes arr[64, u8] }
struct Ring[N const usize] { head usize, items arr[N, u32] }
opaque FILE                             // held only as handle[FILE]

fn f(a u32, p addr) u32 { return a }
fn g() void { }
fn max[T](a T, b T) T { return if a > b { a } else { b } }
fn sum[T, N const usize](xs addr) T { ... }

extern "C" {
    fn puts(s addr) i32                 // no body: import
    fn api(x u32) u32 { return x }      // body: export with the C ABI
    var errors i32                      // no initializer: import
    var version u32 = 3                 // initializer: export
}

// in a function body
x u32                                   // uninitialized
y u32 = 5
a arr[4, u8] = {1, 2, 3, 4}             // positional, every element
words arr[BUFFERSIZE / sizeof[i32], i32] = {}
s P = {}                                // all zero
v vec[4, u32] = splat(7)
t vec[4, u32] = {1, 2, 3, 4}   // compound literal, usable as a value
view r = s.x                            // alias of a place
y += 1                                  // also -= *= /= %= &= |= ^= <<= >>=
copy(a, b)                              // whole-place copy of an array or struct
if c { } else if d { } else { }
while c { }
for i usize = 0; i < n; i += 1 { }
for const i u32 in 0..4 { }               // one straight-line copy per value
switch x {
    case 1, 3: f(x, null)
    case 2: { }
    default: { }
}
outer: while c { continue outer }
break  break outer  continue  continue outer  return  return x  defer { }
max(a, 1)  sum[u64, 8](&xs)             // inferred and explicit generic calls
```

### Types

| Type | Size | Align | Notes |
|---|---|---|---|
| `bool` | 1 | 1 | `true`, `false`. Stores write 0 or 1, and any nonzero byte reads as `true` |
| `u8` `i8` | 1 | 1 | `'A'` is an integer literal |
| `u16` `i16` | 2 | 2 | |
| `u32` `i32` | 4 | 4 | untyped integer literals default to `i32` |
| `f32` | 4 | 4 | IEEE binary32 scalar |
| `u64` `i64` | 8 | 8 | |
| `usize` `isize` | 8 | 8 | sizes, counts and indices, 4 bytes on WASI |
| `f64` | 8 | 8 | IEEE binary64 scalar |
| `addr` | 8 | 8 | untyped byte address or `null`, 4 bytes on WASI |
| `handle[T]` | 8 | 8 | address of an `opaque`, C record, or specialized generic struct `T`, with no arithmetic |
| `arr[N, T]` | N × stride(T) | align(T) | `len(a)` is N |
| `vec[N, T]` | N × size(T), rounded up to a power of two | = size | integer lanes, at most 256 lanes and 2048 bits. `vec[3, u32]` is 16 bytes |
| `vec[N, bool]` | ⌈N/8⌉ bytes, rounded up to a power of two | = size | packed bits, lane 0 is the low bit |
| `struct` | C layout | max field | `@align(N)` raises it |
| `void` | | | result type only |

### Operators

Tightest first. Binary operators are left-associative. If-expressions have the lowest precedence, so parenthesize one before applying another operator.

| | Operators | Notes |
|---|---|---|
| 1 | `f(x)` `a[i]` `s.f` | call, index, field |
| 2 | `-x` `!x` `~x` `&place` | float `-` flips its sign bit, `!` takes `bool` or `vec[N, bool]`, `&` gives `addr` |
| 3 | `*` `/` `%` | integers wrap, `/` and `%` trap on 0, and `/` traps on `MIN / -1`. Float `*` and `/` use IEEE arithmetic, and float `%` is rejected |
| 4 | `+` `-` | integer arithmetic wraps, float arithmetic uses IEEE rules, `addr ± n` moves bytes |
| 5 | `<<` `>>` | count modulo width, `>>` arithmetic on signed |
| 6 | `&` | also `bool` and masks |
| 7 | `^` | |
| 8 | `\|` | |
| 9 | `<` `<=` `>` `>=` | not chainable, unsigned on `addr`, IEEE comparisons on floats |
| 10 | `==` `!=` | float comparisons follow IEEE NaN and signed-zero rules |
| 11 | `&&` | `bool` only, short-circuit |
| 12 | `\|\|` | `bool` only, short-circuit |
| 13 | `if c { a } else { b }` | lowest precedence |

Integer and float vectors take arithmetic and comparisons lane by lane. Float vector arithmetic also includes unary `-` and compound assignment. Bitwise and shift operators are available only for integer vectors.

With no destination type, an if-expression unifies integer branches like mixed integer operands. If `l u16` and `o` and `off` are `u32`, `if c { l } else { o - off }` has type `u32`, and the plain `l` branch is zero-extended. A known destination keeps its existing branch conversions. Constant if-expressions use the same branch types and values. Widen arithmetic operands before the operation when its narrow result could wrap.

### Conversions

| From → to | Write |
|---|---|
| `uN` → wider `uM` or `iM`, `M > N` | implicit, zero-filled |
| `iN` → wider `iM`, `M > N` | implicit, sign-filled |
| `u32` → `usize` or `isize`, `i32` → `isize` | implicit |
| same-width signedness change | `bitcast[u32](x)` |
| `u64` ↔ `usize`, `i64` ↔ `isize` | nothing: write `x`. `zext`/`sext`/`trunc` between them is an error |
| `cvt[T]` of a `T` value | nothing: write `x`. The `cvt` is an error |
| wider → narrower, low bits | `trunc[u8](x)` |
| other integer width change | explicit `zext`, `sext` or `trunc` as appropriate |
| same bits, other type: signedness, vector ↔ integer, mask ↔ bits | `bitcast[u32](x)`, `bitcast[u8](mask8)` |
| `bool` → integer | `zext[u32](b)` |
| integer → `bool` | `x != 0` |
| integer → `f32` or `f64` | `cvt[f32](x)` or `cvt[f64](x)`, rounding to nearest, ties to even |
| `f32` → `f64` | implicit exact widening for a plain value, or `cvt[f64](x)` |
| `f64` → `f32` | `cvt[f32](x)`, rounding to nearest, ties to even |
| float → integer | `cvt[i32](x)`, truncating toward zero and saturating, with NaN becoming 0 |
| `f32` ↔ `u32`/`i32`, `f64` ↔ `u64`/`i64` | `bitcast[T](x)` |
| `addr` ↔ `usize` | `addr_bits(p)`, `addr_from_bits(n)` |
| `addr` ↔ `handle[T]` | `handle_from_addr[T](p)`, `handle_addr(h)` |

On 64-bit targets `u64` and `usize` convert to each other implicitly in both directions, and so do `i64` and `isize`. Mixed operands take `usize` or `isize`. Signedness changes stay explicit. `bool`, vectors, `addr` and `handle[T]` never widen implicitly. Mixed integer operands widen the narrower value when one type holds the other (`u32 < i64` compares as `i64`), while `u32` with `i32` and `u64` with `i64` have no common type. An untyped literal takes the type of the other operand or branch (`w < 1` and `if c { 1 } else { w }` are `u32` for `w u32`). With nothing to take a type from, it is `i32`.

**Widen before the arithmetic, not after.** A result that may wrap never widens implicitly: with `a u16` and `b u16`, `x u32 = a * b` is rejected because the product may already have wrapped in `u16`. Write `x u32 = zext[u32](a) * b`. This applies to `+`, `-`, `*`, `<<`, `~`, unary minus and signed `/` and `%`, wherever the result meets a wider type: a destination, a mixed-width operand (`w + zext[u32](b) * 3`) or a comparison (`x == zext[u64](a) + 1`). Results that cannot wrap widen like plain values: `&`, `|`, `^`, `>>` and unsigned `/` and `%`, so `x u32 = (a >> 4) & 0xf` needs no conversion. When the compiler rejects a narrow result, its `help:` shows a version that compiles. `zext`, `sext` and `trunc` need a strict width change and work lane by lane on vectors, and `bitcast` needs the same total bits.

### Places through `addr`

| Form | Meaning |
|---|---|
| `&x` `&s.f` `&a[i]` | address of a local, global, field or element |
| `&f` | address of a non-generic Fas function, including native functions |
| `p + n` `p - n` | `n` bytes further, and a signed `n` is sign-extended |
| `p[T]` | the `T` stored at `p` (read or write) |
| `p[T, i]` | element `i`: the `T` at `p + i × stride(T)` |
| `p[S].f` `p[S].f[i].g` | a field path of the struct `S` at `p` |
| `(p + 3)[u16]` | parenthesize arithmetic before selecting |
| `view v = p[S]` | name it once, then use `v.f` |
| `view xs = p[T, ..]` | element view of unknown length: `xs[i]` is `p[T, i]`, `&xs[i]` its address |
| `p[vec[8, u32]]` | 8 lanes, exactly 32 bytes |
| `p == q` `p < q` `p == null` | unsigned address comparison |

All of these use alignment 1, are little-endian, and are never bounds-checked. Different types may read and write the same bytes.

### Builtins

`T` is a type argument, `x` is a scalar or integer/float vector where supported, `v` is an integer, bool or float vector as applicable, and `m` is a `vec[N, bool]` mask.

| | Form | Result | Note |
|---|---|---|---|
| **Conversion** | `zext[T](x)` `sext[T](x)` | `T` | widen by zero or sign |
| | `trunc[T](x)` | `T` | keep the low bits |
| | `bitcast[T](x)` | `T` | same bits, any shape |
| | `cvt[T](x)` | `T` | scalar or equal-lane-count vector conversion, rounding or saturating as appropriate |
| **Layout** | `sizeof[T]` `alignof[T]` | `usize` | |
| | `offsetof[S, field]` | `usize` | |
| | `len(a)` | `usize` | array element count, or string literal bytes without the terminator |
| **Bits** | `rotl(x, k)` `rotr(x, k)` | type of `x` | `k` any integer, taken modulo the width |
| | `popcount(x)` `clz(x)` `ctz(x)` | type of `x` | lane-wise, and `clz(0)` and `ctz(0)` give the width |
| **Arithmetic** | `add_sat(a, b)` `sub_sat(a, b)` | type of `a` | clamp to the type's range, lane-wise |
| | `mul_hi(a, b)` | type of `a` | high half of the double-width product, lane-wise |
| **Memory** | `copy(dst, src)` | statement | arrays or structs of one type, overlap is safe |
| | `volatile_load[T](p)` | `T` | scalar, `addr`, handle or vector |
| | `volatile_store[T](p, x)` | statement | never merged or removed |
| | `addr_bits(p)` `addr_from_bits(n)` | `usize`, `addr` | |
| | `handle_addr(h)` `handle_from_addr[T](p)` | `addr`, `handle[T]` | |
| | `call_addr[R](p, args...)` | `R` or `void` | indirect call through `addr`, using the C ABI |
| **SIMD values** | `splat(x)` | `vec[N, T]` | every lane `x`, with the type from context |
| | `select(m, yes, no)` | type of `yes` | lane `i` comes from `yes[i]` when `m[i]` is true, otherwise `no[i]`. Both sides are evaluated |
| | `any(m)` `all(m)` | `bool` | |
| | `reduce_sum(v)` `reduce_min` `reduce_max` `reduce_and` `reduce_or` `reduce_xor` | lane type | integer vectors, and the sum wraps |
| | `shuffle(a, b, {i, ...})` | `vec[K, T]` | lane `k` is lane `i[k]` of `a` then `b` (`0 .. 2N-1`). The constant selector's length `K` sets the width |
| | `permute(v, idx)` | `vec[K, T]` | lane `k` is `v[idx[k]]`, or 0 when `idx[k] ≥ N`. `idx` is a runtime unsigned vector of any length `K` |
| | `compress(v, m)` | type of `v` | the lanes where `m` is true, packed to the front, the rest 0 |
| | `expand(v, m)` | type of `v` | the front lanes of `v`, spread to where `m` is true, the rest 0 |
| | `v[i]` | lane | read or write one lane. A runtime index out of range traps |
| **SIMD memory** | `masked_load[T](p, m, fallback)` | `vec[N, T]` | lane `j` reads `p + j × sizeof(T)` where `m[j]`, else takes `fallback[j]` |
| | `masked_store[T](p, m, values)` | statement | writes only the lanes where `m` is true |
| | `gather[T](p, idx, m, fallback)` | `vec[N, T]` | lane `j` reads `p + idx[j] × sizeof(T)` |
| | `scatter[T](p, idx, m, values)` | statement | lanes stored in order, so the last one wins on overlap |
| | `gather_bytes[T](p, off, m, fallback)` | `vec[N, T]` | lane `j` reads `p + off[j]` bytes |
| | `scatter_bytes[T](p, off, m, values)` | statement | |

In the masked, gather and scatter forms, `T` is the lane type, every vector has the same `N`, inactive lanes touch no memory, and bool lanes take one byte each.

When every operand of `rotl`, `rotr`, `popcount`, `clz`, `ctz`, `add_sat`, `sub_sat` or `mul_hi` is an untyped literal, the literals take the destination type: `x u32 = rotl(0x80000001, 1)` is `3`, and `s u8 = add_sat(250, 10)` saturates at `255`. A typed operand still decides the type, and with no destination the literals are `i32`.

Every builtin name in this table, `cvt` and the float builtins included, is reserved: a variable, parameter, field, function or const named `sqrt` or `popcount` is an error (`` `sqrt` is a builtin and cannot be used as a name ``), and so are type names (`u8`, `f32`), literals (`true`, `null`) and `view`, `use` and `var`. A C declaration with such a name is imported but cannot be used from Fas.

### Builtin cost by CPU level

Every builtin has the same result on every CPU, but some compile to very different code. The default build targets baseline x86-64 (SSE2). LLVM then expands the builtins below into lane-by-lane scalar code where the CPU lacks a matching instruction. The compiler gives no warning, but `fas perf` shows it as a high `instr/iter` and, from its native build, a `native:` estimate. Instruction counts for one call on `vec[8, u32]` or `vec[16, u8]` (clang/LLVM 22, `-O2`):

| Builtin | x86-64 | x86-64-v2 (SSE4.2) | x86-64-v3 (AVX2) |
|---|---|---|---|
| `permute` u8×16 | 145 | 9 (`pshufb`) | 8 |
| `permute` u32×8 | 83 | 63 | 9 (`vpermd`) |
| `popcount` u64 | 21 | 3 (`popcnt`) | 3 |
| `clz` u32×8 | 80 | 49 | 23 |
| `reduce_max` u32×8 | 30 | 9 | 8 |
| `<<` by a lane vector, u32×8 | 31 | 19 | 7 |
| `*` u64×4 | 27 | 27 | 13 |
| `compress` / `expand` u32×8 | 140 / 125 | 140 / 111 | 117 / 98 |
| `gather` / `scatter` u32×8 | 86 / 76 | 69 / 59 | 77 / 67 |
| `masked_load` / `masked_store` u32×8 | 72 / 62 | 61 / 51 | 68 / 58 |

`compress`, `expand`, gathers, scatters and masked memory stay lane-by-lane at every level here: use them where the data dependence needs them, not in the innermost loop when a `select` or a plain load does the job. `shuffle` with constant lanes, `add_sat`, `reduce_sum`, `ctz` and `mul_hi` on u64 cost a few instructions everywhere.

### C to Fas

| C | Fas |
|---|---|
| `int`, `char`, `long`, `double`, `uint8_t`, `size_t`, `ssize_t`, `_Bool` | `i32`, `i8`, `i64` (`i32` on Windows and WASI), `f64`, `u8`, `usize`, `isize`, `bool` |
| enum constants, `#define N 16` | integer constants with C's type |
| `#define SCALE 0.5f`, `FLT_MAX`, `M_PI` | `f32` (`f`/`F` suffix) or `f64` constants. Only a single literal, optionally `-` and parentheses |
| `void *`, `char *`, `int *`, function pointers | `addr` |
| `struct S *` (complete or opaque `S`) | `handle[S]`, and `&s` of an imported struct converts |
| complete structs and unions | Fas record types: fields, `{}`, `sizeof`, globals, arrays |
| C `float`, `double` parameters, results, globals, typedefs | `f32`, `f64` |
| `float`, `double` record or union fields | `f32`, `f64`, with ordinary field access |
| `extern T x;` | a global place |
| `T name[N]` global | `arr[N, T]` place |
| struct or union parameters and results of C functions | a place argument, the result only in `x S = f(...)` or `place = f(...)` ([details](#c-structs-by-value)) |
| bit-fields, variadic functions with struct arguments, `long double`, `_Complex`, `_Float16`, `_Float128`, `__int128`, function-like macros | not supported |

### Compiler

```sh
fas main.fas -o app             # executable (default -O2)
fas -O0 main.fas -o app         # -O0..-O3 (default -O2)
fas -g main.fas -o app          # DWARF line tables for gdb/perf; same machine code
fas --no-inline hot main.fas -o app # keep one function out of line (any -O); repeatable
fas --no-inline dense main.fas -o app      # a generic: every instance; 'dense[3]' just one
fas run main.fas util.c -- a b  # build to a temp file, run with args a b, delete; exit code passes through
fas -c lib.fas -o lib.o         # object, no main required
fas --emit-header lib.fas -o lib.h
fas -S main.fas -o main.s       # assembly
fas --emit-llvm main.fas        # verified, unoptimized LLVM IR
fas --emit-llvm=optimized main.fas # the IR after opt, exactly what llc compiles
fas check main.fas              # errors only: no LLVM, no linking (add --diagnostics=json for tools)
fas check --all-errors main.fas # every error in function bodies at once, not just the first
fas --version                   # fas version and commit, plus the opt, llc and C compiler in use
fas --target=force-native main.fas -o app # this CPU's instructions everywhere (C inputs too); may not run elsewhere
fas --os windows main.fas -o app.exe # a Windows program (needs llvm-mingw, see below)
fas --os wasi main.fas             # a WebAssembly program, a.wasm (needs wasi-sdk, see below)
fas perf main.fas -- args       # run under perf: hot loops by Fas line, with LLVM's facts per loop
fas perf --static main.fas      # facts and llvm-mca estimates only, no run
fas perf --json main.fas > a.json; fas perf --compare a.json main.fas # what changed since a.json
fas perf --tune main.fas -- args # measure builds on this machine, keep the fastest correct one in main.fasprof
fas --profile main.fasprof main.fas -o app # build what the profile chose
fas --keep main.fas -o app      # keep temp files, print tools and paths
fas main.fas util.c -lz -o app  # C sources and link flags (executables only)
```

`--all-errors` (any command) keeps checking after an error: every function body is checked and each failing statement is reported, so leaving out all casts gives the full list of places that need one in a single run. Errors outside function bodies (parsing, types, signatures, constants) still stop at the first. Without the flag only the first error is printed.

`--target=portable` (the default) builds for baseline x86-64. `--target=force-native` builds the whole program for the CPU you compile on: faster in most loops, slower in some, and the result may crash with an illegal instruction on an older CPU. The `native:` fact in `fas perf` shows, per loop, what the native build would do, so you can decide before switching.

**Building for Windows.** `--os windows` builds a 64-bit Windows program (MinGW, `x86_64-w64-windows-gnu`), and `--os linux` builds for Linux. Without the flag `fas` builds for the system it runs on. The C side needs [llvm-mingw](https://github.com/mingw-w64/llvm-mingw) (LLVM 22): on Linux put its `bin` directory on `PATH` (fas then uses `x86_64-w64-mingw32-clang`) or set `FAS_CC`. On Windows use its `clang` with `opt` and `llc` from the official LLVM 22 release. Test from Linux with Wine:

```sh
PATH=$HOME/llvm-mingw/bin:$PATH fas --os windows main.fas   # writes a.exe
wine a.exe
```

Windows C differs in a few places: `long` and `unsigned long` are 32-bit (`i32`/`u32`, while `long long` stays `i64`), `wchar_t` is 16-bit, and the default output is `a.exe`. Fas code itself is unchanged, and `usize` is still 64-bit. `fas perf` (and `--tune`) measure Linux builds only and refuse `--os windows`.

**Building for WebAssembly.** `--os wasi` builds a 32-bit WebAssembly program for WASI (`wasm32-unknown-wasip1`) that runs in [wasmtime](https://wasmtime.dev) and other WASI runtimes. The C side needs [wasi-sdk](https://github.com/WebAssembly/wasi-sdk) (its libc and `clang`): fas uses `$WASI_SDK_PATH/bin/clang`, by default `/opt/wasi-sdk/bin/clang`, or `FAS_CC` when set. `opt-22` and `llc-22` are the usual LLVM 22 tools. `fas run --os wasi` runs the program with `wasmtime` (or `FAS_WASMTIME`), with the current directory readable.

```sh
WASI_SDK_PATH=$HOME/wasi-sdk fas --os wasi main.fas        # writes a.wasm
wasmtime run --dir=. a.wasm args
WASI_SDK_PATH=$HOME/wasi-sdk fas run --os wasi main.fas -- args
```

What changes on WebAssembly:

- `usize`, `isize` and pointers are 32-bit, so code written for 64-bit may need different casts: `trunc[u32](n)` with `n usize` is an error there (write `bitcast[u32](n)`, or keep the value `usize`), and a `usize` constant above 4294967295 does not fit. C `long` and `unsigned long` are 32-bit too (`i32`/`u32`), while `long long` stays `i64`.
- Vectors work everywhere, but the default build (`--target=portable`) splits them into scalar code, because not every runtime has WebAssembly SIMD. `--target=force-native` turns on SIMD (`simd128`, also for C inputs): faster, and needs a runtime with SIMD (wasmtime, current browsers and Node have it). At `-O0` without SIMD, a wide vector with branches inside it (a lane computed with `/` or `%`) can exceed the runtime's limit on local variables (wasmtime: "too many locals"). Keep the default `-O2` or use `--target=force-native`.
- The stack is 8 MiB, as on Linux. Running past it traps with an out-of-bounds memory access.
- Only what WASI offers exists in C: `stdio.h`, files under directories the runtime grants (`--dir`), clocks, `stdlib.h`. There are no threads, signals or sockets.
- The default output is `a.wasm`. `fas perf`, `--tune`, `--profile` and sanitizers measure or instrument Linux builds only and refuse `--os wasi`.

`fas perf` prints one row per hot loop (or per function without loops) with its share of run time, then the loop's facts: whether LLVM vectorized it (and why not), instructions, loads and stores, spills and an llvm-mca cycle estimate per iteration, calls that stayed out of line and why, and a `native:` estimate:

```text
sum.fas:4     for i usize = 0; i < n; i += 1 {         sum
      vectorized x4 interleave 2   native: vectorized x8 interleave 4, loop shape differs
      in sum (copy 1): 9 instr/iter (8 elements)   loads 2 stores 0 /iter   2.4 cycles/iter
```

`--json` prints the same data for your own tools, and `--compare OLD.json` adds a `change:` line under each loop that changed (`not vectorized → x4 interleave 2   cycles/iter 5.0 → 1.9`). `perf` comes from your distribution (`linux-tools`). Without permission to run it, `fas perf` prints the static report and says why.

**Profiles.** `fas perf --tune main.fas -- args` finds the fastest build of your program for this machine and writes the choice to `main.fasprof`, and `fas --profile main.fasprof main.fas -o app` (also `fas run` and `fas perf`) builds it.

- It tries the default build, a build optimized by counts from a training run (PGO), the portable instruction set scheduled for this CPU (`mtune`), `--target=force-native`, and combinations of these. Every candidate must print exactly what the default build prints (stdout, stderr, exit status), or tuning stops.
- Candidates run many times, interleaved, pinned to one quiet CPU, at several code alignments and heap placements. A build is kept only if it is faster at every placement by more than the measured noise, confirmed by a fresh round. Otherwise the profile keeps the default build.
- On the kept build it then tries compiler settings one at a time (larger unroll budgets, `-O3`, more inlining, vector interleave, maximum vector bandwidth) and code alignment, keeping each only when it is clearly faster again.

The last lines say what happened:

```text
fas perf: kept the profile (pgo 1.124×, band 1.3%; second input 1.118×, band 1.5%)
fas perf: kept the profile (native-pgo 1.484×, band 4.8%, options unroll=600)
fas perf: kept the default build (best native 0.981×, band 1.2%) (measured on one input)
```

- `args` should be a representative run that takes about a second or less: tuning runs the program a few hundred times, and the start says how long that will take. The program's output must not change between runs (no timings, addresses or random seeds in it). If it does, tuning keeps the default build and says so.
- `--check-with -- args2` adds a second workload: every build must reproduce its output too, and a choice that is slower there is not kept.
- In a terminal, tuning asks before it starts and offers to use `sudo` to keep the rest of the machine off the measuring CPU. It lists every change it makes as root and undoes them when it ends (also on Ctrl-C). `--yes` skips both questions and never uses sudo, and so does a run without a terminal.
- A kept native build says `tuned for MODEL; the binary may not run on other CPUs`, and building with that profile on another CPU model, or with `--target=portable`, is an error. Keep such profiles per machine. The others work anywhere (an `mtune` build keeps the portable instruction set).
- `--no-arena` measures with your normal allocator instead of the measuring one (needed only if the program behaves differently under it, which tuning tells you).
- The file is text: check it in next to the source like a lockfile. The same source and profile give the same binary. A stale profile is fine: functions you changed just get no counts. Re-tune when the hot code changes a lot.

Tools come only from `FAS_CC`, `FAS_OPT`, `FAS_LLC` and `FAS_PROFDATA` (defaults `clang-22`, `opt-22`, `llc-22`, `llvm-profdata-22`). When a suffixed tool is not on `PATH`, `fas` uses the plain name (`clang`, `opt`, `llc`, `llvm-profdata`) if its `--version` reports LLVM 22, as on Arch or Fedora. `--keep` prints which ones it picked. A `CC` set in your shell does not change what `fas` uses. `FAS_OPT_PASSES` replaces opt's pipeline.

### Editors

`fas lsp` is a language server on stdin/stdout. It takes the same `-I`, `-isystem`, `-D` and `--target` options as a build, applied to every file, and a library named with `use pkg` needs no options at all. Open any file of a project: a file loaded by `use` is checked as part of the program that loads it (the one with `fn main`), found by scanning the workspace folder. It runs the `fas check` analysis on every open file and edit, with your unsaved text, and gives:

- the compiler's errors, with their `help:` and `note:` lines, in the file they belong to (a `use` file or a C header)
- go to definition and find references, across `use` files, and a C name jumps to its header line
- an outline of functions, structs, consts and globals, and the locals inside each function (by design, so you can jump to them)
- hover with types, signatures, const values, struct size, align and field offsets, and the instantiations of generic code (`T = u8`). A C name shows the Fas signature calls are checked against, the C declaration and its header (a macro, its `#define` line), or why Fas cannot use it
- inlay hints such as `[u8]` after calls whose generic arguments were inferred.

Only the first error is shown, but the rest of the program keeps answering: a function with an error, or one cut off by a missing `}`, loses the answers inside its body, and everything else, its own name and parameters included, answers as usual. A header that is not found leaves out only what depends on it. Inside a broken body, answers come from the last version that checked, when there is one. There is no completion, signature help, rename or formatting.
