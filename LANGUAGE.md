# Fas

Fas is a small, explicit systems language for crypto, SIMD and hot loops. The compiler adds no allocation, copies or runtime of its own and exploits no undefined behaviour, and C headers import as they are. It is not a general-purpose language: there is no heap or string type, and C is the ecosystem, so you `use` C headers and call them directly. Scalar `f32` and `f64` follow IEEE arithmetic, integer arithmetic wraps, and lossless widening is implicit. It compiles to x86-64 machine code with its own backend and linker.

Every example below is a complete program that compiles and prints exactly the output shown. Fas targets x86-64 Linux. The [cheat sheet](#cheat-sheet) at the end lists every form, type, operator and builtin.

```sh
fas hello.fas -o hello && ./hello       # no other tools needed unless the program uses C
fas run hello.fas                       # build, run, delete; program arguments go after --
```

```fas
use "C" <stdio.h> as c

fn main() i32 {
    c.printf(c"hello, %s!\n", c"fas")
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
  - [Range loops](#range-loops)
  - [Assertions](#assertions)
- [Errors](#errors)
- [Enums](#enums)
- [Structs, arrays and memory](#structs-arrays-and-memory)
  - [Value structs](#value-structs)
- [Generics](#generics)
  - [Function arguments](#function-arguments)
  - [Const loops](#const-loops)
- [Globals and constants](#globals-and-constants)
- [SIMD](#simd)
- [Secrets](#secrets)
- [Talking to C](#talking-to-c)
  - [C names](#c-names)
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
5. **Integer arithmetic wraps.** `/` truncates toward zero and `%` takes the sign of the dividend, as in C. Division by zero and `MIN / -1` trap. Shift counts are taken modulo the bit width.
6. **`&`, `^` and `|` bind tighter than comparisons**, unlike C: `x & 1 != 0` means `(x & 1) != 0`.
7. **Assignment is a statement.** No `x++`, no `a = b = c`, no assignment inside a condition.
8. **Evaluation is strictly left to right**, where C leaves the order unspecified. Operands, call arguments and brace entries are evaluated in the order written. An assignment computes the place's address first (base, then index), then the value, then stores, and `x op= e` reads `x` before it evaluates `e`. `&&` and `||` short-circuit.
9. **Arrays and structs are places, not values.** They are not passed, returned or assigned by value: pass `&x` and use `copy(dst, src)`. A small [`value struct`](#value-structs) is the exception, and calling a C function that takes or returns a struct by value [still works](#c-structs-by-value).
10. **`addr` is an untyped byte address.** `p + n` moves `n` bytes, `p[T]` is the `T` at `p`, `p[T, i]` is element `i`. Nothing is bounds-checked.
11. **Strings are addresses.** `c"..."` is NUL-terminated. A plain `"..."` passed to an imported C `char *` parameter is an error, even with a length parameter as in `strncmp`; write `c"..."` there. The check follows locals and `if` expressions, but not constants or struct fields.
12. **Generic calls infer their arguments from parameter types**: `max(x, 1)` infers `T` from `x`, and the literal takes that type. Inference covers `T`, `handle[T]`, `vec[N, T]` and generic structs behind handles. Every generic parameter must be determined by an argument. There is no inference from literals alone or from the result type. Explicit forms such as `max[u32](x, 1)` always work.
13. **Reading a provably uninitialized local is an error.** `{}` zero-fills an array or struct. Nothing is zeroed for you otherwise, but memory the compiler cannot check (a buffer behind an `addr`, padding) holds arbitrary fixed bytes: reading it gives the same value each time and is never undefined behaviour.
14. **Not in Fas:** heap, strings, threads (atomics exist; start threads through C), methods, overloading, macros, unions, function types and closures. Function addresses use `addr` and `call_addr`.
15. **`[...]` has two meanings.** After a builtin, a function or a generic type, it passes compile-time arguments, types or constants: `max[u32](x, 1)`, `vec[4, f32]`, `arr[4, u32]`, `syscall[u64 fails](n, ...)`. After a value, it is a memory access: `a[i]` is an array element, and `p[T]` and `p[T, i]` read or write through an `addr` (`(x + y)[T, n] = z` too). The error messages name the form they expected.

**Why there is no typed pointer.** Fas has no `T *` and no `ptr[T]` or `addr[T]`, and this is deliberate. In Fas the type sits on each access (`p[u32, i]`), not on the address.

- **A pointee type is a claim that goes stale.** `uint32_t *p` says "a `u32` lives here" for every later use, even after the memory is reused or read as something else. A type on the access is true where it is written.
- **It would give the optimizer facts nobody proved.** In C the pointee type implies alignment (`*p` on a `uint32_t *` assumes 4 bytes) and strict aliasing (an `int *` and a `float *` never overlap). Fas never emits alignment, aliasing or `inbounds` facts the source did not prove. So a raw access may be unaligned, and accesses of different types may overlap.
- **One memory model instead of two.** Without typed pointers there is no `const` pointee, no dereference operator or `.*` chains, no arithmetic scaled by element size next to byte arithmetic, and no pointer casts just to read memory as another type. Address arithmetic is always in bytes.
- **`handle[T]` is not a typed pointer.** It is the one typed address form. It names a struct: a Fas struct, a specialized generic struct or a C record. It has no arithmetic. It exists to stop mixing up `FILE` and `DIR`, not to describe memory. An `addr[T]` would grow back into `ptr[T]`, so it is rejected.

Use `view r = p[Box]` to name a typed place once and reuse it.

## Values

```fas
use "C" <stdio.h> as c

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
    c.printf(c"%u %u %x %llu %x %d\n", b, wide, bits, big, mask, ok)
    c.printf(c"%08x %x %u %u\n", fnv1a(c"fas"), rotl(bits, 8), popcount(bits), clz(wide))
    return 0
}
```
```text
44 300 fffffffb 1099511627776 af 1
bdf81b81 fffffbff 31 23
```

Smaller integers widen on their own when nothing can be lost. `usize` is another name for `u64`, and `isize` for `i64`. Signedness changes are always explicit:

```fas
use "C" <stdio.h> as c

fn main(argc i32, argv addr) i32 {
    for i i32 = 1; i < argc; i += 1 {           // argv[addr, i] takes any integer index
        c.printf(c"%s\n", argv[addr, i])
    }
    n usize = zext[usize](argc)                 // i32 -> usize: explicit, the sign could change
    total u64 = n * 8                           // usize is u64: no conversion
    delta i64 = -3
    back isize = delta                          // isize is i64
    as_bits usize = bitcast[usize](delta)       // signed -> unsigned: explicit
    c.printf(c"%llu %zd %zx\n", total, back, as_bits)
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

The supported operations are `+`, `-`, `*`, `/`, unary `-`, comparisons and the scalar builtins `sqrt`, `fma`, `floor`, `ceil`, `round` and `abs`. `round` ties to even, so `round(2.5)` is `2.0` and `round(-0.5)` is `-0.0`. `fma` rounds once, and `abs` clears the sign bit, including on NaNs. Builtin arguments use one shared float type, with untyped literals taking that type. A plain `f32` value widens exactly to `f64`. An `f32` arithmetic, unary or builtin result such as `x * x` or `sqrt(x)` remains `f32`, also when you pick a vector lane from it, so explicitly convert an operand before an operation when you need the wider calculation. `cvt[T](x)` handles integer-to-float, float-width, and saturating float-to-integer conversions. Float-to-integer conversion truncates toward zero, and NaN becomes zero. Subnormals are preserved, the sign of zero is kept, and separate multiply and add stay separate operations. A NaN produced by arithmetic has an unspecified payload and sign; unary `-` and `abs` change only the sign bit.

`vec[N, f32]` and `vec[N, f64]` support lane-wise arithmetic, comparisons, unary `-`, the float builtins above, `select`, `splat`, lane indexing, constants and memory access, plus the data-movement builtins (`shuffle`, `permute`, `compress`, `expand`, masked loads and stores, gathers and scatters). `cvt` converts between integer and float vectors of the same lane count with the scalar rules, and equal-size vectors can be `bitcast`. Float reductions, `%`, bitwise operations and shifts are rejected, and float vectors never widen implicitly.

```fas
use "C" <stdio.h> as c

fn main() i32 {
    single f32 = 0.1
    sum f32 = single + 0.2
    wide f64 = single
    c.printf(c"%08x %016llx\n", bitcast[u32](sum), bitcast[u64](wide))
    return 0
}
```
```text
3e99999a 3fb99999a0000000
```

C `float` and `double` map to `f32` and `f64` everywhere: functions, globals, typedefs and fields, in both directions. A C variadic function such as `printf` takes `f64`. An `f32` argument is rejected, so write `cvt[f64](x)`. `long double`, `_Complex`, `_Float16` and `_Float128` are not supported.

## Functions and control flow

```fas
use "C" <stdio.h> as c

fn collatz(n u64) u32 {
    steps u32 = 0
    while n != 1 {                              // parameters are mutable locals
        n = if n % 2 == 0 { n / 2 } else { 3 * n + 1 }
        steps += 1
    }
    return steps
}

fn digit(ch u8) u32 {
    switch ch {                                 // several constants per case, no fallthrough
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
    c.printf(c"%u %u\n", collatz(27), roman(c"CXLIV"))
    values: for i i32 = 0; i < 100; i += 7 {
        if i == 14 { continue values }
        if i > 30 { break values }
        c.printf(c"%d ", i)
    }
    c.printf(c"\n")
    {
        defer { c.printf(c"registered first, runs last\n") }
        defer { c.printf(c"runs first\n") }
        c.printf(c"body\n")
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
use "C" <stdio.h> as c

fn twice(x i32) i32 { return 2 * x }
fn negate(x i32) i32 { return -x }

const OPS arr[2, addr] = {&twice, &negate}     // a dispatch table

fn main() i32 {
    for i usize in 0..len(OPS) {
        c.printf(c"%d ", call_addr[i32](OPS[i], 21))
    }
    c.printf(c"\n")
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

### Range loops

`for i T in A..B { ... }` runs the body for `i` = `A`, `A + 1`, ... `B - 1`. `T` must be an integer type and both bounds must fit it.

```fas
use "C" <stdio.h> as c

fn main() i32 {
    for i u8 in 250..255 { c.printf(c"%u ", zext[u32](i)) }
    for i i32 in 3..3 { c.printf(c"never ") }
    c.printf(c"\n")
    return 0
}
```
```text
250 251 252 253 254 
```

- The range is half-open. When `A >= B` the body never runs. A loop up to the type's maximum, such as `250..255` for `u8`, never overflows.
- `A` and `B` are evaluated once, in that order, before the first iteration. Changing the variables they came from inside the body does not change the range.
- `i` is a new local in each iteration and cannot be assigned, also not through a view. Use a C-style `for` to step by other amounts or to change the counter.
- `break`, `continue` and labels work as in any loop. `continue` moves on to the next value.

### Assertions

`assert(cond)` and `assert(cond, "message")` check a `bool` that must hold; `unreachable()` marks a point the program must never reach. They are always checked, at every optimization level: a failure writes `file:line: assertion failed: message` (or `reached unreachable()`) to stderr and traps. The message must be a string literal. A function may end in `unreachable()` instead of a `return`.

```fas
fn digit(c u8) u8 {
    if c >= '0' && c <= '9' { return c - '0' }
    if c >= 'a' && c <= 'f' { return c - 'a' + 10 }
    unreachable()
}

fn main() i32 {
    d u8 = digit('c')
    assert(d == 12, "hex digit")
    return 0
}
```

### Compile-time assertions

`const assert(cond)` and `const assert(cond, "message")` check a `bool` at compile time, at top level or as a statement. The condition may use literals, constants, const and type generic parameters, and the layout queries `sizeof`, `alignof` and `offsetof`. It may also call Fas functions, which run while compiling under the rules of "Constants from calls"; inside a function body the condition cannot use locals, and a function whose own assertion calls back into it is an error. A condition that does not fold to a constant is an error, never a runtime check. A false condition is the compile error `const assert failed: message`. The message must be a string literal.

Inside a generic body the assertion is checked once per specialisation, and the error names the failing one (``note: while instantiating `first[i32, 0]` ``). In a branch that a generic parameter drops from a specialisation (see "Generics") it is not checked for that specialisation.

```fas
const N usize = 8
const assert(N > 4, "N too small")

struct Arr[T, M const usize] { v arr[M, T] }

fn first[T, M const usize](a handle[Arr[T, M]]) T {
    const assert(M > 0, "empty array")
    return a.v[0]
}
```

## Errors

A function that can fail says so with `fails`, and every caller must handle the failure. Nothing is hidden: no exceptions, no unwinding, no allocation. An error travels in a register next to the result.

- `error Empty, BadDigit` declares errors. An error name is a top-level constant of type `error`. Declaring the same name in two files means the same error, so two libraries can both declare `NotFound`.
- `error` is a 4-byte type. An error can be stored, passed, compared with `==` and `!=`, and used in `switch`, which needs a `default` arm because the set of errors is open. There is no arithmetic on errors; `bitcast` converts between `error` and `u32`/`i32` (for logs and files).
- An error's code is computed from its name, so it is the same in every build and program: the 32-bit FNV-1a hash of the name's bytes (offset basis `0x811c9dc5`, prime `0x01000193`) with the top bit set (`| 0x8000_0000`), so it never collides with an errno code. `error NotFound = 2` pins an errno code (1-4095) instead. Two errors with the same code are a compile error.
- `fn f() T fails` returns `T` or an error; `fn f() fails` and `fn f() void fails` (the same thing) return nothing or an error. `T` is a scalar, `addr`, `handle[T]`, `error`, an [enum](#enums) or a [value struct](#value-structs). Any other struct result goes through an out pointer, as usual.
- `fail E` leaves the function with error `E`, running `defer` and `errdefer` blocks. `return` never carries an error. `fail` with the zero error (only reachable through `bitcast` or `{}`) traps.
- A call to a `fails` function must be followed by `try` or `catch`; anything else is a compile error.
  - `try f(x)` passes an error up to the caller. It works only in a `fails` function and binds tighter than operators: `try f(x) + 1` is `(try f(x)) + 1`.
  - `f(x) catch 0` uses `0` when the call fails. The fallback is the whole expression to the right of `catch`, while the left side is only the call: `a + f(x) catch 1 + 2` is `a + (f(x) catch (1 + 2))`. Parenthesize to stop the fallback early: `(f(x) catch 1) * 10`.
  - `f(x) catch { ... }` and `f(x) catch e { ... }` (which binds the error) run a block on failure. The block produces no value: it must leave with `return`, `fail`, `break` or `continue`, unless the call has no result. A catch block is the whole value of a statement: a statement of its own, a declaration's value (`n u32 = f(x) catch e { ... }`), an assignment's value or a `return` value (`return f(x) catch e { ... }`). Inside a larger expression, such as `1 + f(x) catch { ... }` or a call's argument, it is an error, since the block would leave with the other operands half evaluated; use `catch VALUE` there.
  - `call_addr[T fails](p, args...)` and `call_addr[fails](p, args...)` call a `fails` function through its address.
  - `syscall[T fails](n, args...)` and `syscall[fails](n, args...)` (`T` = `i64`) make a Linux system call that fails the way the kernel reports it: a raw result in -4095..-1 is the error whose code is its negation (an errno code, so `error NotFound = 2` matches `ENOENT`); anything else, negative or not, is success and comes back as the bits of `T`. `T` is `i64`, `u64`/`usize` or `addr`, never narrower, so `trunc[i32](try syscall[fails](257, ...))` keeps the narrowing visible. `EINTR` is an error like any other, never retried behind your back. Plain `syscall` stays the raw `i64`, for the few calls whose success values can fall in -4095..-1 (such as `fcntl(F_GETOWN)` for a process group).
- `errdefer { ... }` and `errdefer e { ... }` run only when the function exits by failing (`fail`, or `try` passing an error up), together with `defer` blocks in reverse order. Like `defer`, an `errdefer` is block-scoped: it covers failures from its registration to the end of its enclosing block, so one inside an `if` body no longer runs once that body has finished. `fail`, `try` and `return` are not allowed inside `defer` and `errdefer`.
- `main` may be `fails`. An error that escapes `main` writes `err_name(e)` and a newline straight to file descriptor 2 (one `write`, no C stdio), then `main` returns 1. That is an ordinary return from `main`, so C's exit still runs: `atexit` handlers run and stdio buffers are flushed, after the error line has been written.
- Integer arithmetic wraps. `checked_add(a, b)`, `checked_sub(a, b)` and `checked_mul(a, b)` are `fails` builtins that give the exact result or fail with the predeclared error `Overflow` when it does not fit the operands' type: `n = try checked_mul(n, 10)`. `checked_trunc[T](x)` converts the integer `x` to the integer type `T` the same way, failing when the value is out of `T`'s range (too big, or negative for an unsigned `T`); it works between any two integer types. Untyped literal operands take the type from context, as for `add_sat`.
- `errno_err()` is the current C `errno` as an error, or the predeclared `Unexpected` if errno is 0. `err_name(e)` is the error's name as a C string: `"errno N"` for an undeclared errno code, `"unknown error"` for any other undeclared code.

```fas
use "C" <stdio.h> as c

error Empty, BadDigit

fn parse_u32(s addr) u32 fails {
    if s[u8, 0] == 0 { fail Empty }
    n u32 = 0
    for i usize = 0; s[u8, i] != 0; i += 1 {
        ch u8 = s[u8, i]
        if ch < 48 || ch > 57 { fail BadDigit }
        n = try checked_add(try checked_mul(n, 10), zext[u32](ch - 48))
    }
    return n
}

fn sum(a addr, b addr) u32 fails {
    errdefer e { c.printf(c"sum failed: %s\n", err_name(e)) }
    return try parse_u32(a) + try parse_u32(b)
}

fn main() i32 {
    c.printf(c"%u\n", parse_u32(c"12x") catch 0)
    t u32 = sum(c"40", c"2") catch 0
    c.printf(c"%u\n", t)
    u u32 = sum(c"40", c"") catch e {
        if e == Empty { c.printf(c"empty\n") }
        return 1
    }
    return 0
}
```
```text
0
42
sum failed: Empty
empty
```

**With C.** A C function is never `fails`; wrap the call and use `errno_err()`. The result of a `fails` function has the C type `struct { _Alignas(8) T value; _Alignas(8) uint32_t err; }` (`uint32_t` alone for no result). The error sits at offset 8 or the next multiple of 8 after the value, so a result of up to 8 bytes (a scalar, `addr`, or a value struct of at most 8 bytes) comes back in two registers, and a value struct over 8 bytes comes back through memory, as C returns that struct. C can call one through its address and Fas can call such a C function with `call_addr[T fails]`. Exporting a `fails` function from an `extern "C"` block is not supported yet.

## Enums

An enum is a closed set of named integer values with its own type.

- `enum Tok u8 { Eof, Ident, Plus = 10, Minus }` declares one. The integer type after the name is required; it is the enum's size, alignment and ABI. Members are separated by `,` or newlines.
- Values count from 0. `Plus = 10` sets one, and the members after it continue from there (`Minus` is 11). A value is an integer literal, negative for a signed type. Two members with the same value, or a value that does not fit the type, are compile errors.
- A member is always written with its enum: `Tok.Plus`. A member is a constant of type `Tok`, so it works in `const` initializers and `case` labels.
- An enum is its own type: no implicit conversion to or from integers, and no arithmetic. `==`, `!=`, `<`, `<=`, `>` and `>=` compare two values of the same enum by their integer values.
- `switch` on an enum without `default` must name every member; a value that is not a member (only possible through memory written as raw bytes, for instance by C) traps. With `default`, any subset of members is fine.
- `enum_value(t)` is the integer (`u8` for `Tok`). `enum_from[Tok](n)` is the member whose value is `n`, which must have the enum's integer type; a constant `n` that is not a member's value is a compile error, and a runtime one traps. `enum_is[Tok](n)` tests membership without trapping. `enum_count[Tok]` is the number of members, a constant. `enum_name(t)` is the member's name as a C string, and a value that is not a member traps; the name table is emitted only when `enum_name` is used.
- Enums can be fields, value-struct fields, array elements, parameters, results (also of `fails` functions), generic arguments, globals and constants. They cannot be vector lanes. A local named like an enum is an error, so `Tok.Plus` always means the member.
- `enum` is a keyword only at the start of a top-level item, before a name; elsewhere it is an ordinary name.

```fas
use "C" <stdio.h> as c

enum Tok u8 {
    Eof
    Ident
    Plus = 10
    Minus
}

fn prec(t Tok) i32 {
    switch t {
        case Tok.Plus, Tok.Minus: return 10
        case Tok.Eof, Tok.Ident: return 0
    }
    return 0
}

fn main() i32 {
    t Tok = Tok.Minus
    c.printf(c"%s %d %d\n", enum_name(t), enum_value(t), prec(t))
    n u8 = 3
    if !enum_is[Tok](n) { c.printf(c"%d is no Tok\n", n) }
    c.printf(c"%d members\n", enum_count[Tok])
    return 0
}
```
```text
Minus 11 10
3 is no Tok
4 members
```

**With C.** An enum crosses the C boundary as its integer type. An exported header declares `typedef uint8_t Tok;` and one `#define Tok_Plus ((Tok)10U)` per member. A C `enum` imported through a header stays an integer type in Fas.

## Structs, arrays and memory

```fas
use "C" <stdio.h> as c

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
    c.printf(c"%d %d %s\n", area(&box), area(&other), &other.name)
    c.printf(c"%zu %zu %zu\n", sizeof[Box], alignof[Box], offsetof[Box, name])

    bytes arr[8, u8] = {1, 2, 3, 4, 5, 6, 7, 8}
    p addr = &bytes
    c.printf(c"%x %x %x\n", p[u32], p[u32, 1], (p + 2)[u16])   // little-endian
    p[u16, 3] = 0xBEEF                          // stores work the same way
    c.printf(c"%x %x %zu\n", bytes[6], bytes[7], len(bytes))
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
- **Array indexing** `a[i]` with a constant `i` out of range is a compile-time error. A runtime `i` is not checked: an index past the end reads or writes outside the array, as `p[T, i]` does. Compare `i` with `len(a)` where it can be out of range.
- **Places through an `addr`**: `p[T]`, `p[T, i]` and `p[S].field`. They use alignment 1, are never bounds-checked, and different types may alias the same bytes.
- **`view v = place`** names existing storage: a local, field, element or raw place.
- **`view xs = p[T, ..]`** is an element view of unknown length: `xs[i]` is exactly `p[T, i]`, and `&xs[i]` is its address. Use it to name a buffer from C once:

```fas
use "C" <stdio.h> as c
use "C" <stdlib.h> as c

fn main() i32 {
    n usize = 5
    buf addr = c.malloc(n * sizeof[u32])
    if buf == null { return 1 }
    defer { c.free(buf) }
    view squares = buf[u32, ..]                 // no length, no bounds check
    for i usize = 0; i < n; i += 1 { squares[i] = trunc[u32](i * i) }
    c.printf(c"%u %u\n", squares[2], squares[4])
    return 0
}
```
```text
4 16
```
- **`@align(N)`** raises a struct's alignment. It is the only annotation. (`@inline` and `--no-inline` were removed: Fas has no inliner yet. They come back with the optimizer.)
- **`handle[S]`** is the address of a struct `S`, typed, with no arithmetic: any Fas struct, a specialized generic struct, or a C record such as `handle[c.FILE]`. `h.field` reads and writes a field through it, the same place as `p[S].field` through an `addr`. `handle_from_addr[S](p)` makes one and `handle_addr(h)` gives the `addr` back. A handle may be `null`, and `h.field` does not check for it. A struct may hold handles to its own type:

```fas
use "C" <stdio.h> as c

struct Node { value i32, next handle[Node] }

fn sum(list handle[Node]) i32 {
    total i32 = 0
    h handle[Node] = list
    while h != null {
        total += h.value                        // a field through the handle
        h = h.next
    }
    return total
}

fn main() i32 {
    last Node = {30, null}
    first Node = {12, handle_from_addr[Node](&last)}
    head handle[Node] = handle_from_addr[Node](&first)
    head.value += 100                           // `h.field` is a place
    c.printf(c"%d\n", sum(head))
    return 0
}
```
```text
142
```
- **Incomplete C types.** A C struct that is declared but never defined (`struct S;`), or a record Fas cannot lay out, has no fields or size. You can only hold it as `handle[c.S]`, pass it and store it. A Fas struct you only ever pass by handle can be empty: `struct Token {}`.

### Value structs

A `value struct` is a small struct that is a value: it is passed, returned and assigned like an integer, and travels in registers.

```fas
use "C" <stdio.h> as c

error Empty

value struct Span { p addr, n usize }       // at most 16 bytes, travels in registers
value struct Pair[T] { a T, b T }

fn span_of(s addr) Span {
    n usize = 0
    while s[u8, n] != 0 { n += 1 }
    return {s, n}                           // a brace list where the type is known
}

fn first_word(s Span) Span fails {
    if s.n == 0 { fail Empty }
    n usize = 0
    while n < s.n && s.p[u8, n] != 32 { n += 1 }
    return {.p = s.p, .n = n}
}

fn swap[T](p Pair[T]) Pair[T] { return {p.b, p.a} }

fn main() i32 {
    line Span = span_of(c"hello value structs")
    word Span = first_word(line) catch line
    c.printf(c"%.*s %zu\n", trunc[i32](word.n), word.p, line.n)
    head Span = word                        // assignment copies the value
    head.n = 2
    c.printf(c"%.*s %zu\n", trunc[i32](head.n), head.p, word.n)
    p Pair[i32] = swap[i32]({1, 2})
    c.printf(c"%d %d\n", p.a, p.b)
    return 0
}
```
```text
hello 19
he 5
2 1
```

- **Fields** are scalars, `bool`, `addr`, `handle[T]`, `error` and other value structs, with C layout, at most 16 bytes in all. No arrays or vectors. A generic value struct checks each specialization: `Pair[arr[4, i64]]` is an error.
- **Everything a struct does**: `{...}` initializers, reading and writing fields, `&v`, `p[S]` and `p[S].field`, fields of plain structs and array elements.
- **What a value adds**: parameters, results, `x = y`, `p[S] = v` and `v = p[S]`.
- **A brace list needs a known type**: a declaration, `return`, an argument of a non-generic parameter, or an assignment. `f({1, 2})` works; a generic `swap({1, 2})` cannot infer `T` from it, so write `swap[i32]({1, 2})`. After `catch`, `{` starts a block, so a fallback is a named value: `f() catch dflt`.
- **No `==`**, since padding has no value: compare the fields.
- **`fails`** works with a value struct result, and `try`/`catch` as with any other.
- **C sees a value struct as the same C struct**, passed and returned by the System V rules, so `extern "C"` functions may take and return them. A `fails` result is C's `struct { _Alignas(8) T value; _Alignas(8) uint32_t err; }`: the error sits at the next multiple of 8 after the value, so a value struct of up to 8 bytes still comes back in two registers, and a bigger one comes back through memory, as C does.
- `value` is a keyword only right before `struct`; it is still a valid name.

## Generics

```fas
use "C" <stdio.h> as c

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
    c.printf(c"oldest %u, newest %u, %zu bytes\n", r.items[r.head], r.items[newest], sizeof[Ring[4]])
    c.printf(c"%d %u %u\n", max[i32](-3, 2), max[u8](200, 7), double[true](21))
    return 0
}
```
```text
oldest 3, newest 6, 32 bytes
2 200 42
```

Type parameters are `[T]`, const parameters `[N const usize]` (any integer type, or `bool`). Each distinct argument list is compiled once. An `if` whose condition is decided by the generic parameters (const parameters, or type parameters as in `if sizeof[T] == 8`) keeps only the selected branch in that specialization; the dropped branch is not type-checked for it. If the kept branch always returns (or breaks or continues), the statements after it are dropped from that specialization too, so `double[true]` never compiles `return x * 2`, and recursive dispatch such as `if C == 1 { return leaf[N](x) }` creates no instances for the dropped calls.

### Function arguments

A generic parameter can also be a function: `LESS fn(T, T) bool` takes a function with that signature as a compile-time argument. Each distinct argument list is compiled once, with the function called directly, so there is no function pointer and nothing to box.

```fas
use "C" <stdio.h> as c

value struct Player { score i32, id i32 }
struct Buf[T] { items arr[4, T] }

fn by_score(a Player, b Player) bool { return a.score < b.score }
fn greater[T](a T, b T) bool { return a > b }

fn sort[T, LESS fn(T, T) bool](xs handle[Buf[T]], n usize) void {
    for i usize in 1..n {
        j usize = i
        while j > 0 && LESS(xs.items[j], xs.items[j - 1]) {
            t T = xs.items[j]
            xs.items[j] = xs.items[j - 1]
            xs.items[j - 1] = t
            j -= 1
        }
    }
}

// a struct may take functions too; calls read them from the handle's type
struct Counter[K, KEY fn(K) usize] { counts arr[8, u32] }
fn bump[K, KEY fn(K) usize](m handle[Counter[K, KEY]], k K) void {
    m.counts[KEY(k) % 8] += 1
}
fn low_bits(x u32) usize { return zext[usize](x & 7) }

fn main() i32 {
    ps Buf[Player] = {}
    for i usize in 0..3 {
        ps.items[i].id = trunc[i32](i) + 1
        ps.items[i].score = trunc[i32](((i + 2) % 3) * 10)   // 30 10 20 below
    }
    sort[Player, by_score](handle_from_addr[Buf[Player]](&ps), 3)
    c.printf(c"%d %d %d\n", ps.items[0].id, ps.items[1].id, ps.items[2].id)

    ns Buf[u32] = {}
    ns.items[0] = 4
    ns.items[1] = 9
    sort[u32, greater[u32]](handle_from_addr[Buf[u32]](&ns), 2)
    c.printf(c"%u %u\n", ns.items[0], ns.items[1])

    k Counter[u32, low_bits] = {}
    kh handle[Counter[u32, low_bits]] = handle_from_addr[Counter[u32, low_bits]](&k)
    bump(kh, 3)
    bump(kh, 11)
    c.printf(c"%u\n", k.counts[3])
    return 0
}
```
```text
2 3 1
9 4
2
```

- The signature is written in the parameter: `F fn(A, B) R`, `F fn(A)` for no result, `F fn(A) R fails` for a `fails` function. `fn(...)` is not a type; it appears only in generic parameter lists.
- The argument names a function: a Fas function (`by_score`), a specialised generic (`greater[u32]`), a function parameter of the enclosing generic (`twice[F](x)`), or a non-variadic C function (`c.toupper`). Its parameter types, result and `fails` must match the signature exactly, and the error says both: ``by_name` does not match function parameter `LESS`: it is `fn(i64, i64) bool`, `LESS` needs `fn(i32, i32) bool``.
- In the body, `LESS(a, b)` is a direct call (`try`/`catch` as usual when the signature `fails`), `&LESS` is the function's address, and `LESS` can be passed on as a generic argument. Anything else, such as using `LESS` as a value or a type, is an error.
- A call without generic arguments infers the function arguments from the declared types of its arguments, such as a `handle[Counter[u32, low_bits]]` local, then infers the type parameters as usual. When they cannot be read from there, write them out.
- A function with function parameters must be given its arguments (or be inferred) before it is used: a bare `sort` or `&sort` is an error.

### Const loops

`for const i T in A..B { ... }` writes the body out once for each value `A`, `A + 1`, ... `B - 1`, as straight-line code: there is no runtime loop, and in each copy `i` is a constant. Use it where you would otherwise copy a block by hand or write a recursive generic: unrolled kernels, per-lane or per-shift code, a dispatch over const instances.

```fas
use "C" <stdio.h> as c

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
        c.printf(c"copy %zu: %zu bytes\n", i, len(widths))
    }
    c.printf(c"%08x %llu %llu\n", mix(1), sum[4](&xs), shift_by(5, 2))
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
use "C" <stdio.h> as c

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
        c.printf(c"%-5s %u\n", MENU[i].name, order(i))
    }
    c.printf(c"%u orders, last %u\n", orders, history[2])
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
- An `addr` slot in a constant table may hold `c"..."`, a plain `"..."` (its bytes, no terminator: `const KW Bytes = {"if", len("if")}`), `null`, `&GLOBAL`, `&GLOBAL.field`, `&GLOBAL[k]`, or `&f` for any non-generic Fas function. A constant `addr` reads like any other: `P[u8, i]`. String bytes are read-only, and a write through them faults.
- A `const` of struct type, an array of structs or nested arrays (`const GRID arr[2, arr[3, i32]] = {{1, 2, 3}, {4, 5, 6}}`) is read-only static data, whether or not it holds addresses. Index and read it at run time. `len` works in constants, but another constant's initializer cannot read its slots.
- Globals are private to the program unless declared inside `extern "C" { }`.

### Constants from calls

A top-level scalar constant may call Fas functions: the compiler runs the call while compiling and stores the result like a literal. Its value can then size arrays, be a generic argument and appear in `const assert`, like any constant. A top-level `const assert` may call functions the same way.

```fas
fn fib(n i32) i32 {
    if n < 2 { return n }
    return fib(n - 1) + fib(n - 2)
}

const F10 i32 = fib(10)                  // 55, computed while compiling
const LEN usize = zext[usize](F10) * 2
const assert(fib(7) == 13)

fn main() i32 {
    buf arr[LEN, u8] = {}
    return F10 - 55
}
```

- The type must be an integer, `bool`, float or enum; the called code may use locals, loops, arrays, structs, generics, recursion and other constants.
- Each of these is a compile error at the constant: calling a C function, a system call, reading or writing a `var` global, a failed `assert` (the error quotes its message), a trap such as dividing by zero, and running for more than 10,000,000 steps. A `fails` call must be caught, as anywhere.
- Constants may call functions that use other such constants; a constant whose functions use the constant itself is an error.
- A `const assert` may call functions too, at top level or inside a function body; inside a generic body the calls run once per specialisation (see "Compile-time assertions").

### Constant blocks

`const NAME T = const { ... }` fills a top-level constant of any type by running a block while compiling. Inside the block `NAME` is a writable `T` that starts zeroed; after the block it is read-only data like any other constant.

```fas
const CRC arr[256, u32] = const {
    for i usize in 0..256 {
        c u32 = trunc[u32](i)
        for k usize in 0..8 {
            c = (c >> 1) ^ (0xEDB88320 & (0 - (c & 1)))
        }
        CRC[i] = c
    }
}
```

- The block follows the rules of "Constants from calls": it may call Fas functions, and the same things are compile errors. The block can also read the entries it has already filled (`FIB[i] = FIB[i - 1] + FIB[i - 2]`).
- Only the block itself writes `NAME`. The functions it calls belong to the program, where `NAME` is a constant: they may read it but not assign it.
- The block cannot `return`; it ends at its closing brace.
- The result may hold integers, `bool`, floats, enums and null addresses, in arrays, vectors and structs. A non-null address, an `error` value, a `vec` of `bool`, and a C union or C record are compile errors. NaNs and infinities keep their exact bits, NaN payload included.

## SIMD

```fas
use "C" <stdio.h> as c
use "C" <string.h> as c

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
    c.printf(c"w %u %u %u %u, reversed %u %u %u %u, compressed %u %u %u %u\n",
        w[0], w[1], w[2], w[3], r[0], r[1], r[2], r[3], k[0], k[1], k[2], k[3])
    c.printf(c"sum %u, max %u, any %d, all %d, sat %u\n",
        reduce_sum(w), reduce_max(w), any(big), all(big), add_sat(w, splat(0xFFFFFFF0))[0])

    text arr[32, u8] = {}
    c.strcpy(&text, c"hello, vector world")
    upper(&text, c.strlen(&text))
    data arr[20, u32] = {}
    for i usize = 0; i < 20; i += 1 {
        data[i] = trunc[u32](i * 7)
    }
    c.printf(c"%s, %u above 50\n", &text, count_above(&data, 20, 50))
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

## Secrets

```fas
use "C" <stdio.h> as c

struct Key {
    k arr[4, secret[u32]]          // secret elements; `n` stays public
    n u32
}

fn mix(a secret[u32], b u32) secret[u32] {
    x secret[u32] = a ^ b          // public b becomes secret implicitly
    return rotl(x, 3) + 1
}

fn ct_eq(a secret[u32], b secret[u32]) secret[bool] {
    return a == b                  // a comparison of secrets is a secret
}

fn main() i32 {
    k Key = {}
    k.k[0] = mix(5, 7)
    same secret[bool] = ct_eq(k.k[0], 17)
    k.k[1] = select(same, 100, 200)     // no branch: both sides, then a cmov
    buf arr[16, u8] = {}
    wipe(&buf, 16)                      // zeroed, never optimized away
    c.printf(c"%u\n", declassify(k.k[1]))
    return 0
}
```
```text
100
```

- **`secret[T]`** marks an integer, `bool` or integer/bool vector as secret: its value must not decide timing. Locals, parameters, results, struct fields and array elements may be secret (`arr[4, secret[u32]]`, `secret[vec[4, u32]]`). Globals, `addr`, floats, structs as a whole and C declarations never are. A typed access reads or writes a secret with `p[secret[u32], i]`.
- **Flow.** A public value becomes secret implicitly. A secret becomes public only through `declassify(x)`. Every result with a secret operand is secret, comparisons included (`secret[bool]`).
- **Allowed on secrets:** `+ - *`, `& | ^ ~ !`, shifts and rotates (secret counts too), comparisons, `zext sext trunc bitcast`, `bswap`, `mul_hi`, `add_sat sub_sat` and `select`. `select(c, a, b)` with a `bool` `c` picks between two scalars (integers, `bool`s, floats or addresses) with no branch, so it is how constant-time code chooses.
- **Errors**, each naming the rule:
  - a secret condition: `if`, `while`, `for`, `switch`, an if-expression, `&&`, `||`, `assert`;
  - a secret index, offset or address;
  - `/` and `%`, and checked arithmetic, on a secret;
  - a secret converted to a float or an address;
  - `popcount clz ctz`, lane indexing, `shuffle permute compress expand`, reductions, `any all`, and the masked, gather and scatter forms: none has a constant-time lowering yet;
  - a secret passed to C (an `extern "C"` declaration cannot have a secret parameter or result), to `syscall`, to an atomic or volatile access;
  - a secret stored into a public local, global, field, element or typed access, or passed to a public parameter. An indirect call's parameters and result are public, so a function with a secret result cannot have its address taken.
- **Generics.** `T = secret[u32]` checks the body for that type, and errors name the instantiation (`in id[secret[u32]]: ...`). An inferred type argument is public: write `f[secret[u32]](x)` to pass a secret.
- **Secrecy lives in values and typed accesses, not in memory.** `p[u32, i]` reads a public value whatever was stored there; `p[secret[u32], i]` reads a secret.
- **`wipe(p, n)`** zeroes `n` bytes at `p` with stores no transform removes. Use it for copies of secrets the program makes itself, such as key buffers.
- **The backend.** The native backend computes secrets with no branch, no secret-dependent address and no division, and checks this on every build: a violation is a compiler bug, never accepted.
- **Zeroization at return.** When a function that handles a secret (a secret parameter, local, result or store) returns, no stack byte it used (its frame and the 128-byte red zone below it) and no register except its results holds a secret it put there. Its callees zero their own. Not covered: a value the function stores to memory it does not own (use `wipe`) and the bytes of `alloca` regions. The LLVM test oracle makes no constant-time promise. Out of scope: the CPU's own data-independent-timing mode (the operating system's job), speculative execution (Spectre) and traps.

## Talking to C

C headers, integer and float macros, structs and `static inline` functions are imported directly. A `use "C" as c <<TAG` block embeds C source that is compiled with your program. Every C name is written through the file's alias: `c.printf`, `c.user`, `c.NAME_MAX_LEN`.

```fas
use "C" <stdio.h> as c
use "C" <stdlib.h> as c
use "C" <string.h> as c
use "C" as c <<C
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
    u c.user = {}                               // a C struct is an ordinary Fas struct
    u.id = 42
    c.strncpy(&u.name, c"ada", zext[usize](c.NAME_MAX_LEN - 1))
    c.printf(c"%s #%d, %d\n", &u.name, u.id, call_addr[i32](&add, 2, 3))

    nums arr[5, i32] = {5, -2, 9, 0, 3}
    c.qsort(&nums, len(nums), sizeof[i32], &compare) // &f of an exported function
    c.printf(c"%d %d %d\n", nums[0], nums[2], nums[4])

    buf addr = c.malloc(32)
    if buf == null { return 1 }
    defer { c.free(buf) }
    c.memset(buf, '.', 31)
    buf[u8, 31] = 0
    c.printf(c"%zu\n", c.strlen(buf))
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
- **C strings.** A plain `"..."` passed to an imported C `char *` parameter is rejected, at any offset into the literal and even when a later length parameter bounds the read, as in `strncmp`. Use `c"..."`. The check follows the literal through named bindings and `if` expressions. Variadic arguments such as `printf`'s `%s` value are not checked by this rule.
- **`errno`** is a function-like macro, so it is invisible. Write `__errno_location()[i32]`.
- **Feature-test macros** (`_GNU_SOURCE`, `_FILE_OFFSET_BITS`, ...) must come before any system header: put that C block first, or pass `-D_GNU_SOURCE`. Defining one after a header was read is an error. Function-like macros such as `CPU_SET` need a `static inline` wrapper in a C block.
- Importing a declaration Fas can't represent is fine, but using it is an error that names it and the reason. The [type mapping](#c-to-fas) is in the cheat sheet.

### C names

Every `use "C"` names an alias, and the file reaches its C only through it:

<!-- skip -->
```fas
use "C" <stdio.h> as c
use "C" <unistd.h> as c                 // several headers may share one alias
// a C block: the alias goes before <<TAG, and nothing may follow the tag
use "C" as c <<C
static inline int twice(int x) { return 2 * x; }
C

fn read(fd i32) i32 { return fd }       // a Fas name: no clash with c.read

fn main() i32 {
    n isize = c.read(0, null, 0)        // the C function
    eof i32 = c.EOF                     // macros, enumerators and types too: c.FILE
    return read(c.twice(eof))           // the Fas function
}
```

- **The alias is mandatory**, on headers and C blocks alike. A C name is always `alias.name`, a Fas name never has a dot, so Fas and C names never collide: a Fas `read`, `remove` or `stdin` sits next to `c.read`, `c.remove` and `c.stdin`.
- **Everything the header declares is visible**, after preprocessing and with its own includes: functions, globals, types, struct tags, enumerators and object-like macros.
- **One C entity is the same under any alias.** All the C of one file is one scope: its headers and blocks merge, and clashes inside it follow C's rules.
- **Aliases belong to their file.** A file sees only the C it imports itself; to share C with other files, wrap it in a Fas function, or import the header there too. An alias may not have the name of a Fas top-level name, and a local or parameter may not reuse it.
- **A C name is one name for the whole program.** Two files that import different C under the same name (two `static` functions `oracle`, or two values of one macro) are an error.
- Messages and editor hovers always spell C names `c.name`, whatever the file's alias.
- A bare C name is an error with a hint: ``unknown function `printf` `` with ``help: did you mean `c.printf`?``

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
use "C" <stdio.h> as c
use "C" as c <<C
#define JSON_IMPLEMENTATION
#include "json.h"
C

fn main() i32 {
    c.printf(c"%d\n", c.json_depth(c"{\"a\": [1, {\"b\": 2}]}"))
    return 0
}
```
```text
3
```

Do this in one file of the program only, or the library's functions are defined twice. Other files use `use "C" "json.h" as c`.

### Libraries with pkg-config

A library installed with a `.pc` file (most of them: SDL, zlib, GTK, libpng) is named in the source with `use pkg`. Fas asks `pkg-config` for its flags, so every command (build, `fas run`, `fas check`, the editor) finds the headers and links the library without any options:

```fas
use pkg "zlib"
use "C" <zlib.h> as c
use "C" <stdio.h> as c

fn main() i32 {
    text arr[5, u8] = {'h', 'e', 'l', 'l', 'o'}
    c.printf(c"%lu\n", c.crc32(0, &text, 5))
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
use "C" <zlib.h> as c
fn main() i32 { return 0 }
```
```text
Package zlibb was not found
```

### C structs by value

Many C APIs pass small structs by value: raylib's `ClearBackground(Color)`, `GetMousePosition()` returning `Vector2`. Fas calls them directly with the System V calling convention: the backend classifies each struct or union from its C layout, as a C compiler does. On the Fas side the struct stays a place:

```fas
use "C" <stdio.h> as c
use "C" as c <<C
typedef struct { unsigned char r, g, b, a; } Color;
typedef struct { float x, y; } Vector2;
static void show(Color c) { printf("%d %d %d %d\n", c.r, c.g, c.b, c.a); }
static Vector2 add(Vector2 a, Vector2 b) { Vector2 v = {a.x + b.x, a.y + b.y}; return v; }
C

fn main() i32 {
    bg c.Color = {245, 245, 245, 255}
    c.show(bg)                        // argument: a variable, field or array element
    a c.Vector2 = {1.0, 2.0}
    sum c.Vector2 = c.add(a, a)       // result: a declaration ...
    sum = c.add(c.add(sum, a), a)     // ... an assignment, or another call's argument
    c.printf(c"%g %g\n", cvt[f64](sum.x), cvt[f64](sum.y))
    return 0
}
```
```text
245 245 245 255
4 8
```

- A by-value argument is a variable, field, array element or another C call's struct result, of exactly that struct type. A literal such as `show({1, 2, 3, 4})` has no destination type and is an error: declare a local first.
- A struct result can initialize a declaration, be assigned to any place (`p.pos = f(...)`, `xs[i] = f(...)`), be passed to another C function, be read as `f(...).x`, or be discarded.
- This works for header and C-block functions, `static` or not, structs and unions, packed or aligned, and for the variadic arguments of variadic functions. `&f` is the address of `f` itself. Only a `static` C function is reached through a small generated C wrapper, because it has no symbol to call.
- Fas functions take and return only [value structs](#value-structs) by value. Other structs go by address.

### Assembly

An assembly unit is linked in, but it declares nothing to Fas. Declare its symbols in `extern "C"`. It takes its arguments in `rdi`, `rsi` and `edx`, as System V passes them.

```fas
use "C" <stdio.h> as c
use "asm" <<S
    .text
    .globl add3
add3:
    leal (%rdi,%rsi), %eax
    addl %edx, %eax
    ret
S

extern "C" {
    fn add3(a i32, b i32, d i32) i32
}

fn main() i32 {
    c.printf(c"%d\n", add3(1, 2, 3))
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
use "C" <stdio.h> as c

fn main() i32 {
    a Vec2 = {2, 3}
    b Vec2 = {4, 5}
    c.printf(c"%d\n", dot(&a, &b))
    return 0
}
```

<!-- cmd: $FAS app.fas -o app2 && ./app2 -->
```text
23
```

`use "path.fas"` is relative to the file that contains it. The whole `use` closure shares one namespace: there are no modules, qualifiers or visibility. Give the compiler only the root file.

`use linux "path.fas"` makes a file part of the program only when building for that OS (`--os`, default and only value today: `linux`). It is the hook for OS-specific code once Fas has more targets: the files for other OSes are never read, so each OS file can define the same functions its own way.

Only Fas files can be conditional. Put OS-specific `use "C"` lines inside the OS file.

## Known limits

- **Checks.** The compiler rejects only what is provably wrong. When it can't tell, it accepts. Fas does not insert runtime checks the programmer can write themselves, so array indexing with a runtime index is unchecked by design (compare with `len(a)` where needed). Its only runtime checks are the integer division trap, the vector lane-index trap, the enum traps (`enum_from`, `enum_name` and a `switch` without `default` on a value that is no member) and the ones you write: `assert`, `unreachable()` and the `checked_*` builtins. A trap ends the program with a hardware exception, and which signal it raises is unspecified (today `SIGFPE` for division, `SIGILL` otherwise). Buffered `printf` output not yet flushed is lost.
- **Stack.** Running out of stack always faults on a guard page; it never silently corrupts memory. How much stack a function uses is not specified, so deep recursion has no portable limit.
- **Platform.** x86-64 Linux. Builds target baseline x86-64 (SSE2) and run on any x86-64 CPU. Programs that use C (headers, `.c` inputs, libraries) also need `clang-22` (see `FAS_CC` below).
- **C.** Variadic C functions cannot take or return structs or unions by value, and the address of a function that does (`&f`) cannot be taken. Records with bit-fields, flexible array members, `long double` or `_Complex` fields, or a named field of an unnamed struct type, import only as `handle[T]` (pointers work, fields and values do not), and the error says which part is the cause.
- **Huge sources.** One expression with tens of thousands of operators, or a function with tens of thousands of locals, compiles slowly (seconds). Ordinary programs are not affected.
- **Command line.** Options go after the subcommand word: `fas run -g file.fas`, not `fas -g run file.fas`.

## Cheat sheet

### Forms

<!-- skip -->
```fas
// top level
use "util.fas"                          // another Fas file, relative to this one
use linux "sys_linux.fas"               // only when building for --os linux
use "C" <stdio.h> as c                  // C header from the system path; names are c.printf, ...
use "C" "lib.h" as c                    // C header relative to this file, same alias
// C source, up to a line holding only the tag; nothing may follow <<C
use "C" as c <<C
static inline int twice(int x) { return 2 * x; }
C
use "asm" "start.S"                     // assembly unit; also use "asm" <<TAG

const N usize = 64                      // compile-time scalar
const TABLE arr[3, u8] = {1, 2, 3}      // read-only static table
const MASK vec[4, u32] = {1, 0, 1, 0}
var count u32                           // mutable global, zero-filled
var seed u32 = 7

struct P { x i32, y i32 }               // fields separated by `,` or newlines
value struct V { x f32, y f32 }         // a value: passed, returned, assigned
struct Line @align(64) { bytes arr[64, u8] }
struct Ring[N const usize] { head usize, items arr[N, u32] }
struct Token {}                         // empty; often held as handle[Token]

fn f(a u32, p addr) u32 { return a }
fn g() void { }
error Empty, NotFound = 2               // errors; a pin is an errno code
fn h(s addr) u32 fails { fail Empty }   // returns u32 or an error
fn k() fails { }                        // no result, or an error
enum Tok u8 { Eof, Ident, Plus = 10 }   // its own type; members are Tok.Plus
fn max[T](a T, b T) T { return if a > b { a } else { b } }
fn sum[T, N const usize](xs addr) T { ... }
fn sort[T, LESS fn(T, T) bool](xs handle[Buf[T]]) void { ... }   // sort[i32, less[i32]](h)

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
for i usize in 0..n { }                   // range loop: i = 0, 1, ... n - 1
for const i u32 in 0..4 { }               // one straight-line copy per value
switch x {
    case 1, 3: f(x, null)
    case 2: { }
    default: { }
}
outer: while c { continue outer }
break  break outer  continue  continue outer  return  return x  defer { }
n = try h(s)  n = h(s) catch 0  h(s) catch e { fail e }  fail Empty  errdefer e { }
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
| `usize` `isize` | 8 | 8 | other names for `u64` and `i64`, used for sizes, counts and indices |
| `f64` | 8 | 8 | IEEE binary64 scalar |
| `addr` | 8 | 8 | untyped byte address or `null` |
| `handle[T]` | 8 | 8 | address of a struct `T` (Fas, specialized generic, or C record), with no arithmetic; `h.field` is a place |
| `arr[N, T]` | N × stride(T) | align(T) | `len(a)` is N |
| `vec[N, T]` | N × size(T), rounded up to a power of two | = size | integer lanes, at most 256 lanes and 2048 bits. `vec[3, u32]` is 16 bytes |
| `vec[N, bool]` | ⌈N/8⌉ bytes, rounded up to a power of two | = size | packed bits, lane 0 is the low bit |
| `struct` | C layout | max field | `@align(N)` raises it |
| `secret[T]` | as `T` | as `T` | `T` an integer, `bool` or integer/bool vector whose value must not decide timing; see [Secrets](#secrets) |
| `void` | | | result type only |
| `error` | 4 | 4 | an error code; `==`, `!=`, `switch` with `default`, `bitcast` to `u32`/`i32` |
| `enum E T` | as `T` | as `T` | its own type; comparisons, exhaustive `switch`, `enum_value`/`enum_from` to convert |

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

Integer and float vectors take arithmetic and comparisons lane by lane. Float vector arithmetic also includes unary `-` and compound assignment. Bitwise and shift operators are available only for integer vectors. A vector shift count is either a scalar integer, which shifts every lane by the same amount (`v >> s`), or a vector of the value's type, which gives each lane its own count. `v >> splat(s)` is rejected in favour of `v >> s`. Scalars never splat implicitly: `v + 1` is an error, and `v + splat(1)` is what you write.

With no destination type, an if-expression unifies integer branches like mixed integer operands. If `l u16` and `o` and `off` are `u32`, `if c { l } else { o - off }` has type `u32`, and the plain `l` branch is zero-extended. A known destination keeps its existing branch conversions. Constant if-expressions use the same branch types and values. Widen arithmetic operands before the operation when its narrow result could wrap.

### Conversions

| From → to | Write |
|---|---|
| `uN` → wider `uM` or `iM`, `M > N` | implicit, zero-filled |
| `iN` → wider `iM`, `M > N` | implicit, sign-filled |
| same-width signedness change | `bitcast[u32](x)` |
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

`usize` and `isize` are the same types as `u64` and `i64`; diagnostics name them `u64` and `i64`, and exported C headers declare them `uint64_t` and `int64_t`. Signedness changes stay explicit. `bool`, vectors, `addr` and `handle[T]` never widen implicitly. Mixed integer operands widen the narrower value when one type holds the other (`u32 < i64` compares as `i64`), while `u32` with `i32` and `u64` with `i64` have no common type. An untyped literal takes the type of the other operand or branch (`w < 1` and `if c { 1 } else { w }` are `u32` for `w u32`). With nothing to take a type from, it is `i32`. Inside `bitcast[T](...)` an untyped literal takes T's size: an integer literal is the unsigned integer of that size, or the signed one when it is negated, and a float literal is the float of that size. So `bitcast[f64](0x7FF0000000000000)` is infinity, `bitcast[f32](-1)` has all bits set, `bitcast[u64](1.0)` is `0x3FF0000000000000`, and `bitcast[vec[8, u8]](0x0807060504030201)` fills eight lanes. A literal that does not fit that size is an error. Targets of a size with no integer or float type (such as `vec[4, u32]`) keep the usual rule.

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
| | `bswap(x)` | type of `x` | reverses the byte order of a 16-, 32- or 64-bit integer, lane-wise; a big-endian load is `bswap(p[u32])` |
| **Arithmetic** | `add_sat(a, b)` `sub_sat(a, b)` | type of `a` | clamp to the type's range, lane-wise |
| | `mul_hi(a, b)` | type of `a` | high half of the double-width product, lane-wise |
| | `checked_add(a, b)` `checked_sub` `checked_mul` | type of `a`, `fails` | the exact result, or the error `Overflow` when it does not fit; scalar integers, needs `try` or `catch` |
| | `checked_trunc[T](x)` | `T`, `fails` | integer `x` as integer `T`, or `Overflow` when its value is out of `T`'s range |
| **Memory** | `copy(dst, src)` | statement | arrays or structs of one type, overlap is safe |
| | `volatile_load[T](p)` | `T` | scalar, `addr`, handle or vector |
| | `volatile_store[T](p, x)` | statement | never merged or removed |
| | `addr_bits(p)` `addr_from_bits(n)` | `usize`, `addr` | |
| | `handle_addr(h)` `handle_from_addr[T](p)` | `addr`, `handle[T]` | |
| | `call_addr[R](p, args...)` | `R` or `void` | indirect call through `addr`, using the C ABI |
| | `call_addr[R fails](p, args...)` `call_addr[fails](p, args...)` | `R` or `void` | indirect call of a `fails` function; needs `try` or `catch` |
| **Errors** | `errno_err()` | `error` | C `errno` as an error, `Unexpected` if it is 0 |
| | `err_name(e)` | `addr` | C string: the name, `"errno N"`, or `"unknown error"` |
| **Enums** | `enum_value(t)` | the enum's integer type | |
| | `enum_from[E](n)` | `E` | `n` has `E`'s integer type; a non-member traps (a constant one is a compile error) |
| | `enum_is[E](n)` | `bool` | whether `n` is a member's value |
| | `enum_count[E]` | integer constant | number of members, `u64` unless the context gives an integer type |
| | `enum_name(t)` | `addr` | C string: the member's name; a non-member traps |
| **Atomics** | `atomic_load[T, o](p)` | `T` | `T` is an integer or `addr`; atomic only when `p` is aligned to `sizeof[T]` (see below) |
| | `atomic_store[T, o](p, x)` | statement | |
| | `atomic_add[T, o](p, x)` `atomic_sub` `atomic_and` `atomic_or` `atomic_xor` | `T`, the old value | integers only, and add/sub wrap |
| | `atomic_xchg[T, o](p, x)` | `T`, the old value | |
| | `atomic_cas[T, o](p, expected, desired)` | `T`, the old value | stores `desired` only when the old value equals `expected`, so success is `old == expected` |
| | `atomic_fence[o]()` | statement | |
| **Checks** | `assert(cond)` `assert(cond, "msg")` | statement | always checked: a false `cond` writes `file:line: assertion failed: msg` to stderr and traps |
| | `unreachable()` | statement | writes `file:line: reached unreachable()` and traps; may end a function |
| | `const assert(cond)` `const assert(cond, "msg")` | statement or top-level item | checked at compile time: a false `cond` is the compile error `const assert failed: msg` |
| **System** | `syscall(n, args...)` | `i64` | Linux x86-64 only: kernel call `n` with up to six integer or `addr` arguments; returns the raw result, `-errno` on failure |
| | `syscall[T fails](n, args...)` `syscall[fails](n, args...)` | `T` (`i64`, `u64`, `addr`) | as `syscall`, but a result in -4095..-1 is the errno error; needs `try` or `catch` |
| **Secrets** | `declassify(x)` | type of `x`, public | the value unchanged, made public; see [Secrets](#secrets) |
| | `select(c, yes, no)` | type of `yes` | `c` a `bool`, `yes` and `no` scalars of one type: `yes` when `c`, else `no`, both evaluated and no branch |
| | `wipe(p, n)` | statement | zeroes `n` bytes at `p`; never merged or removed |
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

When every operand of `rotl`, `rotr`, `popcount`, `bswap`, `clz`, `ctz`, `add_sat`, `sub_sat` or `mul_hi` is an untyped literal, the literals take the destination type: `x u32 = rotl(0x80000001, 1)` is `3`, and `s u8 = add_sat(250, 10)` saturates at `255`. A typed operand still decides the type, and with no destination the literals are `i32`.

Every builtin name in this table, `cvt` and the float builtins included, is reserved: a variable, parameter, field, function or const named `sqrt` or `popcount` is an error (`` `sqrt` is a builtin and cannot be used as a name ``), and so are type names (`u8`, `f32`), literals (`true`, `null`) and `view`, `use` and `var`. A C declaration with such a name is imported but cannot be used from Fas.

**Memory orderings.** The `o` argument of an atomic is `relaxed`, `acquire`, `release`, `acq_rel` or `seq_cst`, with C11 meanings. A load cannot be `release` or `acq_rel`, a store cannot be `acquire` or `acq_rel`, and a fence cannot be `relaxed`. A failed `atomic_cas` uses the strongest ordering valid for a load: `acquire` for `acq_rel`, `relaxed` for `release`. An atomic access to an address that is not a multiple of `sizeof[T]` still happens, but it is atomic only if the CPU makes it so: on x86-64 such a split lock is very slow, and it faults when the kernel enables split-lock detection. It is not undefined behaviour. **Races are not undefined either**, unlike C: a plain read that races with a write returns bytes some write stored, and an aligned plain read or write of an integer, float or `addr` of up to 8 bytes is never torn. But plain accesses give no ordering between threads, and the compiler may merge or hoist plain reads, so a loop that waits for another thread's write must read with `atomic_load`.

### Builtin cost by CPU level

Every builtin has the same result on every CPU. Builds target baseline x86-64 (SSE2), and today Fas's own backend compiles every vector operation lane by lane: a `vec[8, u32]` add is eight scalar adds. Vector code is correct everywhere but not yet faster than the equivalent scalar loop. `objdump -d` shows the generated code.

### C to Fas

| C | Fas |
|---|---|
| any declared name `x` of a `use "C" ... as c` | `c.x` ([C names](#c-names)) |
| `int`, `char`, `long`, `double`, `uint8_t`, `size_t`, `ssize_t`, `_Bool` | `i32`, `i8`, `i64`, `f64`, `u8`, `usize`, `isize`, `bool` |
| enum constants, `#define N 16` | integer constants with C's type |
| `#define SCALE 0.5f`, `FLT_MAX`, `M_PI` | `f32` (`f`/`F` suffix) or `f64` constants. Only a single literal, optionally `-` and parentheses |
| `void *`, `char *`, `int *`, function pointers | `addr` |
| `struct S *` (complete or incomplete `S`) | `handle[c.S]`, and `&s` of an imported struct converts |
| complete structs and unions | Fas record types `c.S`: fields, `{}`, `sizeof`, globals, arrays |
| C `float`, `double` parameters, results, globals, typedefs | `f32`, `f64` |
| `float`, `double` record or union fields | `f32`, `f64`, with ordinary field access |
| `extern T x;` | a global place |
| `T name[N]` global | `arr[N, T]` place |
| struct or union parameters and results of C functions | a place argument, the result only in `x S = f(...)` or `place = f(...)` ([details](#c-structs-by-value)) |
| bit-fields, variadic functions with struct arguments, `long double`, `_Complex`, `_Float16`, `_Float128`, `__int128`, function-like macros | not supported |

### Compiler

```sh
fas main.fas -o app             # executable
fas -g main.fas -o app          # DWARF debug info for gdb/perf; same machine code
fas run main.fas util.c -- a b  # build to a temp file, run with args a b, delete; exit code passes through
fas -c lib.fas -o lib.o         # object, no main required
fas --emit-header lib.fas -o lib.h
fas --emit-ir main.fas          # the lowered module as Fir text
fas check main.fas              # errors only: no code generation, no linking (add --diagnostics=json for tools)
fas check --all-errors main.fas # every error in function bodies at once, not just the first
fas --version                   # fas version and commit, plus the C compiler in use
fas --keep main.fas -o app      # keep temp files and print their paths
fas main.fas util.c -lz -o app  # C sources and link flags (executables only)
```

Exit status: 0 success, 1 errors in the program, 2 a usage error, 3 an internal compiler error (a bug in Fas; the message says so). `fas run` passes the program's own status through once it builds.

`--all-errors` (any command) keeps checking after an error: every function body is checked and each failing statement is reported, so leaving out all casts gives the full list of places that need one in a single run. Errors outside function bodies (parsing, types, signatures, constants) still stop at the first. Without the flag only the first error is printed.

`-O0`..`-O3` are accepted for compatibility with C build scripts and ignored: every build is the same. There is no assembly text output; read an object (`fas -c`) or executable with `objdump -d`.

To find hot code, build with `-g` and run the system `perf` (`perf record ./app; perf report`): the line tables map samples back to Fas lines.

Fas generates and links code itself. The C compiler comes only from `FAS_CC` (default `clang-22`) and is used for C headers, `.c` inputs and `--hosted` executables. When `clang-22` is not on `PATH`, `fas` uses `clang` if its `--version` reports LLVM 22, as on Arch or Fedora. A `CC` set in your shell does not change what `fas` uses.

### Editors

`fas lsp` is a language server on stdin/stdout. It takes the same `-I`, `-isystem`, `-D` and `--target` options as a build, applied to every file, and a library named with `use pkg` needs no options at all. Open any file of a project: a file loaded by `use` is checked as part of the program that loads it (the one with `fn main`), found by scanning the workspace folder. It runs the `fas check` analysis on every open file and edit, with your unsaved text, and gives:

- the compiler's errors, with their `help:` and `note:` lines, in the file they belong to (a `use` file or a C header)
- go to definition and find references, across `use` files, and a C name jumps to its header line
- an outline of functions, structs, consts and globals, and the locals inside each function (by design, so you can jump to them)
- hover with types, signatures, const values, struct size, align and field offsets, and the instantiations of generic code (`T = u8`). A C name shows the Fas signature calls are checked against, the C declaration and its header (a macro, its `#define` line), or why Fas cannot use it
- inlay hints such as `[u8]` after calls whose generic arguments were inferred.

Only the first error is shown, but the rest of the program keeps answering: a function with an error, or one cut off by a missing `}`, loses the answers inside its body, and everything else, its own name and parameters included, answers as usual. A header that is not found leaves out only what depends on it. Inside a broken body, answers come from the last version that checked, when there is one. There is no completion, signature help, rename or formatting.
