# Fas

Fas is a small native language for hot loops, crypto, SIMD and explicit systems code. It compiles to LLVM IR and links through Clang, and C is its ecosystem: you `use` C headers and call C directly, with no bindings to write.

It has wrapping fixed-width integers, IEEE `f32`/`f64` and SIMD vectors such as `vec[8, u32]`, with no heap and no hidden allocation, copies or runtime. Memory is an untyped `addr` with the type on each access (`p[u32, i]`), no implicit narrowing or sign changes. It builds for x86-64 Linux, x86-64 Windows and WebAssembly (WASI).

<!-- skip -->
```fas
use "C" <stdio.h>
use "C" <unistd.h>

fn count(p addr, n usize, c u8) usize {
    needle vec[32, u8] = splat(c)
    one vec[32, u8] = splat(1)
    total usize = 0
    i usize = 0
    while i + 32 <= n {
        hits vec[32, u8] = select(p[vec[32, u8], i / 32] == needle, one, splat(0))
        total += reduce_sum(zext[vec[32, u16]](hits))
        i += 32
    }
    while i < n {
        if p[u8, i] == c { total += 1 }
        i += 1
    }
    return total
}

const SIZE u32 = 65536

fn main() i32 {
    buf arr[SIZE, u8] = {}
    lines usize = 0
    n isize = read(0, &buf, SIZE)
    while n > 0 {
        lines += count(&buf, bitcast[usize](n), '\n')
        n = read(0, &buf, SIZE)
    }
    printf(c"%zu lines\n", lines)
    return 0
}
```

```sh
$ seq 1 1000000 | fas run wcl.fas
1000000 lines
```

## Install

**Prebuilt.** [Releases](https://github.com/fas-language/fas/releases) has the compiler for Linux and Windows (x86-64). Put `fas` or `fas.exe` on your `PATH`.

- **Linux:** `fas-<version>-linux-x86_64.tar.gz`, a static binary for any x86-64 Linux. Fas needs **LLVM 22** to build programs: `clang-22`, `opt-22` and `llc-22` (or the plain names when they are LLVM 22). On Debian and Ubuntu, [apt.llvm.org](https://apt.llvm.org) has them: `wget https://apt.llvm.org/llvm.sh && sudo bash llvm.sh 22`.
- **Windows:** `fas-<version>-windows-x86_64.zip`. Fas needs `clang` from [llvm-mingw](https://github.com/mingw-w64/llvm-mingw) and `opt` and `llc` from the [LLVM 22 release](https://github.com/llvm/llvm-project/releases), all on `PATH`.

**From source.** You need OCaml 5 and dune:

```sh
opam install dune
git clone https://github.com/fas-language/fas.git && cd fas
dune build --release bin/main.exe
cp _build/default/bin/main.exe ~/.local/bin/fas
```

## Use

```sh
fas hello.fas -o hello && ./hello      # build
fas run hello.fas -- args              # build and run
fas check hello.fas                    # check only
fas --os windows main.fas -o app.exe   # Windows, needs llvm-mingw
fas --os wasi main.fas                 # WebAssembly, needs wasi-sdk
fas perf main.fas -- args              # measure hot loops (Linux)
```

`FAS_CC`, `FAS_OPT` and `FAS_LLC` pick other tools. Builds are portable baseline x86-64 by default, and `--target=force-native` uses every instruction of your CPU.

## Learn

[LANGUAGE.md](LANGUAGE.md) is the whole language: every form, type, operator and builtin, coming from C, talking to C, SIMD, targets and the compiler.