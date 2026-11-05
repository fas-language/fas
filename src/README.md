# Source layout

```
src/
  common/        shared by everything: diagnostics, spans, sources, JSON,
                 target layout, limits, subprocesses
  fas/           the Fas front end: lexer, parser, sema, HIR, lowering,
                 C header import/export
    lsp/         the Fas language server
  fir/           Fir, the IR every front end emits and every backend reads
                 (SPEC.md), plus the bridge from the old Ir
  backend/
    llvm/        the old LLVM-shaped Ir (ir.ml) and its folding/JSON; the
                 LLVM backend renders it as LLVM text
    native/      the x86-64 backend
  driver/        the command line, the build driver, and the perf/tune tools
  linker/        (to come) the linker, its own library `fas_linker`
  cc/            (to come) the C front end
```

The compiler modules (everything above except `linker/`) form one library,
`fas_compiler`, with `(include_subdirs unqualified)`. Folders therefore only
organise files: module names stay flat (`Driver`, `Fir`, `Native_gen`), and
moving a file never needs code changes.

A folder that should be its own library, like `linker/`, needs
`(include_subdirs no)` at the top of its own `dune` file. That stops the
parent's `include_subdirs` from claiming it.

Dependencies should point one way:

- `driver` uses the front ends, the backends and the linker.
- The front ends (`fas`, `cc`) produce `fir`.
- The backends consume `fir`.
- Everything may use `common`.
- No front end touches a backend, and no backend touches a front end.

The old `backend/llvm/ir.ml` is the exception for now.
`fas/lower.ml` still emits it, and `backend/native` still reads it, until
v0.4 moves the native backend onto Fir.
