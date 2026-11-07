# Source layout

```
src/
  common/        shared by everything: diagnostics, spans, sources, JSON,
                 target layout, limits, subprocesses
  fas/           the Fas front end: lexer, parser, sema, HIR, lowering,
                 C header import/export
    lsp/         the Fas language server
  fir/           Fir, the IR every front end emits and every backend reads
                 (SPEC.md), with its folding, budgets and text form
  backend/
    llvm/        the LLVM backend: renders Fir as LLVM text (fir_llvm.ml)
    native/      the x86-64 backend
  driver/        the command line, the build driver, and the perf/tune tools
  linker/        the linker, its own library `fas_linker`
  cc/            (to come) the C front end
```

The compiler modules (everything above except `linker/`) form one library,
`fas_compiler`, with `(include_subdirs unqualified)`. Folders therefore only
organise files: module names stay flat (`Driver`, `Fir`, `Mir_emit`), and
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

Since v0.6 there is one IR: `fas/lower.ml` emits Fir through
`fas/lower_emit.ml`, and both backends read it. (The old LLVM-shaped
`ir.ml` and its converters `Fir_of_ir`/`Fir_to_ir` are gone.) The Fas
runtime helpers in `backend/native` are Fas source that the backend
compiles through the front end; that is the one remaining exception.
