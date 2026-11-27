# fcc front end

Build from the repository root with the native Fas compiler.

```sh
timeout 600 _build/default/bin/main.exe -o /path/to/fcc cc/main.fas
/path/to/fcc -E -P input.c
/path/to/fcc -fsyntax-only input.c
/path/to/fcc -Xfcc-dump-ast input.c
bash test/cc_pp.sh --json /path/to/results.json
bash test/cc_parse.sh --json /path/to/syntax-results.json
```

The driver defaults to x86-64 Linux and GNU C23. It accepts `-std=c23`
for strict C23. The parser rejects K&R definitions and implicit int.

`-m32` selects Linux i386 and its ILP32 data model. `-m16` uses the same
data model and ABI with `.code16gcc` instruction encoding. The driver
passes `-mregparm=0` through `-mregparm=3` and the selected stack
alignment to the native FIR backend. Variadic calls use the i386 stack
calling convention. Both modes can emit ELF32 relocatable objects with
`-c`. For example, run the cross-compiler checks with
`FCC=/path/to/fcc FCC_FIR=/path/to/fas_fir bash test/cc_i386.sh`.

The i386 backend supports integer and pointer code, including 64-bit
scalar arithmetic, and uses GNU as for C inline assembly. Floating-point
code, aggregate call signatures, DWARF, and direct linking through
the fcc driver require further work.

The translation unit owns a `Pp` and its allocation region. File buffers
and unsuccessful file lookups are cached for the life of the translation
unit. A sparse physical line map survives splicing. The lexer interns
spellings into stable indices and gives UTF-8 and UCN identifier spellings
the same macro slot. `pp_next` returns expanded tokens from that state.
The text driver uses a related pull interface that also reports newlines
and include boundaries.

Replacement and argument sequences are token spans. The rescan carries
the hide sets specified by the Prosser algorithm. Stringification uses
the argument's recorded spacing. Pasting lexes the concatenation as a
single new preprocessing token. Argument prescan is cached so a duplicated
parameter does not expand `__COUNTER__` twice.

The driver searches `cc/include` from the repository root by default.
Set `FCC_INCLUDE` to the absolute resource-header directory when running
elsewhere. After the normal Linux system include directories it can use
the latest installed native GCC `include` directory for target headers.
`FCC_GCC_INCLUDE` can select another ordinary GCC include directory.
Automatic discovery skips `include-fixed`.

The differential test runs an ordinary Clang comparison first. A second
comparison holds the compiler identity, target macros, and advertised
feature probes fixed. A third uses the same resource headers. The JSON
records the first differing token and the result for each input.

## Parser interface

`parse_tu(&parser, &pp)` consumes expanded tokens directly. The parser
holds two lookahead tokens for distinguishing declarators and expressions.
Attribute arguments have a local replay buffer so typed arguments can be
evaluated after the attribute has been read.

The caller can set `Parser.on_function` and `Parser.on_object` to callbacks
with the signature `fn(ctx addr, node addr, user addr) void fails`. Function
callbacks run after body and label validation. Object callbacks run after
translation-unit finalization so they see completed tentative definitions.
The nodes, types, and spellings are owned by the translation-unit region
and remain available until it is disposed. Callbacks run with that unit's
type context active. VLA type nodes refer to typed bounds in
`Parser.vla_bounds`, and binary expression nodes carry the type used to
evaluate the operation.

The syntax suite reads the frozen GCC and Clang decisions in the
`cc-tests` manifest and invokes Clang for system headers and project
sources. It also exercises constant folding and checks layout values
obtained from a native Clang build. Each expected difference identifies an
individual case, its outcome, and its reason.

## Fast front-end architecture

The front end is structured for low memory traffic and single-pass
translation without intermediate token or AST materialization:

- **Source & locations (`cc/src.fas`)**: 32-bit `Loc` offsets into a flat
  virtual address space over `mmap`'d source files, with sparse physical
  line/splice tables resolved lazily on diagnostics and `__LINE__`.
- **Unified symbol table (`cc/sym.fas`, `cc/kw.fas`)**: open-addressed
  table of 24-byte `SymSlot` entries (`hash`, `off`, `len`, `macro_id`,
  `flags`, `kw_id`) shared across lexer, preprocessor, and parser.
  Identifiers carry a `SymId` with `kw_id` embedded directly in the
  token, giving O(1) keyword, macro, and typedef dispatch with zero
  string comparisons on the hot path.
- **Direct pull preprocessing (`cc/lex.fas`, `cc/macro.fas`, `cc/pp.fas`)**:
  fast-path raw scanning feeds `Parser` via a small ring buffer;
  `#include` guard detection skips re-opening guarded headers.
- **Binding-stack scopes & compact AST (`cc/parse*.fas`)**: ordinary,
  tag, and local-label namespaces are dense `SymId`-indexed arrays with
  compact 16-byte undo records per scope (no per-block hash tables).
  `Ast` nodes are 72 bytes (`AstExtra` sidecar allocated only for cold
  fields), allocated from a two-level `Arena` (`tu_arena` for file-scope
  declarations, `fn_arena` reset after each function callback).
- **Streaming FIR lowering (`cc/gen*.fas`)**: lowers each function AST to
  Fir 0.3 directly inside `Parser.on_function` before `fn_arena` is
  reclaimed, then hands the Fir stream to `fas_fir` for native codegen.
