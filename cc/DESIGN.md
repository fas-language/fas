# fcc preprocessing

Build from the repository root with the native Fas compiler.

```sh
timeout 600 _build/default/bin/main.exe -o /path/to/fcc cc/main.fas
/path/to/fcc -E -P input.c
bash test/cc_pp.sh --json /path/to/results.json
```

The driver follows the x86-64 Linux data model. It defaults to GNU C23 and
accepts `-std=c23` for strict preprocessing. The build has reached the
preprocessing phase. The parser will reject K&R declarations and
definitions when that phase lands.

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
