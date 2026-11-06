# fas-ld: ELF linker

An independent OCaml library and CLI for Linux x86-64. The declared v0.5
dynamic-executable subset is implemented, including PIE, versioned imports, TLS,
explicit CRT/libc startup and minimal input scripts. This is not a production-ready
general replacement for GNU ld or LLD.
v0.6 adds shared-library output, preemption, RELRO, TLSDESC, defined IFUNCs and
multiple named symbol-version aliases. These have independent consumer and loader
tests; the declared v0.6 subset passes its acceptance checks. v0.7 compatibility
work is underway, including compressed debug input, static musl/glibc, thin
archives, relocatable partial linking and focused C++ runtime interoperability.
It does not invoke an external linker or use LLVM/BFD libraries.

See the [v0.1–v1.0 roadmap](ROADMAP.md) for release scope and acceptance gates.
The [CI qualification guide](CI.md) describes automated and local reproduction.
The [release packaging guide](release/README.md) describes qualified local archives.
The [current release audit](AUDIT.md) records the original findings, subsequent
fixes and remaining release qualification work. Fas/C are the release priorities;
C++ stays at the existing usable interoperability level. LTO is out of scope.

From the `fas` directory:

```sh
dune build --profile release src/linker/bin/main.exe
_build/default/src/linker/bin/main.exe -o program start.o support.o
_build/default/src/linker/bin/main.exe -o program start.o -L ./lib -lsupport
./program
dune runtest src/linker
```

The executable identifies itself as `fas-ld`; its dune build filename is
`main.exe`. The native compiler backend uses this library for executable output
and emits independent ELF objects with `-c`. Development tests require
Python 3, GNU as/ar/ld/readelf, and GCC; they also compare LLD and compile with Clang
when those tools are installed. Runtime dependencies are the OCaml standard
runtime and Unix facilities, with no third-party OCaml packages.

To build and install the CLI from source:

```sh
make -C src/linker
make -C src/linker install PREFIX="$HOME/.local"
```

The default prefix is `/usr/local`; `BINDIR` overrides its `bin` directory.
Packagers can stage the installation with `DESTDIR=/path/to/staging`. Installation
copies the native executable as `fas-ld`; it does not install an OCaml library API.
`make -C src/linker test` runs the linker checks in the release profile.

## Supported contract

| Area | Current support |
| --- | --- |
| Inputs | Regular ELF64 little-endian x86-64 ET_REL objects; System V or Linux ABI version zero; GNU/SysV regular and thin archives, 32/64-bit indexes and GNU long names |
| Output | Relocatable ET_REL; static ET_EXEC and CRT-relocated static PIE; dynamic ET_EXEC, loader-backed PIE and shared-library ET_DYN, 4 KiB load-page alignment for final images |
| Sections | PROGBITS, allocatable NOBITS, init/fini/preinit arrays, GNU Linux ABI/property notes, SystemTap v3 probe notes, validated groups/COMDAT and merge flags; input alignment preserved; conservative read-only whole-section deduplication |
| Symbols | Local, strong and weak globals; section-relative, absolute and global common; NOTYPE, OBJECT, FUNC, SECTION, FILE, COMMON, TLS, GNU_IFUNC; default/protected/hidden/internal visibility |
| Relocations | RELA: `R_X86_64_NONE`, `64`, `PC32`, `PC64`, `PLT32`, `32`, `32S`, `GOTPCREL`, `GOTPCRELX`, `REX_GOTPCRELX`, `GOTPC32`, `GOTPC64` |
| Entry | Executables: global `_start` by default, or `-e SYMBOL`, inside executable code; shared libraries: zero |
| Flags | `-o`/`--output`, `-r`/`--relocatable`, `-e`/`--entry`, `-L`/`-l`, groups/whole-archive, `@FILE`, `-Map`/`--Map`, `--gc-sections`/`--no-gc-sections`, `-u`/`--undefined`, `--stats`, `--`, `--help`, `--version`, `-v` |
| Metadata | LLVM address-significance tables validated/consumed; five GNU x86 bitmask properties merged; DWARF 4/5 compiler fixtures and CFI unwind/index plus personality/LSDA pointer handling |
| Output permissions | Separate R, RX, and RW load segments, no writable executable sections, non-executable stack |
| Diagnostics | Input file, target section/offset and symbol for relocation failures |
| Publication | Completed temporary file renamed in the destination directory; prior output preserved on failure |

`--version` prints the linker identity and exits. GNU-style `-v` prints it and
continues a requested link; without inputs it prints the identity and exits.

Relocation expressions operate on 64-bit address bit patterns. `32` must
zero-extend and `32S`/`PC32`/`PLT32` must sign-extend to the computed result.
`64` writes the complete modular 64-bit result. `PLT32` resolves directly to an
executable definition or through an eager PLT/GOT stub for a dynamic import.
An unresolved strong global is an error when referenced by a non-NONE relocation
or selected as the entry point; an unresolved weak reference becomes zero. A
strong declaration of the same name upgrades weak references. Unreferenced
undefined declarations are omitted from the output symbol table; undefined
strong symbols with non-default visibility are rejected. Local symbols are
scoped to their input file.

Strong definitions beat common and weak definitions. Common declarations merge
maximum size and alignment into zero-filled BSS; common beats weak. The first
weak definition wins ties, and multiple strong definitions are errors by default.
`--allow-multiple-definition` or `-z muldefs` keeps the first strong definition,
including during partial links; strong/common/weak precedence is unchanged. Output
contains one canonical definition per global name, with the most restrictive
visibility from loaded inputs. Common symbols must have global binding.

Archives are searched left to right. Indexed extraction follows forward member
passes: new references to earlier members wait for the next pass, preserving
GNU ld's weak-selection behavior. Unresolved weak symbols do not extract members;
common symbols extract regular strong definitions, but do not extract weak/common
definitions. The entry symbol is an extraction root. `--start-group`/`--end-group`
(`-(`/`-)`) repeat their inputs to a fixed point. `--whole-archive` forces all
subsequent archive members until `--no-whole-archive`. Member ELF payloads are
validated only when candidates are needed; container bounds/index offsets are
always validated. Unindexed nonempty archives require whole-archive mode.

Repeated positional files reuse the first bytes read from their canonical path
within one invocation. Each occurrence is parsed independently, so archive member
selection and whole-archive behavior remain separate. File/byte budgets still
charge every occurrence, and alias protection still checks every supplied path.
`--stats` reports `input_read_cache_hits`; the cache is never shared between links
or domains. Thin-member/dependency reads retain their separate existing policy.
Cached bytes remain live until the invocation ends, within the combined input
budget; wider sets of large DSOs still need memory qualification.

GNU thin archives refer to external objects rather than embedding them. Relative
member paths resolve against the archive's directory, following
[GNU ar's path contract](https://sourceware.org/binutils/docs/binutils/ar.html).
Members are read lazily with regular-file and invocation-budget checks. Missing
unselected members are tolerated, matching the tested GNU ld policy; this differs
from the installed LLD 22. Nested references into regular archives are supported;
GNU ar flattens thin-archive inputs. Nested containers must be regular archives.
Existing external members, including unselected ones, receive output/map alias
protection. Selected external files count toward the input count/byte limits;
shared nested containers are read and parsed once per outer archive. The objects
actually read determine the link: thin archives are not source snapshots.

`-r` combines selected objects into another ET_REL without assigning addresses
or applying relocations. Section contributions retain their alignment, COMDAT
membership and remapped symbol/relocation references. Common symbols remain
common; undefined symbols, including hidden references and future linker-defined
layout symbols, remain unresolved. There is no implicit `_start` archive root;
use `-u` or whole-archive to select members. No selected objects is an error.
The output can be partially linked again, archived or consumed by Fas/GNU/LLD.
Tests cover local-name collisions, weak/common precedence, groups, TLS,
initialization arrays, compressed debug input, named versions and IFUNCs.
Address-significance tables are rebuilt conservatively when an input lacks a
valid table, preserving safe-ICF address identity in independent consumers.
Partial output has a 128 MiB limit so it remains usable as an ordinary input.
GC, maps, entry overrides and final-image/version-script options are rejected
with `-r`. The output is atomically published with mode 0644.

`-lfoo` searches `libfoo.a` for static output and prefers `libfoo.so` over
`libfoo.a` per directory for dynamic output; `-l:NAME` searches the exact basename. All explicit
`-L` directories apply regardless of option position, in their declared order.
There are no implicit library directories. Response files support whitespace,
single/double quotes, backslash escaping and nested `@FILE`; paths are relative
to the working directory, with no shell expansion. Malformed or unreadable
response files are errors (GNU's unreadable-file literal fallback is not used).

Non-allocatable ordinary PROGBITS sections with matching metadata are combined;
their relocations use offsets in the combined output section. Multi-object
GCC/Clang DWARF 4/5 fixtures pass GNU readelf/addr2line inspection and GDB source
breakpoint, local-variable and backtrace checks, including section GC. This is a
tested configuration set, not complete support for every DWARF extension.
The empty non-executable `.note.GNU-stack` marker is consumed rather than copied.
Crash durability (`fsync`) is not promised. Input/output inode aliases are
rejected. Reproducibility holds for fixed input bytes and input order.

When referenced and not defined by an input, the linker supplies absolute symbols
`__executable_start`/`__ehdr_start` (image base), `_etext`/`etext` (end of executable
loads), `__bss_start`/`_edata`/`edata` (end of file-backed load contents, before BSS
alignment padding), and `_end`/`end` (end of load memory, including BSS/common).
These describe this linker's layout, not a GNU default linker script.
Their internal absolute representation does not give dynamic exports absolute
semantics: exported image endpoints use regular section indexes in `.dynsym`,
so independent loader lookup includes the load bias. Genuine input absolute
symbols retain `SHN_ABS`. Synthesized image-header addresses, loader internals,
startup-array boundaries and registry boundaries are hidden; explicit input
definitions retain their requested visibility.

`__start_SECTION` and `__stop_SECTION` are synthesized on final links when
referenced, not explicitly defined, and a loaded section named `SECTION` exists.
Names must be ASCII C identifiers. This subset covers allocatable non-TLS
PROGBITS and NOBITS with compatible types, flags and entry sizes. Contributions
form one contiguous output section in input order, with alignment padding
included in its boundary range. Empty sections produce equal boundaries.
Registry contributions retain distinct storage even with SHF_MERGE. COMDAT
losers are excluded. Mixed incompatible contributions fail explicitly.

A live reference to either synthesized boundary, or a corresponding `-u` root,
retains every loaded matching contribution under `--gc-sections`, including
ordinary relocation dependencies. This is GNU ld's conservative policy; LLD's
comparable mode is `-z nostart-stop-gc`. Fas does not yet expose a selectable
start/stop GC policy. Explicit input definitions disable this special retention.
Boundary references alone do not extract archive members: use real referenced
symbols or `--whole-archive`. Partial links preserve unresolved boundaries for
the final linker; absent sections do not acquire synthetic boundaries.

Nonempty allocatable legacy `.ctors`/`.dtors` lists and their priority suffixes
are rejected on final links with a targeted diagnostic, including under GC.
Use modern `.init_array`/`.fini_array`. Partial links preserve legacy lists for
independent final linkers; empty lists and unrelated section names are allowed.

`-Map PATH`, `-Map=PATH`, and `--Map` equivalents emit a deterministic text map
with entry address, input section placement, winning symbols, and file/archive
provenance. Strings use OCaml escaped quoting. Maps have a 128 MiB limit.
The map is published before the executable, each through its own atomic rename;
the pair is not transactional. A later executable publication failure can leave
an updated map alongside the previous executable. Output/input aliases and
executable/map aliases are rejected.

The first loaded COMDAT signature wins as a group; non-COMDAT groups remain
distinct. `--gc-sections` roots the entry, `-u` symbols and SHF_GNU_RETAIN, follows
live relocation edges and retains groups as units. Common BSS is conservatively
one GC unit. Debug sections do not keep dead code alive; references to discarded
definitions receive tombstones. Unresolved references from live code still fail.
Identical read-only, relocation-free SHF_MERGE sections share storage. Individual
entry/string and suffix merging are not implemented. Writable or relocated merge
sections remain distinct. Static ET_EXEC PIC uses a GOT without instruction
relaxation. Static PIE has the narrow bootstrap rewrites described below.

Executable publication requests mode 0755; object, DSO and map publication
requests 0644. Creation applies the caller's `umask` and default ACL, and those
resulting permissions are preserved through atomic rename, including replacement
of an existing file. The helper never changes the process-wide `umask`; image
bytes stay private while being written to the temporary file. Failure before
rename preserves the existing output and its permissions.
Test-only Linux syscall interposition covers ENOSPC, partial-write/EIO, failed
close/chmod/rename, and successful recovery from short writes/EINTR. Input
short reads/EINTR, EIO/EOF, temporary-creation resource failures/name collisions
and real file-size limits are also tested, including repeated library recovery
without descriptor leaks. A killed
writer may leave a private incomplete `.fas-ld-*.tmp`; later links ignore it.
No cross-file transaction or fsync-based crash durability is promised.

`-o /dev/null` and map output to `/dev/null`, including symlinks to that device,
perform complete link/map validation and discard the result. The linker never
opens, replaces or changes permissions on the device. Other special output files
remain rejected. Input/output and executable/map alias checks still apply.

`.eh_frame` supports 32-bit record lengths, CIE versions 1/3/4, empty augmentation
or `z` with distinct `P`/`L`/`R`/`S` fields, and absolute/PC-relative 32/64-bit
initial pointers. Personality pointers can be indirect; LSDA pointers use supported
fixed-width absolute or PC-relative forms. Augmentation lengths and relocation
kinds are checked; unsupported pointer forms fail explicitly. Contributions are
normalized and combined; `.eh_frame_hdr` and PT_GNU_EH_FRAME index live FDEs.
Live code retains its personality, LSDA and referenced typeinfo under section GC;
metadata alone does not root the described code. Dead FDEs remain as unindexed
tombstones. LSDA payloads remain compiler-generated opaque data with normal
section/relocation bounds checks. Extended DWARF record lengths remain unsupported.

GCC/Clang CFI and GDB backtraces with frame pointers omitted are tested. A focused
GCC/Clang C++ corpus against the installed libstdc++/libgcc runtime also tests
cross-object cleanup/rethrow, partial links, fixed/PIE output and GC, exceptions
in both directions across DSOs, RTTI/dynamic_cast/typeinfo identity, virtual
object destruction and C++ global initialization/shutdown. Fas/GNU/LLD produce
and consume the shared fixtures. Late `dlopen` fixtures also check exception
cleanup, thread-local construction/destruction, unload and reload. This does not
qualify the full C++ ecosystem: ET_REL GNU unique definitions, static C++ and a
broader application corpus remain separate gates.

GNU property notes support FEATURE_1_AND, FEATURE_2_NEEDED/USED and ISA_1_NEEDED/USED.
AND/OR-AND properties require presence in every loaded input; NEEDED properties
are ORed. Missing markings clear CET claims. Synthetic common/layout objects do
not count as inputs. One merged note and NOTE/GNU_PROPERTY program headers are
emitted when a property survives. Repeated property notes within one input merge
their bitmasks with OR before the across-input policy is applied, matching the
tested GNU ld behavior. Empty GNU property descriptors are accepted.

Nonallocatable SystemTap v3 `.note.stapsdt` records preserve provider/name/argument
strings and relocate their three address fields. Section-symbol COMDAT signatures
use the defining section name when the signature symbol has no name. Probe notes
do not retain described code under GC; discarded references receive zero
tombstones. Partial links and GNU/LLD consumers are tested. Other note/property
semantics fail explicitly.

Unsupported inputs fail explicitly: BSD archives, zstd/legacy GNU compressed sections,
other note/property types, unsupported unwind pointer forms, extended numbering,
overlapping relocation patches, full layout scripts, LLVM bitcode, and other targets.
There is no automatic C startup or libc discovery. Supply `_start` or system CRT
objects explicitly.
GCC slim/fat LTO objects and Clang full/thin bitcode receive an explicit LTO
diagnostic, including selected archive members and partial links. Recompile with
`-fno-lto`; the native object path remains the compiler boundary. Fat LTO objects
are intentionally rejected as a whole rather than silently selecting their native
fallback. Embedded bitcode in an otherwise ordinary native object is a distinct
format and does not enable LTO.

### Dynamic executables (v0.5 declared subset)

`-pie` emits loader-backed PIE; `--dynamic-linker PATH` selects an interpreter and
also enables fixed-address dynamic output without `-pie`. The PIE default is
`/lib64/ld-linux-x86-64.so.2`. `-no-pie` disables PIE but preserves an explicit
interpreter. `-rpath`/`--rpath` adds DT_RUNPATH by default;
`--disable-new-dtags` selects legacy DT_RPATH, and `--enable-new-dtags` restores
RUNPATH. The last selection wins; repeated paths join with `:`. `$ORIGIN` is
preserved literally. Independent Fas/GNU/LLD producers and consumers test
three-level runtime dependency chains, inherited RPATH, per-library RUNPATH and
environment priority, including mixed RPATH/RUNPATH chains. This qualifies
runtime loader behavior. Supplied DSOs become DT_NEEDED entries,
deduplicated by SONAME, except those skipped by a script's `AS_NEEDED` policy.
Command-line `--as-needed`/`--no-as-needed` applies positionally to following
shared dependencies; unresolved weak references do not retain a library.
`--push-state`/`--pop-state` saves/restores whole-archive, as-needed and library
search selection. `-Bstatic` restricts following `-l` searches to archives;
`-Bdynamic` restores the normal shared-first search for dynamic output. `-static`
selects static output and archive-only search. `-m elf_x86_64`, `--eh-frame-hdr`
are accepted: the target is fixed and unwind indexes are generated by default.
`-nostdlib` ignores script `SEARCH_DIR` entries while retaining command-line `-L`.

`--sysroot=DIR` (also `--sysroot DIR`) selects a host directory containing target
files; the last selection wins and an empty value clears it. `-L=lib`, `-L=/lib`
and `-L'$SYSROOT/lib'` search its `lib` directory. `--library-path` is an alias
for `-L`. Ordinary absolute `-L` directories and positional input files remain
host paths. All command-line `-L` directories apply globally, in their specified
order; no implicit system directories, environment paths or loader cache are
searched. Scripts can add directories as described below. Search is bounded to
4,096 directories and one million filesystem probes per invocation.

`--no-allow-shlib-undefined` enables link-time transitive DSO validation.
`-rpath-link`/`--rpath-link` accepts repeated colon-separated search paths,
including the `=PATH` spellings. Selected direct DSOs are matched by identity
first; missing dependencies are searched in those explicit directories, then
the referring DSO's RUNPATH or its own/inherited RPATH. `$ORIGIN`/`${ORIGIN}`
resolve against the referring DSO's directory. Explicit search path tokens use
the invocation's working directory. Basename lookup reuses the first selected
SONAME identity; expanded explicit paths can select distinct files with the same
SONAME. Tokens expand before deciding whether a dependency names a path, with
identifier boundaries checked for unbraced `$ORIGIN`. Provider-version checks
retain the referring DSO association; cycles terminate by loaded file identity. Missing files, strong unresolved references, TLS type mismatches
and missing required version nodes/symbols fail with the referring DSO's path.
Weak unresolved references are allowed. Indirect callback references retain and
export visible executable definitions under GC. Inspected indirect DSOs do not
become direct DT_NEEDED entries or satisfy regular-object references.
Discovered inputs share file/byte budgets and output/map alias protection.

This is an explicit-search subset: it does not inspect environment paths, the
loader cache, implicit system directories, the output image's runtime path,
or `$LIB`/`$PLATFORM`. With a sysroot, explicit `-rpath-link` directories support
the same `=`/`$SYSROOT` prefixes as `-L`; ordinary host directories remain literal.
Absolute paths in a DSO's embedded RPATH/RUNPATH or DT_NEEDED are rooted for
link-time reads. `$ORIGIN` expands to the actual referring DSO directory. The
host sysroot is never added to output interpreter, SONAME, DT_NEEDED or runtime
search strings. Indirect DSO references do not initiate
another archive extraction pass, matching the tested GNU ld behavior for regular
and thin archives, including groups. Provide callbacks as explicit objects,
`--whole-archive` members, `-u` roots or a direct DSO followed by its archive.
GNU's complete search/default policy remains
qualification work. The current default is `--allow-shlib-undefined`, leaving
transitive lookup to the runtime loader; the last strictness selection wins.
Strict search is bounded to 1,024 libraries, 65,536 edges, 4,096 search directories,
one million probes and four million aggregate symbol/version records, in addition
to the invocation input budgets. Each DSO parser permits at most one million
version headers/auxiliary records. These are correctness limits, not scale claims.

Defined `STB_GNU_UNIQUE` symbols are accepted from DSOs, including indirect
`libstdc++` inputs during strict validation. Output imports retain normal
undefined GLOBAL/WEAK binding; the original DSO supplies the unique definition
and the runtime loader enforces its identity. Tests cover both dependency orders,
direct data imports and identity across separate `RTLD_LOCAL` loads. GNU unique
definitions in ordinary ET_REL inputs remain unsupported; this input support
does not qualify general C++ interoperability.

`--hash-style=sysv|gnu|both` selects dynamic lookup tables (default: sysv).
GNU tables use stable bucket ordering and a bloom filter; relocations, symbol
versions, IFUNC ordering and TLS indices follow the resulting dynsym permutation.
`--build-id` defaults to SHA-1; `--build-id=sha1|md5|none` selects a deterministic
GNU note. The digest covers the complete final ELF with the descriptor zeroed;
it is a build identifier, not an authentication mechanism. Byte equality with
GNU ld's build-ID algorithm is not promised.

GCC and Clang can invoke the linker through an isolated directory containing an
`ld` symlink, selected with `-B/path/to/directory/`. GCC currently requires
`-fno-use-linker-plugin`; LTO plugins remain unsupported. Default PIE, non-PIE,
shared output, threads/TLS, debug information and section GC have direct driver
tests. `-gz` also requests output compression through a linker option; compressed
debug input is supported, but output compression is not yet implemented.

DSO input validates ELF/dynamic tables, SysV/GNU hashes and version definitions.
Default and explicit `name@VERSION` imports generate required-version tables.
GOT-based PIC data/function addresses, PLT32 calls, writable 64-bit import
pointers, GLOB_DAT and PIE RELATIVE relocations are supported. Binding is eager;
text relocations remain rejected.

Fixed and PIE executables also support direct references to DSO `STT_OBJECT`
data through COPY relocations. PC32/PC64, 32/32S and read-only R64 references
can request a copy; normal address-range and PIE relocation restrictions still
apply. Writable data gets executable BSS storage. Data from read-only sections
or wholly inside GNU RELRO gets `.data.rel.ro` storage, made read-only by the
loader when RELRO is enabled. Size/alignment, DSO/executable address identity,
pointer initializers, explicit/default import versions, preload overrides, GC,
archive inputs and partial links have independent GNU/LLD tests. Distinct
versioned definitions retain distinct copies; tested BFD 2.46 and LLD 22 coalesce
such versions in one combined executable fixture, so that case uses explicit
expected values rather than their output as an oracle.

The COPY subset rejects protected/GNU-unique data, zero-sized or out-of-section
storage, overlapping exported aliases and mixed explicit/unversioned spellings
of the same provider definition. Use GOT-based position-independent references
for those cases. Allocatable non-TLS PROGBITS/NOBITS, alignments up to 2 MiB and
256 MiB aggregate copied storage are supported within the existing image/output
limits. Direct imported function addresses requiring unsupported narrow/PC-relative
relocations still fail. Shared output continues to require position-independent
import references and never creates copies.
Unresolved default-visibility weak imports remain available to LD_PRELOAD through
dynamic GOT/PLT references. Fixed-output narrow absolute references to absent
weak providers bind to zero, as required by the tested non-PIE GCC CRT.
Regular executable definitions take precedence over DSO definitions. Definitions
referenced by supplied DSOs are exported and retained by GC; `-E`,
`--export-dynamic` and GNU's `-export-dynamic` export all visible definitions,
and `--no-export-dynamic` restores the default. DSO references can extract later
archive members.

Init/fini/preinit arrays are combined in numeric priority order, preserving input
order for ties. `.init`/`.fini` CRT fragments are combined with executable NOP
padding; DT_INIT/FINI and array tags describe them. GCC/Clang hosted fixtures pass
with explicit glibc CRT objects and either libc.so's input script or libc.so.6 plus
libc_nonshared.a, including arguments, environment, buffered stdio, constructor
priorities, atexit and destructors. The native compiler driver selects this C
startup path for hosted builds and retains its own syscall startup for
freestanding builds.

Dynamic output supports ELF TLS templates and PT_TLS, with local-exec,
initial-exec, general-dynamic and local-dynamic code sequences. Input relocations
`TLSGD`, `TLSLD`, `DTPOFF32`, `DTPOFF64`, `GOTTPOFF` and `TPOFF32` generate the
required module/offset GOT slots and dynamic TLS relocations. GCC/Clang fixtures
cover initialized/zero-filled and over-aligned templates, imported TLS, executable
preemption and per-thread isolation. GNU2 TLSDESC input (`GOTPC32_TLSDESC`/`TLSDESC_CALL`) emits eager two-word
descriptors with `R_X86_64_TLSDESC`. Local descriptors use local TLS dynsym entries;
GCC module-base references bind locally. Weak undefined descriptors and late
loading with existing threads/large TLS templates are tested. There is no TLS
instruction relaxation or common TLS. Static output supports local-exec,
initial-exec and DTPOFF references; runtime startup must copy/zero PT_TLS and
install the thread pointer. Both self-contained startup and musl CRT are tested.

Implicit input scripts accept `INPUT`, `GROUP`, nested `AS_NEEDED`, `SEARCH_DIR` and
`OUTPUT_FORMAT(elf64-x86-64)`, with bounded comments/quoted paths and cycle checks.
Relative paths search the script directory, working directory and explicit `-L`
and script search directories. Command-line directories take precedence over
script directories. Every `SEARCH_DIR` in a script applies to that whole script
and subsequent inputs; it does not affect earlier command-line `-l` searches.
`-Bstatic` also applies to libraries referenced inside following scripts.
Absolute input paths in a script canonically located inside the sysroot resolve
under that root. An outside script keeps ordinary absolute host input paths.
Either location can force a target path with `=/lib/file` or `$SYSROOT/lib/file`.
File prefixes concatenate literally (`=lib/file` does not insert a slash), unlike
directory prefixes. A missing rooted input never falls back to a host file.
This covers the tested system glibc script and isolated rooted glibc/musl cases.
`SECTIONS`, assignments, `INCLUDE` and explicit `-T` scripts remain unsupported.
Included inputs receive the same output/map alias protection as direct inputs.

Keep C++ at the tested usable interoperability level; broader ecosystem expansion
is outside the current scope. Text relocations remain unsupported, and a
multi-sysroot compatibility claim needs further qualification.
Imported IFUNC calls/function pointers and LD_PRELOAD
overrides are tested. The dynamic loader handles transitive DSO dependencies; this
linker does not fully diagnose those dependencies' unresolved references.

### Shared libraries and hardening (v0.6 declared subset)

`-shared` emits a DSO without an interpreter or entry point; `-soname NAME`
sets DT_SONAME. A supplied `--dynamic-linker` is ignored for shared output, as in
GNU ld; combining `-shared` and `-pie` remains invalid. Visible definitions are
exports and section-GC roots. Default
definitions participate in runtime symbol lookup, including references within
their defining DSO. Protected/hidden definitions bind locally. Unresolved visible
references are allowed by default for loader/host resolution; `--no-undefined`
or `-z defs` rejects unresolved strong references, and `-z undefs` restores the
permissive policy. Preinit arrays and local-exec TLS are invalid in DSO output.
GD/LD/IE TLS and TLSDESC, including late `dlopen` with pre-existing threads,
have runtime fixtures.
Native Fas objects can produce versioned DSOs consumed by Fas and GCC programs.

Dynamic output enables full RELRO with eager binding by default. GOT, `.dynamic`,
initialization arrays and `.data.rel.ro` contributions occupy complete protected
pages before mutable data and TLS. Tests inspect `/proc/self/maps` and verify
write protection in child processes. `-z norelro` disables protection; `-z relro`
restores it. `-z now` is accepted; lazy binding remains unsupported. Shared
libraries are published with mode 0644, executables with mode 0755.

`--version-script PATH` supports named nodes with one earlier parent, or one
anonymous node for export policy. Rules accept exact symbol names and terminal
`*` patterns under `global:`/`local:`; the more specific match wins, with global
winning ties. Ambiguous version assignments fail. Named exports generate
VERDEF/VERSYM and coexist with required import versions. Scripts are limited to
1 MiB, 256 nodes, 1,024 patterns and bounded matching work. Other glob syntax,
quoted/demangled names and forward parents fail explicitly.
Version files receive executable/map alias protection. Broader version-script
semantics need their own compatibility tests.

GNU assembler `.symver` aliases support multiple named versions (`name@VERSION`)
and one default (`name@@VERSION`), with matching named version-script nodes.
Default aliases resolve unversioned/explicit references and lazy archive indexes.
Only one dynsym is emitted for the default and its resolution aliases. Functions,
TLS and IFUNC aliases are tested with Fas and GNU consumers and runtime
`dlvsym`. Explicit aliases retain their named versions over wildcard export
rules; exact local rules can hide them. This matches the tested LLD policy;
GNU ld differs for some local/default combinations. Empty/base aliases (`name@`)
and defined aliases in executable output are explicitly unsupported.

Defined IFUNC resolvers are supported in loader-backed executables and DSOs.
Default-visible shared definitions remain preemptible; local/hidden/protected
references use IRELATIVE or canonical PLT addresses when required by direct PC32
references. Ordinary relocations precede resolver execution. IFUNC entry points
fail explicitly. Nonzero addends on a local IFUNC address
require a canonical PLT reference. GCC/Clang, GNU ld and LLD fixtures cover calls,
function pointers, dependency relocation ordering, visibility and runtime lookup.
On this host GNU ld PIE output disagrees on some local pointer identities;
those fixtures retain its call oracle and separately check Fas/LLD identities.

Static ET_EXEC IFUNCs use canonical `.iplt` stubs, writable `.got.iplt` slots and
symbol-zero IRELATIVE records in `.rela.iplt`. Startup code must apply the records
between `__rela_iplt_start` and `__rela_iplt_end` before calling IFUNCs; the linker
does not inject a runtime. Unused boundaries resolve to zero when there are no
records. Resolvers must be defined in executable sections. GCC/Clang PIC/non-PIC,
local/global function pointers, partial links, GC and repeated/concurrent library
invocations have deterministic tests. Only IFUNCs referenced by live allocatable
relocations receive startup records; an unused definition or debug-only reference
does not execute its resolver.

Installed static glibc CRT/archive tests run against Fas, GNU ld and LLD, covering
IFUNC startup, arguments/environment, threads, aligned TLS, memory functions and
constructor/atexit/destructor order. Actual GCC/Clang `-static` driver invocations
also pass; GCC requires `-fno-use-linker-plugin`. The recorded sysroot includes
static libc/libgcc archive and CRT hashes.

`-static-pie`, or `-static -pie --no-dynamic-linker`, emits ET_DYN with PT_DYNAMIC
and PT_PHDR, no interpreter and no DT_NEEDED. Library searches are archive-only;
explicit shared-library inputs fail. Provide PIC-compatible objects and an
appropriate self-relocating CRT (`rcrt1.o` for the tested glibc/musl versions).
The linker emits RELATIVE/IRELATIVE records; the CRT applies them. It does not
inject startup code or a relocation runtime. Glibc startup, GCC/Clang inputs,
independent GNU/LLD comparisons, ASLR, pointers, lifecycle, threaded/aligned TLS,
glibc IFUNC ordering and actual GCC `-static-pie` invocation are tested. Isolated
musl 1.2.5 CRT startup, threads, TLS and lifecycle pass separately. GNU and Fas
agree on the declared absent-weak-call case: a guarded PLT32 call may remain
unresolved, while its GOT test stays zero. Other PC-relative absolute/undefined
addresses retain explicit diagnostics.

Before CRT relocation, glibc entry code needs local addresses and calls without
an initialized GOT. Static PIE converts validated 64-bit RIP-relative MOV
`REX_GOTPCRELX` instructions to LEA and indirect `GOTPCRELX` calls to direct
calls of the same length. Targets must be defined, non-TLS and non-IFUNC;
absolute, unresolved and other instruction forms retain their GOT semantics.
Instruction fields covered by another relocation are not rewritten. Input data
is copied before patching, and repeat/concurrent library invocations remain
deterministic. This is not general instruction or TLS relaxation. Static PIE
currently supports only local-exec/initial-exec TLS and DTPOFF references; it
rejects GD/LD/TLSDESC models requiring loader support or further relaxation.
Musl IFUNCs and static-PIE dlopen are not qualified by these tests.

For a simple freestanding C fixture:

```sh
gcc -c -O2 -g -ffreestanding -fPIC answer.c -o answer.o
```

GCC and Clang fixtures no longer need the previous metadata-disabling flags.
Supply any required freestanding runtime symbols yourself; this does not provide
general hosted C compatibility or automatic libc/startup integration.

Initial resource limits are explicit: 1,024 input files, 128 MiB per file,
256 MiB combined physical input and decoded debug storage, 512 MiB output, 1 GiB section/BSS layout size, alignment
up to 2 MiB, and fewer than `0xff00` output sections. Each input allows at most
one million symbols, one million relocations, 64 MiB of decoded names, and
65,535 bytes per name. Archives allow 65,536 members and one million index entries;
an invocation allows 65,536 extracted objects, four million symbols and four
million relocations. Response files allow 16 levels, 16 MiB combined contents,
and 100,000 expanded arguments; groups allow 16 levels. The limits constrain
pathological input work; they do
not bound the process's peak RSS to the input size. Increase them only with
corresponding scale and resource tests.
Current large-layout checks inspect a roughly 1 GiB NOBITS image and relocated
end pointers against GNU ld, without running that image. They also exercise
aggregate BSS overflow, individual section limits and removal of large dead
sections. Existing end-to-end tests reject overflowing 32-bit signed/unsigned
and PC-relative relocations while preserving prior output. The separate
[large-input runner](scale/README.md) qualifies declared file-backed, dense
symbol/relocation and section/file-count cases with retained replay artifacts.
Address-space gaps beyond the current cap, peak memory and sustained real large
program loads remain unqualified.

## Library and tests

The native compiler can build freestanding programs without invoking an
assembler or external linker:

```sh
dune build --profile release bin/main.exe
_build/default/bin/main.exe --backend=native -c library.fas -o library.o
_build/default/bin/main.exe --backend=native --freestanding main.fas library.o -o program
# C inputs select the C runtime automatically; Fas still performs the final link.
_build/default/bin/main.exe --backend=native main.fas helper.c -o hosted-program
# Explicitly request C startup even for a closed Fas program.
_build/default/bin/main.exe --backend=native --hosted main.fas -o hosted-program
```

Object output has no generated `_start` and does not require `main`. Native
`-c` combines embedded C and assembly with the generated Fas object using the
linker's ET_REL combiner. References to libc or other future inputs remain
undefined; no CRT or final executable is linked. Duplicate definitions fail
before replacing the previous output. Package libraries still require a final
link and are rejected by native object output.

Executable
output defaults to freestanding for closed Fas programs: it adds argc/argv
startup and a Linux exit syscall without invoking C tools. C/header/assembly or
package imports, additional objects/archives/shared libraries, `-l` inputs,
external declarations/imported globals, or a main supplied by another input
conservatively select hosted startup. Linker policy flags alone do not change
this choice. `--hosted` and `--freestanding` explicitly override it and require
a native executable output; they do not apply to `-c`.

Hosted links call the configured `FAS_CC` GCC/Clang driver with `-###` to discover
the platform's CRT objects, interpreter and runtime library order. Fas parses
that plan and links it itself; no printed command or external linker is
executed. The compiler is a bootstrap dependency for this discovery, and also
compiles positional `.c` inputs and embedded C/assembly. No LLVM `opt`/`llc` tools
are used. C header imports and embedded assembly retain their existing Clang
requirement. For a link
with explicit CRT inputs and no C driver discovery, emit a native object with
`-c` and invoke `fas-ld` directly.

Hosted output is non-PIE by default to match the native code generator. Forward
`-Wl,-pie`, `-Wl,-static` or `-Wl,-static-pie` to select the matching C startup
plan; glibc covers all four modes and isolated musl covers dynamic fixed/PIE and
static startup. These tests do not establish that every native instruction/data
pattern is position independent. C `main` harnesses can call exported Fas
functions without any Fas `main`. Constructors, buffered stdio, destructors,
package `-pthread`, C source flags and argument passing have driver regressions.
Native sanitizer options fail explicitly rather than producing uninstrumented
output.

Use `extern "C"` exports
for symbols that must be visible between objects; ordinary definitions remain
local. Separate-object external calls currently support only non-variadic
i32/i64/pointer parameters and results (or void results), checked against GCC.
Internal Fas float/vector calls remain supported. Exported external float/vector
or aggregate ABIs, native `-S`, and other targets fail explicitly. The backend
owns ABI classification independently of the driver's runtime choice.
Additional objects/archives and explicit `-L`/`-l` use
the standalone linker's rules. Use `-Wl,--gc-sections,--start-group`,
`-Wl,--end-group`, `-Wl,--whole-archive`, or `-Xlinker @inputs.rsp` for linker
policy/response arguments. Compiler output/entry/map/statistics overrides are
rejected among forwarded arguments.
Compiler output uses atomic publication and preserves an existing file on native
build failure. The compiler itself still needs OCaml and is not self-hosted;
the linker library's concurrency guarantee does not extend to the native code
generator's global constant pools. Self-hosting remains a later Fas language gate.

`Fas_linker.Static_link.link` returns executable bytes;
`Fas_linker.Static_link.link_files` publishes an executable. Both own all mutable
invocation state and report errors through `Fas_linker.Error.Error`.
`link_inputs`/`link_input_files` accept an `Input.t` tree for groups and
whole-archive mode; `Input.Object` supplies in-memory ET_REL bytes.
`link_inputs_with_map` returns executable bytes and map text; file APIs accept
optional `map_file`. Optional `Stats.t` instances collect invocation statistics;
each caller must own its instance, including in concurrent invocations.

The tests cover exact signed/unsigned relocation boundaries, independent GNU
assembly and GCC/Clang objects, cross-object calls and function/data pointers,
absolute/section symbols, positive/negative addends, over-aligned data, BSS zero
fill, symbol conflicts, GNU ld/LLD execution parity, seeded valid object graphs,
ELF structure and permissions, malformed tables/ranges/indices, truncations,
256 deterministic object mutations and 256 archive mutations, atomic-publication failure paths, byte
reproducibility, and repeated/concurrent library invocations.
Archive tests cover GNU ld precedence/extraction oracles, forward-pass weak
selection, cycles, whole/unindexed archives, 64-bit indexes, long names, seeded
archive graphs, quoted/nested response files, and statistics.
Object-emission tests use GNU readelf/ld independently. Native-driver tests run
the actual integer/float/vector/argc fixtures with tool lookup disabled, link
separate Fas objects and archives, exercise GCC scalar ABI boundaries, and check
failure preservation. Layout tests verify map rows and predefined values against
independently decoded ELF sections, symbols and program headers.
Section/PIC tests cover COMDAT selection, GC graphs/roots, merge aliases/interior
pointers, GOT formulas/slot contents and GCC/Clang PIC. Metadata tests include
malformed groups/addrsig/properties/unwind records, seeded mutations, GNU property
merge oracles, DWARF 4/5 and GDB checks with and without frame pointers.
Exception tests cover GCC/Clang personality/LSDA records, live metadata GC edges,
cleanup/rethrows and RTTI/lifecycle across independently linked DSOs, plus
augmentation bounds, unsupported encodings and 256 seeded record mutations.

The [fresh Ubuntu qualification](corpus/results/2026-11-06-clean/README.md)
records the shared correctness recipe on GCC 13/Clang 18, glibc 2.39 and isolated
musl, including real zlib/Chibicc/SQLite and installation checks. It also records
that workstation regression and musl matrix, the unwind/debug-reader
repairs it exposed, exact binary identities and remaining release limits.
The [release archive checkpoint](corpus/results/2026-11-06-release/README.md)
adds input/resource failure recovery, 259 linker cases, repeated native Sieve
acceptance and verified binary/source archives with a clean source rebuild.

The seeded mutation tests are regression checks. Coverage-guided campaigns use
the separate [AFL++ harness](fuzz/README.md); their recorded duration and findings
must be assessed separately from integration-test results. A short clean campaign
does not establish production readiness. Performance scalability remains a later
acceptance gate.

## Performance baseline

```sh
dune build --profile release src/linker/bin/main.exe
python3 src/linker/bench.py _build/default/src/linker/bin/main.exe --output /tmp/fas-linker-bench.json
python3 src/linker/bench.py _build/default/src/linker/bin/main.exe --archive all --statistics
python3 src/linker/bench.py _build/default/src/linker/bin/main.exe --archive lazy --statistics
```

This generates a repeatable object corpus outside timed runs, warms the inputs,
rotates tool order across seven measured runs, executes each result, and records
wall time through exit, CPU time, peak RSS, output size, tool versions, CPU, and
filesystem. GNU ld is required; LLD and mold are included when installed.
GNU time is required for a separate memory measurement so the Python parent's
resident-memory footprint does not impose a floor on reported linker RSS.
Numbers describe this small static workload with default worker counts. They
are not evidence of general compatibility or performance against those linkers.

See the [initial measured baseline](benchmarks/README.md) and its raw samples.

The implementation is single-worker per link. `--stats` writes JSON to stderr:
wall/CPU time, physical inputs, loaded objects/members, symbols/relocations,
output size, domain allocation/promotion words and sampled process collection
counts. Phase measurements are inclusive: `resolve` includes extracted ELF
parsing and `emit` includes relocation. Do not add overlapping phases together.
Statistics do not force a collection. Wall time uses the system clock; allocation
counters are domain-local and collection counts are sampled by OCaml's runtime.
Larger/debug-heavy corpora, cold-cache runs and parallel scaling remain future work.

Format references: [ELF sections](https://gabi.xinuos.com/elf/03-sheader.html),
[symbols](https://gabi.xinuos.com/elf/05-symtab.html),
[relocations](https://gabi.xinuos.com/elf/06-reloc.html),
[program loading](https://gabi.xinuos.com/elf/07-pheader.html), and the
[x86-64 psABI](https://gitlab.com/x86-psABIs/x86-64-ABI).


### Broader compatibility work (v0.7)

Non-allocatable `.debug_*` PROGBITS with ELF SHF_COMPRESSED/zlib are decoded by
an independent OCaml RFC 1950/1951 implementation; no zlib native dependency is
added. Stored/fixed/dynamic Huffman blocks, checksums, windows, declared sizes and
malformed streams are checked. Output debug sections are uncompressed. Decoded
storage is limited to 128 MiB per object and shares the 256 MiB invocation input
budget. Relocations use uncompressed offsets and source lookup is tested.
The decoder passes 3,550 differential cases against Python zlib. zstd and legacy
`.zdebug_*` formats are not supported.

On-demand `__preinit_array_start/end`, `__init_array_start/end` and
`__fini_array_start/end` symbols support CRT-owned lifecycle iteration, including
static musl. Empty arrays yield an equal empty range; user definitions retain
precedence. See the [real-program corpus](corpus/README.md) and its recorded
results for Chibicc, all four Sieve sources, Doom startup, bounded headless Wolf3D
rendering, cJSON's 21 upstream tests and the isolated musl 1.2.5 sysroot. These
results do not establish full Linux compatibility or production readiness.

Decoder references: [RFC 1950](https://www.rfc-editor.org/rfc/rfc1950),
[RFC 1951](https://www.rfc-editor.org/rfc/rfc1951).
