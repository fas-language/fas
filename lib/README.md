# The compiler's library

The library core that the Fas rewrites of the toolchain are built on,
starting with the linker (roadmap v0.8). It has **no stability promise**
until Fas hosts itself: functions change whenever the compiler needs them
to.

`use "lib/core.fas"` (relative to the using file) brings in all of it.
Programs that use it need no C: with no `use "C"` they build as static
executables that talk to Linux x86-64 directly through `syscall`.

| File | What |
| --- | --- |
| `os.fas` | system calls: read/write (with whole-buffer loops), open, close, stat, rename, unlink, readlink, getcwd, mmap, clocks, exit, futexes and `os_lock`; arguments and environment (`os_init(argc, argv)` first in `main`) |
| `mem.fas` | `mem_copy`/`mem_move`/`mem_set`/`mem_eq`; the heap (`heap_alloc`, `heap_realloc`, `heap_free`); arenas (`arena_alloc`, `arena_free`) |
| `bytes.fas` | `Bytes`, a borrowed (address, length) value: compare, search, slice, hash, parse numbers |
| `buf.fas` | `Buf`, a growable byte buffer: little-endian integers, alignment padding, decimal and hex text, writing out |
| `vec.fas` | `Vec[T]`, a growable array |
| `map.fas` | `Map[K, V, HASH, EQ]`, an open-addressing hash map |
| `sort.fas` | `sort[T, LESS]`, a stable merge sort |
| `intern.fas` | `Intern`: byte strings to dense `u32` ids |
| `file.fas` | read a whole regular file; write one atomically (temporary file and rename) |
| `thread.fas` | `Pool`: worker threads (clone through a small assembly trampoline) that run `pool_for` parallel loops; not in `core.fas`, `use` it on its own |
| `path.fas` | `dirname`, `basename`, `concat`, `is_relative` with the exact results of OCaml's `Filename`; `realpath` |

## Rules

- **Names.** Fas has one namespace across all `use`d files, so every
  function carries its module's prefix (`os_`, `heap_`, `arena_`, `pool_`, `thread_`, `bytes_`,
  `buf_`, `vec_`, `map_`, `sort`, `intern`, `file_`, `path_`). Programs
  using the library should not define those prefixes themselves. The older
  `std/sys.fas` is separate; do not use both in one program.
- **Memory never fails.** When the kernel refuses memory, the library
  prints `fas library: out of memory` and exits with status 2. Callers
  never write `try` for an allocation.
- **The OS fails with errno.** Calls that reach the kernel are `fails`,
  and the error is the kernel's errno: `NotFound` is ENOENT (2), `Access`
  is EACCES (13), and so on (`os.fas` names the common ones; `err_name`
  prints any other as `errno N`). Library errors of its own (`TooBig`,
  `NotRegular`, `BadNumber`, ...) have no pinned number.
- **Ownership.** `Buf`, `Vec`, `Map`, `Intern` and `Arena` start as `{}`
  and own their memory until their `_free`; they are passed by handle
  (`handle_from_addr[Buf](&b)`). `Bytes` never owns anything.
- **Determinism.** Nothing depends on addresses or on time except the
  temporary file names of `file_write_atomic`. Map iteration order depends
  only on the insertions and removals, but sort keys before output that must be
  byte-identical to the OCaml tools.
- **Threads.** The heap and `os_lock` work from any thread; an `Arena`,
  `Buf`, `Vec`, `Map` or `Intern` belongs to one thread at a time. Fas has
  no thread-local storage, so a `pool_for` job gets its worker number and
  keeps per-worker state in arrays indexed by it.
- **Correctness first, speed by design.** The data structures and the
  thread pool are built for the linker's speed; the inner loops stay plain
  until backend 1.0.

## Tests

`test/lib.sh` builds `test/lib/lib.fas` with the native backend and with
LLVM, at O0 and O2. The program checks every module against reference
implementations (random operations checked step by step), and the script
compares the path results with OCaml's `Filename` (`test/lib/paths.out`),
`realpath` with the system's, and checks that atomic writes leave no
temporary files.
`test/lib/thread.fas` checks that parallel loops cover every item once,
that thousands of short jobs hand off cleanly, that the heap and locks
work from workers, and that a pool stops and starts again (1, 2, 3, 16
threads and one per CPU).
