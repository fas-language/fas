#!/usr/bin/env python3
"""Native driver CRT selection, C compilation, libraries and atomic failures."""
import os
from pathlib import Path
import shutil
import shlex
import subprocess
import tempfile
import unittest

import native_integration as native

run = native.run
COMPILER = native.COMPILER


class HostedNativeTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.temp = tempfile.TemporaryDirectory(prefix="fas hosted native ")
        cls.root = Path(cls.temp.name)
        cls.ccs = list(dict.fromkeys(filter(None, (shutil.which("gcc"),
                       shutil.which("clang-22") or shutil.which("clang")))))
        if not cls.ccs:
            raise RuntimeError("hosted native integration requires GCC or Clang")

    @classmethod
    def tearDownClass(cls):
        cls.temp.cleanup()

    def source(self, name, text, suffix=".fas"):
        path = self.root / (name + suffix)
        path.write_text(text)
        return path

    def env(self, cc=None, **extra):
        return dict(os.environ, FAS_CC=cc or self.ccs[0], FAS_OPT="/nonexistent/opt",
                    FAS_LLC="/nonexistent/llc", **extra)

    def compile(self, name, source, *args, cc=None, expected=0, env=None):
        output = self.root / name
        result = run(COMPILER, "--backend=native", source, *args, "-o", output,
                     expected=expected, env=env or self.env(cc))
        return output, result

    def test_automatic_libc_startup_and_compiler_plans(self):
        source = self.source("libc", '''
extern "C" { fn puts(message addr) i32 }
fn main() i32 { puts(c"hosted hello"); return 42 }
''')
        for cc in self.ccs:
            with self.subTest(cc=cc):
                out, _ = self.compile("libc program", source, cc=cc)
                self.assertEqual(run(out, expected=42).stdout, b"hosted hello\n")
                headers = run("readelf", "-lW", out).stdout
                self.assertIn(b"INTERP", headers)
                self.assertRegex(headers, rb"GNU_STACK[^\n]* RW ")
                self.assertNotRegex(headers, rb"GNU_STACK[^\n]*RWE")

    def test_constructor_destructor_and_c_source_compilation(self):
        source = self.source("lifecycle", '''
extern "C" { fn initialized_value() i32 }
fn main() i32 { return initialized_value() }
''')
        c = self.source("lifecycle helper", '''
#include <stdio.h>
static int value;
__attribute__((constructor)) static void initialize(void){value=42;puts("init");}
__attribute__((destructor)) static void finalize(void){puts("fini");}
int initialized_value(void){return value;}
''', ".c")
        obj = c.with_suffix(".o")
        run(self.ccs[0], "-fPIC", "-c", c, "-o", obj)
        for dependency in (c, obj):
            for cc in self.ccs:
                with self.subTest(dependency=dependency, cc=cc):
                    out, _ = self.compile("lifecycle", source, dependency, cc=cc)
                    self.assertEqual(run(out, expected=42).stdout, b"init\nfini\n")

    def test_no_fas_main_c_harness_and_define_flags(self):
        source = self.source("export library", '''
extern "C" { fn answer(x i64) i64 { return x + 2 } }
''')
        c = self.source("main harness", '''
#include <stdio.h>
extern long answer(long);
int main(int argc,char **argv){puts("C main");return answer(INPUT_VALUE);}
''', ".c")
        for cc in self.ccs:
            with self.subTest(cc=cc):
                out, _ = self.compile("C main program", source, c, "-DINPUT_VALUE=40", cc=cc)
                self.assertEqual(run(out, expected=42).stdout, b"C main\n")

    def test_archive_shared_library_and_search_paths(self):
        source = self.source("library user", '''
extern "C" { fn library_answer() i32 }
fn main() i32 { return library_answer() }
''')
        c = self.source("library definition", '''
#include <stdio.h>
__attribute__((constructor)) static void initialize(void){puts("library init");}
int library_answer(void){return 42;}
''', ".c")
        obj = c.with_suffix(".o")
        run(self.ccs[0], "-c", "-fPIC", c, "-o", obj)
        archive = self.root / "libanswer.a"
        run("ar", "rcs", archive, obj)
        shared = self.root / "libanswer.so"
        run(self.ccs[0], "-shared", obj, "-Wl,-soname,libanswer.so", "-o", shared)
        for args in ((archive,), ("-L", self.root, "-lanswer", "-Wl,-rpath,$ORIGIN")):
            out, _ = self.compile("library user", source, *args)
            self.assertEqual(run(out, expected=42).stdout, b"library init\n")

    def test_embedded_c_and_header_import(self):
        cc = shutil.which("clang-22") or shutil.which("clang")
        if not cc:
            self.skipTest("Fas C header importer requires Clang")
        source = self.source("embedded C", '''
use "C" <stdio.h>
use "C" <<HELPER
int embedded_answer(void){puts("embedded C");return 42;}
HELPER
fn main() i32 { return embedded_answer() }
''')
        out, _ = self.compile("embedded C", source, cc=cc)
        self.assertEqual(run(out, expected=42).stdout, b"embedded C\n")

    def test_explicit_hosted_and_argc(self):
        source = self.source("argc", "fn main(argc i32, argv addr) i32 { return argc - 1 }\n")
        out, _ = self.compile("argc hosted", source, "--hosted")
        run(out, "one", "two", expected=2)

    def test_static_c_adapters_calls_and_callback_addresses(self):
        cc = shutil.which("clang-22") or shutil.which("clang")
        if not cc:
            self.skipTest("Fas C header importer requires Clang")
        helper = 'static int local_answer(int value){return value+1;}\n'
        header = self.source("static helper", helper, ".h")
        callback = '''
use "C" <<CALLBACK
int invoke(int (*callback)(int),int value){return callback(value);}
CALLBACK
const saved_callback addr = &local_answer
fn main() i32 {
    if local_answer(0) != 1 { return 1 }
    if invoke(&local_answer, 40) != 41 { return 2 }
    return invoke(saved_callback, 41)
}
'''
        for mode, imports in (("container", 'use "C" <<HELPER\n'+helper+'HELPER\n'),
                              ("header", 'use "C" "'+str(header)+'"\n')):
            source = self.source("static adapter "+mode, imports+callback)
            for fir in ("", "roundtrip"):
                with self.subTest(mode=mode, fir=fir):
                    out, _ = self.compile("static adapter", source,
                                          env=self.env(cc, FAS_FIR=fir))
                    run(out, expected=42)

    def test_embedded_assembly(self):
        cc = shutil.which("clang-22") or shutil.which("clang")
        if not cc:
            self.skipTest("Fas embedded assembly driver requires Clang")
        source = self.source("embedded assembly", '''
extern "C" { fn asm_answer() i32 }
use "asm" <<ASM
.text
.globl asm_answer
.type asm_answer,@function
asm_answer: mov $42,%eax; ret
.size asm_answer,.-asm_answer
.section .note.GNU-stack,"",@progbits
ASM
fn main() i32 { return asm_answer() }
''')
        out, _ = self.compile("embedded assembly", source, cc=cc)
        run(out, expected=42)

    def test_package_thread_flags_reach_c_compilation_and_link_plan(self):
        source = self.source("thread package", '''
use pkg "thread-test"
extern "C" { fn thread_answer() i32 }
fn main() i32 { return thread_answer() }
''')
        c = self.source("thread helper", '''
#ifndef _REENTRANT
#error package -pthread did not reach C compilation
#endif
#include <pthread.h>
static int result;
static void *worker(void *arg){result=42;return 0;}
int thread_answer(void){pthread_t t;if(pthread_create(&t,0,worker,0))return 1;
  if(pthread_join(t,0))return 2;return result;}
''', ".c")
        pkg = self.root / "pkg config"
        pkg.write_text('#!/bin/sh\ncase "$1" in\n--cflags|--libs) printf "%s\\n" "-pthread";;\nesac\n')
        pkg.chmod(0o755)
        out, _ = self.compile("thread package", source, c,
                              env=self.env(FAS_PKG_CONFIG=str(pkg)))
        run(out, expected=42)

    def test_hosted_pie_static_and_static_pie(self):
        source = self.source("hosted modes", '''
extern "C" { fn puts(message addr) i32 }
fn main() i32 { puts(c"runtime mode"); return 42 }
''')
        for cc in self.ccs:
            for mode in ("-pie", "-static", "-static-pie"):
                with self.subTest(cc=cc, mode=mode):
                    out, _ = self.compile("hosted modes", source, "-Wl," + mode, cc=cc)
                    self.assertEqual(run(out, expected=42).stdout, b"runtime mode\n")
                    report = run("readelf", "-hW", "-lW", out).stdout
                    self.assertIn(b"DYN" if mode != "-static" else b"EXEC", report)
                    if mode != "-pie":
                        self.assertNotIn(b"INTERP", report)

    def test_musl_compiler_plan(self):
        sysroot = os.environ.get("FAS_MUSL_SYSROOT")
        if not sysroot:
            self.skipTest("set FAS_MUSL_SYSROOT for isolated musl runtime")
        cc = str(Path(sysroot) / "bin/musl-gcc")
        source = self.source("musl driver", '''
extern "C" { fn puts(message addr) i32 }
fn main() i32 { puts(c"musl runtime"); return 42 }
''')
        for flags in ((), ("-Wl,-pie",), ("-Wl,-static",)):
            with self.subTest(flags=flags):
                out, _ = self.compile("musl driver", source, *flags, cc=cc)
                self.assertEqual(run(out, expected=42).stdout, b"musl runtime\n")

    def test_closed_and_explicit_freestanding_without_tools(self):
        empty = self.root / "empty PATH"
        empty.mkdir(exist_ok=True)
        env = self.env("/nonexistent/cc", PATH=str(empty))
        source = self.source("closed", "fn main() i32 { return 42 }\n")
        out, _ = self.compile("closed", source, env=env)
        self.assertNotIn(b"INTERP", run("readelf", "-lW", out).stdout)
        run(out, expected=42)
        out, _ = self.compile("closed with linker flags", source, "-Wl,--gc-sections", env=env)
        run(out, expected=42)
        # Lowered LLVM-shaped intrinsic declarations are implemented by the
        # native backend and must not accidentally select the C runtime.
        source = self.source("closed intrinsics", '''
fn main(argc i32, argv addr) i32 {
  bits u32 = bitcast[u32](argc)
  if popcount(bits) != 1 { return 1 }
  if ctz(bits) != 0 { return 2 }
  return 42
}
''')
        out, _ = self.compile("closed intrinsics", source, env=env)
        self.assertNotIn(b"INTERP", run("readelf", "-lW", out).stdout)
        run(out, expected=42)
        library = self.source("free library", 'extern "C" { fn answer() i32 { return 42 } }\n')
        obj = self.root / "free library.o"
        run(COMPILER, "--backend=native", "-c", library, "-o", obj, env=env)
        source = self.source("free caller", '''
extern "C" { fn answer() i32 }
fn main() i32 { return answer() }
''')
        out, _ = self.compile("free caller", source, "--freestanding", obj, env=env)
        run(out, expected=42)
        sections = run("readelf", "-SW", obj).stdout
        self.assertIn(b".note.GNU-stack", sections)

    def test_link_plan_never_executes_external_linker(self):
        tools = self.root / "forbidden linker"
        tools.mkdir()
        marker = tools / "executed"
        ld = tools / "ld"
        ld.write_text("#!/bin/sh\ntouch " + shlex.quote(str(marker)) + "\nexit 99\n")
        ld.chmod(0o755)
        wrapper = self.root / "cc wrapper"
        wrapper.write_text("#!/bin/sh\nexec " + shlex.quote(self.ccs[0]) + " "
                           + shlex.quote("-B" + str(tools) + "/") + ' "$@"\n')
        wrapper.chmod(0o755)
        source = self.source("own linker", "fn main() i32 { return 42 }\n")
        out, _ = self.compile("own linker", source, "--hosted", cc=str(wrapper))
        run(out, expected=42)
        self.assertFalse(marker.exists())

    def test_failure_preserves_outputs_and_original_c_sources(self):
        source = self.source("failing", '''
extern "C" { fn fas_missing_symbol() i32 }
fn main() i32 { return fas_missing_symbol() }
''')
        output = self.root / "preserved"
        sentinel = b"previous executable\0\xff"
        output.write_bytes(sentinel)
        output.chmod(0o640)
        _, result = self.compile("preserved", source, expected=1)
        self.assertIn(b"fas_missing_symbol", result.stderr)
        self.assertEqual(output.read_bytes(), sentinel)
        self.assertEqual(output.stat().st_mode & 0o777, 0o640)
        _, result = self.compile("preserved", source, expected=1, cc="/nonexistent/cc")
        self.assertIn(b"hosted", result.stderr)
        self.assertEqual(output.read_bytes(), sentinel)
        c = self.source("preserved C", "int main(void){return 0;}\n", ".c")
        library = self.source("empty library", "fn anchor() void {}\n")
        alias = self.root / "C source alias"
        os.link(c, alias)
        before = c.read_bytes()
        result = run(COMPILER, "--backend=native", library, c, "-o", alias,
                     expected=1, env=self.env())
        self.assertIn(b"overwrite input", result.stderr)
        self.assertEqual(c.read_bytes(), before)

    def test_invalid_modes_and_output_override(self):
        source = self.source("invalid modes", "fn main() i32 { return 0 }\n")
        for args in (("--hosted", "--freestanding"), ("--hosted", "-c"),
                     ("--sanitize=address",)):
            with self.subTest(args=args):
                self.compile("invalid modes", source, *args, expected=2)
        self.compile("invalid modes", source, "--hosted", "-Wl,-o,unexpected", expected=1)
        result = run(COMPILER, source, "--hosted", expected=2, env=self.env())
        self.assertIn(b"--backend=native executable", result.stderr)
        bad_cc = self.root / "invalid plan cc"
        bad_cc.write_text("#!/bin/sh\necho no-link-command >&2\n")
        bad_cc.chmod(0o755)
        _, result = self.compile("invalid plan", source, "--hosted", cc=str(bad_cc), expected=1)
        self.assertIn(b"expected one GCC/Clang linker command", result.stderr)


if __name__ == "__main__":
    unittest.main(verbosity=2)
