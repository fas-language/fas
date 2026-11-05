#!/usr/bin/env python3
"""Real Fas native objects, independent toolchain interoperability, tool-free links."""
import os
from pathlib import Path
import shutil
import struct
import subprocess
import sys
import tempfile
import unittest

COMPILER = str(Path(sys.argv.pop(1)).resolve())
LINKER = str(Path(sys.argv.pop(1)).resolve())
FIXTURES = Path(__file__).resolve().parents[3] / "test"


def run(*args, expected=0, env=None):
    result = subprocess.run([str(a) for a in args], capture_output=True, timeout=30, env=env)
    if result.returncode != expected:
        raise AssertionError(f'{args}: exit {result.returncode}, expected {expected}\n'
                             + result.stderr.decode(errors="replace"))
    return result


def symbols(path):
    data = path.read_bytes()
    shoff = struct.unpack_from("<Q", data, 40)[0]
    count = struct.unpack_from("<H", data, 60)[0]
    sections = [struct.unpack_from("<IIQQQQIIQQ", data, shoff + n * 64) for n in range(count)]
    table = next(s for s in sections if s[1] == 2)
    strings = sections[table[6]]
    names = data[strings[4]:strings[4] + strings[5]]
    result = {}
    for at in range(table[4], table[4] + table[5], 24):
        name, info, other, section, value, size = struct.unpack_from("<IBBHQQ", data, at)
        result[names[name:].split(b"\0", 1)[0].decode()] = dict(info=info, other=other,
                                                               section=section, value=value, size=size)
    return result


class NativeTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.temp = tempfile.TemporaryDirectory(prefix="fas-native-linker-tests-")
        cls.root = Path(cls.temp.name)
        empty_path = cls.root / "no-tools"
        empty_path.mkdir()
        cls.no_tools = dict(os.environ, PATH=str(empty_path), FAS_OPT="/nonexistent/opt",
                            FAS_LLC="/nonexistent/llc", FAS_CC="/nonexistent/cc")

    @classmethod
    def tearDownClass(cls):
        cls.temp.cleanup()

    def source(self, name, text):
        path = self.root / (name + ".fas")
        path.write_text(text)
        return path

    def compile(self, name, source, *inputs, object=False, extra=(), env=None):
        output = self.root / name
        run(COMPILER, "--backend=native", *(("-c",) if object else ("--freestanding",)), source,
            *inputs, *extra, "-o", output, env=self.no_tools if env is None else env)
        return output

    def test_tool_free_separate_objects_and_archive(self):
        library = self.source("separate-library", '''
fn helper(x i64) i64 { return x + 2 }
extern "C" { fn answer(x i64) i64 { return helper(x) } }
''')
        main = self.source("separate-main", '''
extern "C" { fn answer(x i64) i64 }
fn helper() i32 { return trunc[i32](answer(40)) }
fn main() i32 { return helper() }
''')
        bootstrap = self.source("bootstrap", "fn anchor() void {}\n")
        env = dict(self.no_tools, FAS_NATIVE_NOINLINE="1")
        libobj = self.compile("separate-library.o", library, object=True, env=env)
        mainobj = self.compile("separate-main.o", main, object=True, env=env)
        for path in (libobj, mainobj):
            self.assertEqual(struct.unpack_from("<H", path.read_bytes(), 16)[0], 1)
            self.assertNotIn("_start", symbols(path))
        self.assertEqual(symbols(libobj)["answer"]["info"] >> 4, 1)
        self.assertEqual(symbols(mainobj)["main"]["info"] >> 4, 1)
        run(self.compile("separate-program", main, libobj), expected=42)
        run(self.compile("separate-all-objects", bootstrap, mainobj, libobj), expected=42)
        archive = self.root / "libseparate.a"
        run("ar", "rcs", archive, libobj)
        run(self.compile("separate-archive", bootstrap, mainobj, archive), expected=42)
        run(self.compile("separate-search", bootstrap, mainobj, extra=("-L", self.root, "-lseparate")), expected=42)
        result = run(COMPILER, "--backend=native", "--freestanding", bootstrap, archive, mainobj,
                     "-o", self.root / "wrong-order", expected=1, env=self.no_tools)
        self.assertIn(b"undefined symbol answer", result.stderr)
        run(self.compile("separate-group", bootstrap,
                         extra=("-Wl,--gc-sections,--start-group", archive, mainobj,
                                "-Xlinker", "--end-group")), expected=42)
        response = self.root / "native link inputs.rsp"
        response.write_text(f'--gc-sections --start-group "{archive}" "{mainobj}" --end-group')
        run(self.compile("separate-response", bootstrap,
                         extra=("-Xlinker", "@" + str(response))), expected=42)
        for flags, diagnostic, status in ((("-Wl,",), b"nonempty", 2),
                                  (("-Xlinker",), b"requires an argument", 2),
                                  (("-Wl,--output,elsewhere",), b"not accepted", 1)):
            result = run(COMPILER, "--backend=native", bootstrap, *flags,
                         expected=status, env=self.no_tools)
            self.assertIn(diagnostic, result.stderr)

    def test_object_relocations_and_independent_linkers(self):
        source = self.source("object-data", '''
struct Aligned @align(4096) { value u64 }
var initialized Aligned = {42}
var zeroed Aligned
var pointer addr = &initialized.value
const entries arr[2,addr] = {&initialized, &zeroed}
fn main() i32 {
  if addr_bits(&initialized) % 4096 != 0 { return 1 }
  if addr_bits(&zeroed) % 4096 != 0 { return 2 }
  if zeroed.value != 0 { return 3 }
  if pointer != entries[0] { return 4 }
  return trunc[i32](pointer[u64])
}
''')
        obj = self.compile("object-data.o", source, object=True)
        report = run("readelf", "-aW", obj)
        self.assertNotIn(b"Warning:", report.stderr)
        self.assertNotIn(b"Error:", report.stderr)
        self.assertIn(b"R_X86_64_PC32", report.stdout)
        self.assertIn(b"R_X86_64_64", report.stdout)
        startup = self.root / "reference-start.s"
        startup.write_text('.text\n.globl _start\n_start: call main; mov %eax,%edi; mov $60,%eax; syscall\n.section .note.GNU-stack,"",@progbits\n')
        startobj = startup.with_suffix(".o")
        run("as", "--64", "-o", startobj, startup)
        for tool in ("ld", LINKER):
            output = self.root / "independent-link"
            run(tool, "-o", output, startobj, obj)
            run(output, expected=42)
        run(self.compile("aligned-native", source), expected=42)

    def test_integer_pointer_sysv_boundary_with_gcc(self):
        library = self.source("integer-abi", '''
extern "C" {
  var fas_storage u64 = 7
  fn sum8(a u64,b u64,c u64,d u64,e u64,f u64,g u64,h u64) u64 {
    return a + 2*b + 3*c + 4*d + 5*e + 6*f + 7*g + 8*h
  }
  fn change(p addr, delta u64) u64 {
    p[u64] += delta
    return p[u64] + fas_storage
  }
}
''')
        primary = self.source("integer-abi-main", '''
extern "C" { fn check() i32
  var c_value u32 }
fn main() i32 { if c_value != 42 { return 1 } return check() }
''')
        libobj = self.compile("integer-abi.o", library, object=True)
        c = self.root / "integer-abi-check.c"
        c.write_text('''
extern unsigned long sum8(unsigned long,unsigned long,unsigned long,unsigned long,
                          unsigned long,unsigned long,unsigned long,unsigned long);
extern unsigned long change(void *, unsigned long);
extern unsigned long fas_storage;
unsigned c_value = 42;
int check(void) {
  unsigned long x = 11;
  return sum8(1,2,3,4,5,6,7,8) == 204 && change(&x,24) == 42
         && x == 35 && fas_storage == 7 ? 42 : 2;
}
''')
        cobj = c.with_suffix(".o")
        run("gcc", "-c", "-O0", "-ffreestanding", "-fno-pic", "-fno-pie",
            "-fno-stack-protector", "-fno-unwind-tables", "-fno-asynchronous-unwind-tables",
            "-fno-ident", "-fcf-protection=none", "-o", cobj, c)
        run(self.compile("integer-abi-program", primary, cobj, libobj), expected=42)

    def test_shared_library_calls_without_external_linker(self):
        source = self.source("dynamic-native", '''
extern "C" { fn answer(x i64) i64 }
fn main() i32 { return trunc[i32](answer(40)) }
''')
        c = self.root / "dynamic-native.c"
        c.write_text("long answer(long x) { return x + 2; }\n")
        library = self.root / "libnative-answer.so"
        run("gcc", "-shared", "-nostdlib", "-fPIC", "-Wl,-soname,libnative-answer.so",
            "-o", library, c)
        obj = self.compile("dynamic-native.o", source, object=True)
        self.assertIn(b"R_X86_64_PLT32", run("readelf", "-rW", obj).stdout)
        for name, mode in (("dynamic-native-fixed", "--dynamic-linker,/lib64/ld-linux-x86-64.so.2"),
                           ("dynamic-native-pie", "-pie")):
            with self.subTest(mode=mode):
                output = self.compile(name, source, library,
                                      extra=("-Wl," + mode + ",-rpath,$ORIGIN",))
                report = run("readelf", "-aW", output)
                self.assertNotIn(b"Warning:", report.stderr)
                self.assertNotIn(b"Error:", report.stderr)
                run(output, expected=42)

    def test_fas_produced_versioned_dso_with_fas_and_c_consumers(self):
        source = self.source('dso-library','''
fn helper(x i64) i64 { return x + 2 }
extern "C" { fn answer(x i64) i64 { return helper(x) } }
''')
        obj = self.compile('dso-library.o',source,object=True,
                           env=dict(self.no_tools,FAS_NATIVE_NOINLINE='1'))
        script = self.root/'fas-exports.map'
        script.write_text('FAS_1 { global: answer; local: *; };')
        library = self.root/'libfas-answer.so'
        run(LINKER,'-shared','-soname',library.name,'--gc-sections',
            '--version-script',script,'-o',library,obj,env=self.no_tools)
        self.assertIn(b'answer@@FAS_1',run('readelf','--dyn-syms','-W',library).stdout)
        main = self.source('dso-fas-main','''
extern "C" { fn answer(x i64) i64 }
fn main() i32 { return trunc[i32](answer(40)) }
''')
        out = self.compile('dso-fas-main',main,library,extra=('-Wl,-pie,-rpath,$ORIGIN',))
        run(out,expected=42)
        c = self.root/'dso-c-main.c'
        c.write_text('extern long answer(long); int main(void){return (int)answer(40);}')
        out = self.root/'dso-c-main'
        run('gcc','-fPIC',c,library,'-Wl,-rpath,$ORIGIN','-o',out)
        run(out,expected=42)

    def test_fas_object_with_system_c_startup_and_lifecycle(self):
        source = self.source("hosted-fas", '''
extern "C" { fn check_initialization() i32 }
fn main() i32 { return check_initialization() }
''')
        obj = self.compile("hosted-fas.o", source, object=True)
        c = self.root / "hosted-fas-lifecycle.c"
        c.write_text('''
#include <stdio.h>
static int initialized;
__attribute__((constructor)) static void initialize(void){
  initialized=7;puts("initialized");
}
__attribute__((destructor)) static void finalize(void){puts("finalized");}
int check_initialization(void){return initialized+35;}
''')
        cobj = c.with_suffix('.o')
        run('gcc','-c','-fPIC','-O2',c,'-o',cobj)
        def crt(name):
            return run('gcc','-print-file-name='+name).stdout.decode().strip()
        for pie in (False, True):
            out = self.root / 'hosted-fas'
            run(LINKER, *('-pie',) if pie else (), '--dynamic-linker',
                '/lib64/ld-linux-x86-64.so.2', '--gc-sections', '-o',out,
                crt('Scrt1.o' if pie else 'crt1.o'),crt('crti.o'),crt('crtbeginS.o'),
                obj,cobj,'/usr/lib/x86_64-linux-gnu/libc.so',crt('crtendS.o'),crt('crtn.o'),
                env=self.no_tools)
            self.assertEqual(run(out,expected=42).stdout,b'initialized\nfinalized\n')

    def test_existing_freestanding_fixture_corpus(self):
        for name, status in (("native_backend", 3), ("native_floats", 0), ("native_vectors", 0), ("entry_args", 0)):
            with self.subTest(name=name):
                source = FIXTURES / (name + ".fas")
                native = self.compile(name + "-new-linker", source)
                result = run(native, expected=status)
                if name == "entry_args":
                    run(native, "one", "two", expected=2)
                elif all(shutil.which(tool) for tool in ("opt-22", "llc-22", "clang-22")):
                    reference = self.root / (name + "-llvm-reference")
                    env = dict(os.environ, FAS_OPT="opt-22", FAS_LLC="llc-22", FAS_CC="clang-22")
                    run(COMPILER, "-O2", source, "-o", reference, env=env)
                    self.assertEqual(result.stdout, run(reference, expected=status).stdout)

    def test_unsupported_abi_target_and_output_mode(self):
        float_export = self.source("float-export", 'extern "C" { fn f(x f64) f64 { return x } }\n')
        result = run(COMPILER, "--backend=native", "-c", float_export, "-o", self.root / "unsupported.o",
                     env=self.no_tools, expected=1)
        self.assertIn(b"unsupported external ABI", result.stderr)
        basic = self.source("target", "fn main() i32 { return 0 }\n")
        for os_name in ("windows", "wasi"):
            result = run(COMPILER, "--backend=native", "--os=" + os_name, basic,
                         "-o", self.root / "wrong-target", env=self.no_tools, expected=1)
            self.assertIn(b"Linux x86-64", result.stderr)
        result = run(COMPILER, "--backend=native", "-S", basic, "-o", self.root / "native.s",
                     env=self.no_tools, expected=1)
        self.assertIn(b"native assembly text output is not supported", result.stderr)

    def test_failures_preserve_outputs_and_source_aliases(self):
        source = self.source("missing-function", 'extern "C" { fn missing() i32 }\nfn main() i32 { return missing() }\n')
        out = self.root / "preserved-native"
        sentinel = b"previous executable\0\xff"
        out.write_bytes(sentinel)
        out.chmod(0o640)
        run(COMPILER, "--backend=native", source, "-o", out, env=self.no_tools, expected=1)
        self.assertEqual(out.read_bytes(), sentinel)
        self.assertEqual(out.stat().st_mode & 0o777, 0o640)
        basic = self.source("alias-source", "fn main() i32 { return 0 }\n")
        before = basic.read_bytes()
        alias = self.root / "source-alias"
        os.link(basic, alias)
        for flags in ((), ("-c",)):
            result = run(COMPILER, "--backend=native", *flags, basic, "-o", alias,
                         env=self.no_tools, expected=1)
            self.assertIn(b"overwrite input", result.stderr)
            self.assertEqual(basic.read_bytes(), before)
        self.assertEqual(list(self.root.glob(".fas-ld-*.tmp")), [])


if __name__ == "__main__":
    unittest.main(verbosity=2)
