import os
import re
import subprocess
import tempfile
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
CC = os.environ.get("CC", "clang-22")
LLVM_OPT = os.environ.get("LLVM_OPT", "opt-22")
OCAML_FAS = os.environ.get("OCAML_FAS", str(ROOT / "_build/default/bin/main.exe"))


def run(args):
    subprocess.run([str(arg) for arg in args], check=True)


types = [
    ("u8", "u8", "uint8_t", "UINT8_MAX", "(uint8_t)-1", "0", "(uint8_t)0"),
    ("i8", "i8", "int8_t", "INT8_MAX", "-1", "INT8_MIN", "0"),
    ("u16", "u16", "uint16_t", "UINT16_MAX", "(uint16_t)-1", "0", "(uint16_t)0"),
    ("i16", "i16", "int16_t", "INT16_MAX", "-1", "INT16_MIN", "0"),
    ("u32", "u32", "uint32_t", "UINT32_MAX", "(uint32_t)-1", "0", "(uint32_t)0"),
    ("i32", "i32", "int32_t", "INT32_MAX", "-1", "INT32_MIN", "0"),
    ("u64", "u64", "uint64_t", "UINT64_MAX", "(uint64_t)-1", "0", "(uint64_t)0"),
    ("i64", "i64", "int64_t", "INT64_MAX", "-1", "INT64_MIN", "0"),
    ("usize", "usize", "size_t", "SIZE_MAX", "(size_t)-1", "0", "(size_t)0"),
    ("isize", "isize", "ptrdiff_t", "PTRDIFF_MAX", "-1", "PTRDIFF_MIN", "0"),
]


def fas_negative_one(name):
    if name in ("u8", "u16"):
        return f"trunc[{name}](-1)"
    if name == "u32":
        return "bitcast[u32](-1)"
    if name in ("u64", "usize"):
        return f"sext[{name}](-1)"
    return "-1"


def fas_max(name):
    return {
        "u8": "255",
        "i8": "127",
        "u16": "65535",
        "i16": "32767",
        "u32": "4294967295",
        "i32": "2147483647",
        "u64": "18446744073709551615",
        "i64": "9223372036854775807",
        "usize": "18446744073709551615",
        "isize": "9223372036854775807",
    }[name]


def fas_min(name):
    return {
        "u8": "0",
        "i8": "-128",
        "u16": "0",
        "i16": "-32768",
        "u32": "0",
        "i32": "-2147483648",
        "u64": "0",
        "i64": "-9223372036854775808",
        "usize": "0",
        "isize": "-9223372036854775808",
    }[name]


def fas_cases(name):
    return [fas_min(name), fas_max(name), fas_negative_one(name), "0"]


def c_cases(name):
    entry = next(item for item in types if item[0] == name)
    return [entry[5], entry[3], entry[4], entry[6]]


def generate_fas(include_main=True):
    imports = []
    exports = []
    checks = []
    failure = 1
    for name, fas_ty, _, _, _, _, _ in types:
        imports.append(f"    fn c_echo_{name}(x {fas_ty}) {fas_ty}")
        exports.append(f"    fn fas_echo_{name}(x {fas_ty}) {fas_ty} {{ return x }}")
        for value in fas_cases(name):
            checks.append(
                f"    if c_echo_{name}({value}) != {value} {{ return {failure} }}"
            )
            failure += 1
    imports.extend(
        [
            "    fn c_echo_bool(x bool) bool",
            "    fn c_echo_addr(x addr) addr",
            "    fn c_echo_handle(x handle[Token]) handle[Token]",
            "    fn c_mix8(a u8, b i8, c u16, d i16, e u32, f i32, g u64, h i64) u64",
            "    fn c_mix12(a u8, b i8, c u16, d i16, e u32, f i32, g u64, h i64, i usize, j isize, k bool, l handle[Token]) u64",
            "    fn c_noop() void",
            "    fn c_variadic(marker u64, ...) u64",
        ]
    )
    exports.extend(
        [
            "    fn fas_echo_bool(x bool) bool { return x }",
            "    fn fas_echo_addr(x addr) addr { return x }",
            "    fn fas_echo_handle(x handle[Token]) handle[Token] { return x }",
            "    fn fas_mix8(a u8, b i8, c u16, d i16, e u32, f i32, g u64, h i64) u64 {",
            "        if a != 255 || b != -128 || c != 65535 || d != -32768 || e != 4294967295 || f != -2147483648 || g != 18446744073709551615 || h != -9223372036854775808 { return 0 }",
            "        return 1",
            "    }",
            "    fn fas_mix12(a u8, b i8, c u16, d i16, e u32, f i32, g u64, h i64, i usize, j isize, k bool, l handle[Token]) u64 {",
            "        if a != 255 || b != -128 || c != 65535 || d != -32768 || e != 4294967295 || f != -2147483648 || g != 18446744073709551615 || h != -9223372036854775808 || i != 18446744073709551615 || j != -9223372036854775808 || !k || l == null { return 0 }",
            "        return 1",
            "    }",
            "    fn fas_noop() void { }",
        ]
    )
    checks.extend(
        [
            "    if c_echo_bool(false) || !c_echo_bool(true) { return 31 }",
            "    if c_echo_addr(null) != null { return 32 }",
            "    if c_echo_addr(addr_from_bits(1)) != addr_from_bits(1) { return 33 }",
            "    if c_echo_addr(addr_from_bits(18446744073709551615)) != addr_from_bits(18446744073709551615) { return 34 }",
            "    if c_echo_addr(addr_from_bits(0)) != null { return 35 }",
            "    token handle[Token] = handle_from_addr[Token](addr_from_bits(1))",
            "    if c_echo_handle(null) != null || c_echo_handle(token) != token { return 36 }",
            "    if c_echo_handle(handle_from_addr[Token](addr_from_bits(18446744073709551615))) != handle_from_addr[Token](addr_from_bits(18446744073709551615)) { return 37 }",
            "    if c_mix8(255, -128, 65535, -32768, 4294967295, -2147483648, 18446744073709551615, -9223372036854775808) != 1 { return 38 }",
            "    if c_mix12(255, -128, 65535, -32768, 4294967295, -2147483648, 18446744073709551615, -9223372036854775808, 18446744073709551615, -9223372036854775808, true, token) != 1 { return 39 }",
            "    c_noop()",
            "    flag bool = true",
            "    narrow u8 = 200",
            "    short_value i16 = -1234",
            "    wide u64 = 18446744073709551615",
            "    marker u8 = 19",
            "    if c_variadic(77, flag, narrow, short_value, wide, &marker, token) != 1 { return 40 }",
            "    return 0",
        ]
    )
    source = ["struct Token {}", "extern \"C\" {", *imports, *exports, "}", ""]
    if include_main:
        source.extend(["fn main() i32 {", *checks, "}", ""])
    return "\n".join(source)


def generate_c(include_main=True):
    lines = [
        "#include <stdbool.h>",
        "#include <stddef.h>",
        "#include <stdint.h>",
        "#include <limits.h>",
        "#include <inttypes.h>",
        "typedef struct Token Token;",
    ]
    for name, _, c_ty, *_ in types:
        lines.append(f"{c_ty} c_echo_{name}({c_ty} x) {{ return x; }}")
        lines.append(f"extern {c_ty} fas_echo_{name}({c_ty} x);")
    lines.extend(
        [
            "_Bool c_echo_bool(_Bool x) { return x; }",
            "extern _Bool fas_echo_bool(_Bool x);",
            "void *c_echo_addr(void *x) { return x; }",
            "extern void *fas_echo_addr(void *x);",
            "Token *c_echo_handle(Token *x) { return x; }",
            "extern Token *fas_echo_handle(Token *x);",
            "uint64_t c_mix8(uint8_t a, int8_t b, uint16_t c, int16_t d, uint32_t e, int32_t f, uint64_t g, int64_t h) {",
            "    return a == UINT8_MAX && b == INT8_MIN && c == UINT16_MAX && d == INT16_MIN && e == UINT32_MAX && f == INT32_MIN && g == UINT64_MAX && h == INT64_MIN;",
            "}",
            "extern uint64_t fas_mix8(uint8_t a, int8_t b, uint16_t c, int16_t d, uint32_t e, int32_t f, uint64_t g, int64_t h);",
            "uint64_t c_mix12(uint8_t a, int8_t b, uint16_t c, int16_t d, uint32_t e, int32_t f, uint64_t g, int64_t h, size_t i, ptrdiff_t j, _Bool k, Token *l) {",
            "    return a == UINT8_MAX && b == INT8_MIN && c == UINT16_MAX && d == INT16_MIN && e == UINT32_MAX && f == INT32_MIN && g == UINT64_MAX && h == INT64_MIN && i == SIZE_MAX && j == PTRDIFF_MIN && k && l != NULL;",
            "}",
            "extern uint64_t fas_mix12(uint8_t a, int8_t b, uint16_t c, int16_t d, uint32_t e, int32_t f, uint64_t g, int64_t h, size_t i, ptrdiff_t j, _Bool k, Token *l);",
            "void c_noop(void) {}",
            "extern void fas_noop(void);",
            "uint64_t c_variadic(uint64_t marker, ...) {",
            "    va_list args;",
            "    va_start(args, marker);",
            "    int flag = va_arg(args, int);",
            "    int narrow = va_arg(args, int);",
            "    int short_value = va_arg(args, int);",
            "    uint64_t wide = va_arg(args, uint64_t);",
            "    void *address = va_arg(args, void *);",
            "    Token *handle = va_arg(args, Token *);",
            "    va_end(args);",
            "    return marker == 77 && flag == 1 && narrow == 200 && short_value == -1234 && wide == UINT64_MAX && address != NULL && handle != NULL;",
            "}",
            "extern uint64_t c_variadic(uint64_t marker, ...);",
        ]
    )
    lines.insert(5, "#include <stdarg.h>")
    if include_main:
        lines.extend(
            [
                "#define CHECK(x) do { if (!(x)) return __LINE__ % 251 + 1; } while (0)",
                "int main(void) {",
                "    uint8_t storage = 0;",
            ]
        )
        for name, _, c_ty, c_max, c_neg, c_min, c_zero in types:
            for value in (c_min, c_max, c_neg, c_zero):
                lines.append(f"    CHECK(fas_echo_{name}(({c_ty})({value})) == ({c_ty})({value}));")
        lines.extend(
            [
                "    uint8_t bool_bytes[] = {0, 1};",
                "    CHECK(fas_echo_bool(bool_bytes[0] != 0) == 0);",
                "    CHECK(fas_echo_bool(bool_bytes[1] != 0) == 1);",
                "    void *addresses[] = {NULL, &storage, (void *)(uintptr_t)UINTPTR_MAX, (void *)(uintptr_t)1};",
                "    for (size_t i = 0; i < sizeof(addresses) / sizeof(addresses[0]); ++i) CHECK(fas_echo_addr(addresses[i]) == addresses[i]);",
                "    Token *handles[] = {NULL, (Token *)&storage, (Token *)(uintptr_t)UINTPTR_MAX, (Token *)(uintptr_t)1};",
                "    for (size_t i = 0; i < sizeof(handles) / sizeof(handles[0]); ++i) CHECK(fas_echo_handle(handles[i]) == handles[i]);",
                "    CHECK(fas_mix8(UINT8_MAX, INT8_MIN, UINT16_MAX, INT16_MIN, UINT32_MAX, INT32_MIN, UINT64_MAX, INT64_MIN) == 1);",
                "    CHECK(fas_mix12(UINT8_MAX, INT8_MIN, UINT16_MAX, INT16_MIN, UINT32_MAX, INT32_MIN, UINT64_MAX, INT64_MIN, SIZE_MAX, PTRDIFF_MIN, 1, (Token *)&storage) == 1);",
                "    fas_noop();",
                "    return 0;",
                "}",
                "",
            ]
        )
    return "\n".join(lines)


def function_signature(text, name):
    matches = [
        line
        for line in text.splitlines()
        if re.match(r"^(?:define|declare)\b", line) and re.search(r"@" + re.escape(name) + r"\(", line)
    ]
    if len(matches) != 1:
        raise RuntimeError(f"expected one LLVM signature for {name}, found {len(matches)}")
    line = matches[0]
    start = line.index("@" + name + "(")
    prefix = line[:start]
    result_types = re.findall(r"\b(?:void|ptr|i[0-9]+)\b", prefix)
    result_type = result_types[-1]
    result_attrs = tuple(attr for attr in ("zeroext", "signext", "noundef") if re.search(r"\b" + attr + r"\b", prefix))
    args_text = line[start + len("@" + name + "(") :]
    args_text = args_text[: args_text.index(")")]
    args = []
    variadic = False
    for arg in args_text.split(",") if args_text.strip() else []:
        arg = arg.strip()
        if arg == "...":
            variadic = True
            continue
        arg_types = re.findall(r"\b(?:ptr|i[0-9]+)\b", arg)
        if not arg_types:
            raise RuntimeError(f"cannot parse LLVM parameter {arg!r} in {line}")
        arg_type = arg_types[0]
        attrs = tuple(attr for attr in ("zeroext", "signext", "noundef") if re.search(r"\b" + attr + r"\b", arg))
        args.append((arg_type, attrs))
    return result_type, result_attrs, tuple(args), variadic


def check_parity(fas_text, c_text):
    names = [f"c_echo_{name}" for name, *_ in types]
    names.extend(f"fas_echo_{name}" for name, *_ in types)
    names.extend(
        [
            "c_echo_bool", "fas_echo_bool", "c_echo_addr", "fas_echo_addr",
            "c_echo_handle", "fas_echo_handle", "c_mix8", "fas_mix8",
            "c_mix12", "fas_mix12", "c_noop", "fas_noop", "c_variadic",
        ]
    )
    omitted_noundef = 0
    for name in names:
        fas_sig = function_signature(fas_text, name)
        c_sig = function_signature(c_text, name)
        if fas_sig[0] != c_sig[0] or fas_sig[1] != tuple(attr for attr in c_sig[1] if attr != "noundef") or fas_sig[3] != c_sig[3]:
            raise RuntimeError(f"return or variadic ABI mismatch for {name}: Fas={fas_sig} Clang={c_sig}")
        if len(fas_sig[2]) != len(c_sig[2]):
            raise RuntimeError(f"parameter count mismatch for {name}")
        for index, (fas_arg, c_arg) in enumerate(zip(fas_sig[2], c_sig[2])):
            fas_type, fas_attrs = fas_arg
            c_type, c_attrs = c_arg
            c_ext = tuple(attr for attr in c_attrs if attr != "noundef")
            fas_ext = tuple(attr for attr in fas_attrs if attr != "noundef")
            if fas_type != c_type or fas_ext != c_ext:
                raise RuntimeError(f"parameter ABI mismatch for {name}[{index}]: Fas={fas_arg} Clang={c_arg}")
            if "noundef" in fas_attrs:
                raise RuntimeError(f"Fas unexpectedly asserts noundef for {name}[{index}]")
            if "noundef" in c_attrs:
                omitted_noundef += 1
    print(f"ABI parity: {len(names)} generated signatures match Clang types and extension attributes")
    print(f"ABI parity: Fas omits Clang noundef on {omitted_noundef} parameters; no ABI extension differs")


def main():
    with tempfile.TemporaryDirectory(prefix="fas-abi-matrix-") as temp:
        temp = Path(temp)
        fas_source = temp / "matrix.fas"
        c_source = temp / "matrix.c"
        fas_ir = temp / "fas.ll"
        c_ir = temp / "clang.ll"
        fas_source.write_text(generate_fas())
        c_source.write_text(generate_c())
        run([OCAML_FAS, "--emit-llvm", fas_source, "-o", fas_ir])
        run([LLVM_OPT, "-passes=verify", fas_ir, "-disable-output"])
        optimized = temp / "fas-o2.ll"
        run([LLVM_OPT, "-passes=default<O2>", "-verify-each", fas_ir, "-S", "-o", optimized])
        run([LLVM_OPT, "-passes=verify", optimized, "-disable-output"])
        run([CC, "-std=c17", "-Werror", "-Wno-override-module", "-S", "-emit-llvm", "-O0", c_source, "-o", c_ir])
        check_parity(fas_ir.read_text(), c_ir.read_text())
        for level in (0, 2):
            import_exe = temp / f"imports-{level}"
            export_exe = temp / f"exports-{level}"
            import_c = temp / "helpers.c"
            export_fas = temp / "exports.fas"
            export_ir = temp / "exports.ll"
            import_c.write_text(generate_c(include_main=False))
            export_fas.write_text(generate_fas(include_main=False))
            run([OCAML_FAS, "--emit-llvm", export_fas, "-o", export_ir])
            run([LLVM_OPT, "-passes=verify", export_ir, "-disable-output"])
            run([CC, "-std=c17", "-Werror", "-Wno-override-module", f"-O{level}", fas_ir, import_c, "-o", import_exe])
            run([import_exe])
            run([CC, "-std=c17", "-Werror", "-Wno-override-module", f"-O{level}", export_ir, c_source, "-o", export_exe])
            run([export_exe])
        print("ABI matrix: O0/O2 execution and pre/post-O2 LLVM verification passed")


if __name__ == "__main__":
    main()
