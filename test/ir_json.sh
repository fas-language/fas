#!/bin/sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
IR_JSON_TMP=$(mktemp -d)
trap 'rm -rf "$IR_JSON_TMP"' EXIT HUP INT TERM
mkdir "$IR_JSON_TMP/pairs"

emit_one() {
  mode=$1
  source=$2
  output=$3
  error=$4
  case "$source" in
    "$ROOT/test/assembly_linkage.fas")
      "$OCAML_FAS" -DASSEMBLY_TABLE_VALUE=42 "$mode" "$source" >"$output" 2>"$error"
      ;;
    "$ROOT/test/export_nested_headers.fas")
      "$OCAML_FAS" -I "$ROOT/test" "$mode" "$source" >"$output" 2>"$error"
      ;;
    *) "$OCAML_FAS" "$mode" "$source" >"$output" 2>"$error" ;;
  esac
}

emit_one --emit-ir-json "$ROOT/test/ir_json.fas" "$IR_JSON_TMP/out.json" \
  "$IR_JSON_TMP/stderr"
[ ! -s "$IR_JSON_TMP/stderr" ] || { cat "$IR_JSON_TMP/stderr" >&2; exit 1; }
emit_one --emit-ir-json "$ROOT/test/ir_json.fas" "$IR_JSON_TMP/repeat.json" \
  "$IR_JSON_TMP/stderr"
cmp "$IR_JSON_TMP/out.json" "$IR_JSON_TMP/repeat.json"
emit_one --emit-ir "$ROOT/test/ir_json.fas" "$IR_JSON_TMP/out.ir" "$IR_JSON_TMP/stderr"
"$OCAML_FAS" --emit-ir-json "$ROOT/test/ir_json.fas" -o "$IR_JSON_TMP/file.json"
cmp "$IR_JSON_TMP/out.json" "$IR_JSON_TMP/file.json"
"$OCAML_FAS" -g --no-inline pick --emit-ir-json "$ROOT/test/ir_json.fas" \
  >"$IR_JSON_TMP/noinline.json"

for source_name in c_interop sdl_headers ir_json_static_address; do
  emit_one --emit-llvm "$ROOT/test/$source_name.fas" "$IR_JSON_TMP/$source_name.ll" \
    "$IR_JSON_TMP/stderr"
done

mkdir "$IR_JSON_TMP/collisions"
cat >"$IR_JSON_TMP/collisions/duplicate_adapters.fas" <<'EOF'
use "C" <<A
static inline int oracle(int x) { return x + 1; }
A
use "C" <<B
static inline int oracle(int x) { return x + 2; }
B
fn main() i32 { return oracle(1) }
EOF
cat >"$IR_JSON_TMP/collisions/function_name.fas" <<'EOF'
use "C" <<C
static inline int oracle(int x) { return x + 1; }
C
fn oracle(x i32) i32 { return x }
fn main() i32 { return oracle(1) }
EOF
cat >"$IR_JSON_TMP/collisions/global_name.fas" <<'EOF'
use "C" <<C
static inline int oracle(int x) { return x + 1; }
C
var oracle i32 = 3
fn main() i32 { return oracle }
EOF
cat >"$IR_JSON_TMP/collisions/extern_name.fas" <<'EOF'
use "C" <<C
static inline int oracle(int x) { return x + 1; }
C
extern "C" { fn oracle(x i32) i32 { return x } }
fn main() i32 { return oracle(1) }
EOF
mkdir "$IR_JSON_TMP/collisions/dependencies"
cat >"$IR_JSON_TMP/collisions/dependencies/a.fas" <<'EOF'
use "C" <<C
static inline int oracle(int x) { return x + 1; }
C
EOF
cat >"$IR_JSON_TMP/collisions/dependencies/b.fas" <<'EOF'
use "C" <<C
static inline int oracle(int x) { return x + 2; }
C
EOF
cat >"$IR_JSON_TMP/collisions/dependencies/root.fas" <<'EOF'
use "a.fas"
use "b.fas"
fn main() i32 { return oracle(1) }
EOF

expect_rejection() {
  source=$1
  expected=$2
  if "$OCAML_FAS" --emit-ir-json "$source" >"$IR_JSON_TMP/rejected.json" \
    2>"$IR_JSON_TMP/stderr"; then
    echo "IR JSON: collision unexpectedly compiled: $source" >&2
    exit 1
  fi
  grep -F "$expected" "$IR_JSON_TMP/stderr" >/dev/null || {
    cat "$IR_JSON_TMP/stderr" >&2
    echo "IR JSON: collision diagnostic did not contain: $expected" >&2
    exit 1
  }
}

expect_rejection "$IR_JSON_TMP/collisions/duplicate_adapters.fas" \
  "C compilation failed: redefinition of 'oracle'"
expect_rejection "$IR_JSON_TMP/collisions/function_name.fas" \
  'duplicate declaration `oracle`'
expect_rejection "$IR_JSON_TMP/collisions/global_name.fas" \
  'duplicate declaration `oracle`'
expect_rejection "$IR_JSON_TMP/collisions/extern_name.fas" \
  "C compilation failed: static declaration of 'oracle' follows non-static declaration"
expect_rejection "$IR_JSON_TMP/collisions/dependencies/root.fas" \
  'C declaration `oracle` is not supported: conflicting C declarations'

index=0
skipped=0
for source in "$ROOT"/test/*.fas; do
  dump="$IR_JSON_TMP/current.ir"
  json="$IR_JSON_TMP/current.json"
  if emit_one --emit-ir "$source" "$dump" "$IR_JSON_TMP/stderr"; then
    if ! emit_one --emit-ir-json "$source" "$json" "$IR_JSON_TMP/stderr"; then
      cat "$IR_JSON_TMP/stderr" >&2
      echo "IR JSON: rejected source accepted by --emit-ir: $source" >&2
      exit 1
    fi
    cp "$dump" "$IR_JSON_TMP/pairs/$index.ir"
    cp "$json" "$IR_JSON_TMP/pairs/$index.json"
    basename "$source" .fas >"$IR_JSON_TMP/pairs/$index.name"
    index=$((index + 1))
  else
    skipped=$((skipped + 1))
  fi
done

python3 - "$IR_JSON_TMP" "$index" "$skipped" "$ROOT/test/ir_json_expected" <<'PY'
import gzip
import json
import os
import re
import sys

root, count, skipped, expected_root = sys.argv[1], int(sys.argv[2]), int(sys.argv[3]), sys.argv[4]

def read_json(path):
    opener = gzip.open if path.endswith(".gz") else open
    with opener(path, "rt", encoding="utf-8") as stream:
        return json.load(stream)

source_indexes = {}
for index in range(count):
    with open(os.path.join(root, "pairs", str(index) + ".name"), encoding="utf-8") as stream:
        source_indexes[stream.read().strip()] = index

def names_in_dump(text):
    result = []
    for line in text.splitlines():
        match = re.search(r'Function \{ name = "((?:\\.|[^"\\])*)";', line)
        if match:
            result.append(bytes(match.group(1), "utf-8").decode("unicode_escape"))
    return result

def ir_tag(line):
    line = line.strip()
    if line == "call void @llvm.trap()":
        return "trap"
    rhs = line.split(" = ", 1)[1] if " = " in line else line
    if rhs.startswith("getelementptr "):
        if "@.str." in rhs:
            return "string_ptr"
        if re.search(r", ptr @[^, ]+", rhs):
            return "global_ptr"
        return "gep"
    if rhs.startswith("load volatile "):
        return "load_volatile"
    if rhs.startswith("load "):
        return "load"
    if rhs.startswith("store volatile "):
        return "store_volatile"
    if rhs.startswith("store "):
        return "store"
    if rhs.startswith("icmp "):
        return "cmp"
    if rhs.startswith("fcmp "):
        return "cmp"
    if rhs.startswith("fneg "):
        return "fneg"
    if rhs.startswith("alloca "):
        return "alloca"
    if rhs.startswith('call i64 asm sideeffect "syscall"'):
        return "syscall"
    if rhs.startswith("call "):
        return "call_indirect" if re.search(r"\s%[A-Za-z0-9_.]+\(", rhs) else "call"
    if rhs.startswith("phi "):
        return "phi"
    if rhs.startswith("select "):
        return "select"
    if rhs.startswith("extractelement "):
        return "extract"
    if rhs.startswith("insertelement "):
        return "insert"
    if rhs.startswith("shufflevector "):
        return "shuffle_zero" if rhs.endswith("zeroinitializer") else "shufflevector"
    if rhs.startswith(("zext ", "sext ", "trunc ", "bitcast ", "ptrtoint ",
                       "inttoptr ", "sitofp ", "uitofp ", "fptosi ", "fptoui ",
                       "fpext ", "fptrunc ")):
        return "cast"
    if rhs.startswith(("add ", "sub ", "mul ", "sdiv ", "srem ", "udiv ",
                       "urem ", "and ", "or ", "xor ", "shl ", "lshr ", "ashr ",
                       "fadd ", "fsub ", "fmul ", "fdiv ")):
        return "bin"
    raise AssertionError("unrecognized --emit-ir instruction: " + line)

def term_tag(line):
    value = line.split("terminator = ", 1)[1].strip()
    if value.startswith("ret ") or value == "ret void":
        return "ret"
    if value.startswith("br i1 "):
        return "condbr"
    if value.startswith("br label "):
        return "br"
    if value.startswith("switch "):
        return "switch"
    if value == "unreachable":
        return "unreachable"
    raise AssertionError("unrecognized --emit-ir terminator: " + value)

def parse_dump(text):
    functions = []
    current_function = None
    current_block = None
    in_instrs = False
    for line in text.splitlines():
        match = re.search(r'Function \{ name = "((?:\\.|[^"\\])*)";', line)
        if match:
            current_function = {
                "name": bytes(match.group(1), "utf-8").decode("unicode_escape"),
                "blocks": [],
            }
            functions.append(current_function)
            current_block = None
            continue
        match = re.search(r"Block \{ id = ([0-9]+);", line)
        if match:
            current_block = {"id": int(match.group(1)), "instrs": [], "term": None}
            current_function["blocks"].append(current_block)
            in_instrs = "instrs = [" in line
            continue
        if current_block is None:
            continue
        if "instrs = [" in line:
            in_instrs = True
            continue
        if in_instrs:
            if line.strip() == "];":
                in_instrs = False
            else:
                current_block["instrs"].append(ir_tag(line))
            continue
        if "terminator = " in line:
            current_block["term"] = term_tag(line)
    return functions

def target_names(module):
    functions = {function["name"] for function in module["funcs"]}
    globals_ = {global_["name"] for global_ in module["globals"]}
    for function in module["funcs"]:
        for block in function["blocks"]:
            for instruction in block["instrs"]:
                if instruction[0] == "call":
                    assert instruction[4] in functions or instruction[4] in globals_, (
                        function["name"], instruction[4]
                    )
    def function_values(value, path=()):
        if isinstance(value, dict):
            if set(value) == {"func"}:
                assert value["func"] in functions, (path, value["func"])
            else:
                for key, item in value.items():
                    function_values(item, path + (key,))
        elif isinstance(value, list):
            for index, item in enumerate(value):
                function_values(item, path + (index,))

    function_values(module)

def validate_adapter_metadata(module, source_name):
    adapters = module["c_adapters"]
    assert all(list(adapter) == ["name", "symbol"] for adapter in adapters), source_name
    names = [adapter["name"] for adapter in adapters]
    symbols = [adapter["symbol"] for adapter in adapters]
    assert len(names) == len(set(names)), (source_name, names)
    assert len(symbols) == len(set(symbols)), (source_name, symbols)
    assert all(symbol.startswith("__fas_c_adapter_") for symbol in symbols), (source_name, symbols)
    functions = {function["name"]: function for function in module["funcs"]}
    for adapter in adapters:
        function = functions[adapter["name"]]
        assert function["linkage"] == "external", (source_name, adapter)
        assert function["blocks"] == [], (source_name, adapter)
    assert {function["name"] for function in module["funcs"]
            if function["name"] in set(names)} == set(names), (source_name, names)

    def check_symbols(value, path=()):
        if isinstance(value, dict):
            for key, item in value.items():
                check_symbols(item, path + (key,))
        elif isinstance(value, list):
            for index, item in enumerate(value):
                check_symbols(item, path + (index,))
        elif isinstance(value, str) and value.startswith("__fas_c_adapter_"):
            assert len(path) == 3 and path[0] == "c_adapters" and path[2] == "symbol", (
                source_name, path, value
            )

    check_symbols(module)

def assert_legacy_values(expected, actual, function_names, path=()):
    if (isinstance(expected, list) and len(expected) == 3 and expected[0] == "global"
            and expected[1] == "ptr" and expected[2] in function_names
            and actual == {"func": expected[2]}):
        return
    if (isinstance(expected, list) and len(expected) == 3 and len(actual) == 3
            and len(path) >= 2 and path[-2] == "pointers"
            and expected[1] in function_names and actual[1] == {"func": expected[1]}):
        assert expected[0] == actual[0] and expected[2] == actual[2], (path, expected, actual)
        return
    if isinstance(expected, dict):
        assert isinstance(actual, dict), path
        added = set()
        if len(path) == 1:
            added = {"format", "version", "target_triple", "data_layout", "no_inline", "no_inline_functions", "c_adapters"}
            assert actual.get("format") == "fas-ir-json" and actual.get("version") == 2, path
        elif len(path) == 3 and path[1] == "funcs" and isinstance(path[2], int):
            added = {"ret_extension"}
        elif (len(path) == 5 and path[1] == "funcs" and isinstance(path[2], int)
              and path[3] == "params" and isinstance(path[4], int)):
            added = {"extension"}
        elif (len(path) == 3 and path[1] == "globals" and isinstance(path[2], int)
              and expected.get("kind") == "storage"):
            added = {"linkage"}
        assert set(actual) == set(expected) | added, (path, set(expected), set(actual))
        for key, value in expected.items():
            assert_legacy_values(value, actual[key], function_names, path + (key,))
    elif isinstance(expected, list) and len(path) == 2 and path[-1] in {
            "funcs", "globals", "structs"}:
        actual_index = 0
        for expected_item in expected:
            if (actual_index < len(actual)
                    and expected_item.get("name") == actual[actual_index].get("name")):
                assert_legacy_values(
                    expected_item, actual[actual_index], function_names,
                    path + (actual_index,)
                )
                actual_index += 1
                continue
            name = expected_item.get("name")
            assert name, (path, expected_item)
            if path[-1] == "funcs":
                assert expected_item.get("blocks") == [], (path, expected_item)
            elif path[-1] == "globals":
                assert expected_item.get("kind") == "storage", (path, expected_item)
            else:
                assert "fields" in expected_item, (path, expected_item)
            pattern = re.compile(
                r"(?<![A-Za-z0-9_$])" + re.escape(name) + r"(?![A-Za-z0-9_$])"
            )
            opcodes = {
                "alloca", "bin", "br", "call", "call_indirect", "cast", "cmp",
                "condbr", "extract", "gep", "insert", "load", "phi", "ret",
                "select", "shuffle_zero", "shufflevector", "store", "switch",
            }

            def referenced(value, key=None):
                if isinstance(value, dict):
                    adapters = value.get("c_adapters") if key is None else None
                    if isinstance(adapters, list) and any(
                            isinstance(adapter, dict)
                            and name in (adapter.get("name"), adapter.get("symbol"))
                            for adapter in adapters):
                        return True
                    return any(
                        (field in {"func", "global", "target", "symbol"}
                         and item == name)
                        or (field != "name" and referenced(item, field))
                        for field, item in value.items()
                    )
                if isinstance(value, list):
                    if len(value) > 4 and value[0] == "call" and value[4] == name:
                        return True
                    if len(value) > 1 and value[0] == "global" and value[1] == name:
                        return True
                    return any(referenced(item) for item in value)
                return (
                    isinstance(value, str) and value not in opcodes
                    and pattern.search(value) is not None
                )

            assert not referenced(actual), (path, name)
        assert actual_index == len(actual), (path, actual[actual_index:])
    elif isinstance(expected, list):
        assert isinstance(actual, list) and len(expected) == len(actual), path
        for index, (expected_item, actual_item) in enumerate(zip(expected, actual)):
            assert_legacy_values(expected_item, actual_item, function_names, path + (index,))
    else:
        assert expected == actual, (path, expected, actual)

def llvm_adapter_symbols(text):
    result = []
    for line in text.splitlines():
        if line.startswith(("define ", "declare ")):
            match = re.search(r'@(__fas_c_adapter_[^\s(]+)\(', line)
            if match:
                result.append(match.group(1))
    return result

expected = {
    "value": {"const", "vconst", "null", "undef", "zero", "local", "param", "func"},
    "instr": {"bin", "cmp", "fneg", "alloca", "load", "load_volatile", "store",
              "store_volatile", "gep", "cast", "call", "phi", "select", "extract",
              "insert", "shuffle_zero", "shufflevector", "string_ptr", "global_ptr", "call_indirect", "trap", "syscall"},
    "terminator": {"ret", "br", "condbr", "switch", "unreachable"},
    "global": {"string", "array", "storage"},
    "ty": {"i1", "i8", "i16", "i32", "i64", "i128", "float", "double",
           "ptr", "vector", "struct", "array", "void"},
}
observed = {key: set() for key in expected}
value_tags, instruction_tags, terminator_tags = expected["value"], expected["instr"], expected["terminator"]

def visit(value):
    if isinstance(value, list):
        if value and isinstance(value[0], str):
            tag = value[0]
            if tag in value_tags:
                observed["value"].add(tag)
            if tag in instruction_tags:
                observed["instr"].add(tag)
            if tag in terminator_tags:
                observed["terminator"].add(tag)
        for item in value:
            visit(item)
    elif isinstance(value, dict):
        if set(value) == {"func"}:
            observed["value"].add("func")
        if value.get("kind") in expected["global"]:
            observed["global"].add(value["kind"])
        for item in value.values():
            visit(item)
    elif isinstance(value, str):
        if value in {"i1", "i8", "i16", "i32", "i64", "i128", "float", "double",
                     "ptr", "void"}:
            observed["ty"].add(value)
        elif value.startswith("<") and " x " in value:
            observed["ty"].add("vector")
        elif value.startswith("[") and " x " in value:
            observed["ty"].add("array")
        elif value.startswith("%"):
            observed["ty"].add("struct")

for index in range(count):
    pair = os.path.join(root, "pairs", str(index))
    source_name = open(pair + ".name", encoding="utf-8").read().strip()
    module = read_json(pair + ".json")
    dump = open(pair + ".ir", encoding="utf-8").read()
    parsed = parse_dump(dump)
    dump_names = names_in_dump(dump)
    assert len(module["funcs"]) == len(dump_names), source_name
    assert len(module["funcs"]) == len(parsed), source_name
    for json_function, ir_function in zip(module["funcs"], parsed):
        assert json_function["name"] == ir_function["name"], (
            source_name, json_function["name"], ir_function["name"]
        )
        assert [block["id"] for block in json_function["blocks"]] == [
            block["id"] for block in ir_function["blocks"]
        ], json_function["name"]
        assert len(json_function["blocks"]) == len(ir_function["blocks"]), json_function["name"]
        for json_block, ir_block in zip(json_function["blocks"], ir_function["blocks"]):
            assert len(json_block["instrs"]) == len(ir_block["instrs"]), (
                source_name, json_function["name"], json_block["id"],
                len(json_block["instrs"]), len(ir_block["instrs"])
            )
            for instruction, ir_instruction in zip(json_block["instrs"], ir_block["instrs"]):
                assert instruction[0] == ir_instruction or (
                    instruction[0] == "call_indirect" and ir_instruction == "call"
                ), (source_name, json_function["name"], json_block["id"], instruction[0], ir_instruction)
            assert json_block["term"][0] == ir_block["term"], (
                source_name, json_function["name"], json_block["id"],
                json_block["term"][0], ir_block["term"]
            )
    target_names(module)
    validate_adapter_metadata(module, source_name)
    visit(module)

for key, tags in expected.items():
    missing = tags - observed[key]
    assert not missing, f"missing {key} forms from IR constructors in src/backend/llvm/ir.ml: {sorted(missing)}"

fixture = read_json(os.path.join(root, "out.json"))
assert list(fixture) == ["structs", "globals", "funcs", "format", "version",
                         "target_triple", "data_layout", "no_inline", "no_inline_functions",
                         "c_adapters"]
assert fixture["c_adapters"] == []
assert fixture["format"] == "fas-ir-json" and fixture["version"] == 2
assert fixture["target_triple"] == "x86_64-unknown-linux-gnu"
assert fixture["data_layout"] == "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
assert fixture["no_inline"] is None and fixture["no_inline_functions"] == []
assert [list(item) for item in fixture["structs"]] == [["name", "fields", "tail_padding"]]
assert [item["name"] for item in fixture["structs"]] == ["Pair"]
assert fixture["structs"][0]["fields"] == ["i32", "i32"]
global_kinds = {item["kind"] for item in fixture["globals"]}
assert len(fixture["globals"]) == 2
assert global_kinds <= {"array", "storage", "string"}, global_kinds
assert any(
    item["kind"] == "array" and item["elems"] == [7, -2, 9] and item["elem"] == "i32"
    for item in fixture["globals"]
) or any(item["kind"] == "storage" and item["size"] == 12 for item in fixture["globals"])
for array in (item for item in fixture["globals"] if item["kind"] == "array"):
    assert list(array) == ["kind", "name", "elem", "elems", "align"]
for storage in (item for item in fixture["globals"] if item["kind"] == "storage"):
    assert list(storage) == [
        "kind", "name", "ty", "size", "bytes", "pointers", "readonly", "align", "linkage"
    ]
names = [function["name"] for function in fixture["funcs"]]
assert names == names_in_dump(open(os.path.join(root, "out.ir"), encoding="utf-8").read())
for function in fixture["funcs"]:
    assert list(function) == ["name", "params", "ret", "linkage", "variadic", "blocks", "ret_extension"]
    for parameter in function["params"]:
        assert list(parameter) == ["name", "ty", "extension"]
pick = next(function for function in fixture["funcs"] if function["name"] == "pick")
assert pick["ret_extension"] == "none"
assert all(parameter["extension"] == "none" for parameter in pick["params"])
ops = [instruction[0] for block in pick["blocks"] for instruction in block["instrs"]]
kinds = [block["term"][0] for block in pick["blocks"]]
assert "switch" in kinds and "condbr" in kinds and "ret" in kinds, kinds
switch = next(block["term"] for block in pick["blocks"] if block["term"][0] == "switch")
assert switch[1] == "i32" and [case[0] for case in switch[3]] == [1], switch
bins = [instruction[2] for block in pick["blocks"] for instruction in block["instrs"] if instruction[0] == "bin"]
assert "srem" in bins and "add" in bins, bins
cmps = [instruction[2] for block in pick["blocks"] for instruction in block["instrs"] if instruction[0] == "cmp"]
assert "slt" in cmps and "ne" in cmps, cmps
assert "alloca" in ops and "load" in ops and "store" in ops, ops
main = next(function for function in fixture["funcs"] if function["name"] == "main")
calls = [instruction for block in main["blocks"] for instruction in block["instrs"] if instruction[0] == "call"]
assert calls and calls[0][4] == "pick" and [arg[2] for arg in calls[0][5]] == [
    ["const", "i32", 3], ["const", "i32", 5]
], calls

noinline = read_json(os.path.join(root, "noinline.json"))
assert noinline["no_inline"] == "pick" and noinline["no_inline_functions"] == ["pick"]
abi = read_json(os.path.join(root, "pairs", str(source_indexes["abi_exports"]) + ".json"))
abi_functions = {function["name"]: function for function in abi["funcs"]}
for name, extension in (("fas_bool", "zext"), ("fas_i8", "sext"), ("fas_u8", "zext")):
    function = abi_functions[name]
    assert function["params"][0]["extension"] == extension, function
    assert function["ret_extension"] == extension, function

globals_module = read_json(os.path.join(
    root, "pairs", str(source_indexes["globals"]) + ".json"
))
globals_by_name = {global_["name"]: global_ for global_ in globals_module["globals"]}
assert globals_by_name["ZeroScalar"]["linkage"] == "internal"
assert globals_by_name["Exported"]["linkage"] == "export"
assert globals_by_name["Imported"]["linkage"] == "import"

adapter_module = read_json(os.path.join(
    root, "pairs", str(source_indexes["c_interop"]) + ".json"
))
adapter_names = [adapter["name"] for adapter in adapter_module["c_adapters"]]
adapter_symbols = [adapter["symbol"] for adapter in adapter_module["c_adapters"]]
adapter_functions = {function["name"]: function for function in adapter_module["funcs"]
                     if function["name"] in set(adapter_names)}
adapter_calls = {
    instruction[4]
    for function in adapter_module["funcs"]
    for block in function["blocks"]
    for instruction in block["instrs"]
    if instruction[0] == "call" and instruction[4] in set(adapter_names)
}
assert adapter_calls, "C adapter call was not serialized by its C name"
assert adapter_calls <= set(adapter_functions), adapter_calls - set(adapter_functions)
assert all(name in adapter_names for name in adapter_calls), adapter_calls

static_address = read_json(os.path.join(
    root, "pairs", str(source_indexes["ir_json_static_address"]) + ".json"
))
static_pointers = [
    pointer
    for global_ in static_address["globals"] if global_["kind"] == "storage"
    for pointer in global_["pointers"]
]
assert [0, {"func": "fas_json_address_target"}, 0] in static_pointers, static_pointers

indirect_module = read_json(os.path.join(
    root, "pairs", str(source_indexes["ir_json_function_address"]) + ".json"
))
indirect_calls = [
    instruction
    for function in indirect_module["funcs"]
    for block in function["blocks"]
    for instruction in block["instrs"]
    if instruction[0] == "call_indirect"
]
assert len(indirect_calls) == 2, indirect_calls
assert sorted(instruction[4]["func"] for instruction in indirect_calls) == [
    "fas_json_indirect_adapter", "step"
], indirect_calls

for source_name in ("c_interop", "sdl_headers", "ir_json_static_address"):
    module = read_json(os.path.join(root, "pairs", str(source_indexes[source_name]) + ".json"))
    symbols = [adapter["symbol"] for adapter in module["c_adapters"]]
    llvm = open(os.path.join(root, source_name + ".ll"), encoding="utf-8").read()
    assert llvm_adapter_symbols(llvm) == symbols, (source_name, symbols, llvm_adapter_symbols(llvm))

for source_name in ("c_interop", "sdl_headers", "ir_json_static_address"):
    expected = read_json(os.path.join(expected_root, source_name + ".json.gz"))
    actual = read_json(os.path.join(root, "pairs", str(source_indexes[source_name]) + ".json"))
    # Lowering now places local stack slots at the top of the entry block,
    # which the snapshot predates; compare entry blocks with allocas first.
    for module in (expected, actual):
        for function in module["funcs"]:
            if function.get("blocks"):
                entry = function["blocks"][0]["instrs"]
                function["blocks"][0]["instrs"] = (
                    sorted((i for i in entry if i[0] == "alloca"), key=lambda i: i[1])
                    + [i for i in entry if i[0] != "alloca"]
                )
    functions = {function["name"] for function in actual["funcs"]}
    assert_legacy_values(expected, actual, functions, (source_name,))

print(f"IR JSON: {count} compilable source files matched --emit-ir; {skipped} were rejected")
print("IR JSON: dda55cb compatibility, adapter metadata and collision rejection passed")
PY

echo "IR JSON: format, constructors, determinism and symbol closure passed"
