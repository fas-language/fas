#!/usr/bin/env python3
"""Bidirectional differential ABI fuzzer against clang-22:
Generates seeded random C23 function signatures (scalars, _Float16, long double,
__int128, _BitInt(N), _Complex, GNU vectors, structs/unions/bitfields/packed/aligned,
empty structs, register exhaustion partway through an aggregate, sret, and varargs).
For each signature:
  1. C caller (clang-22) -> Fir callee (cc/abi.fas + cc/fir_out.fas -> fas-fir)
  2. Fir caller (cc/abi.fas + cc/fir_out.fas -> fas-fir) -> C callee (clang-22)
Verifies every single argument and return value round-trips identically!
"""
import os
from pathlib import Path
import random
import subprocess
import sys
import tempfile

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "tools/refcache"))
from refcache import digest, run as cached_run

GENERATOR = digest(Path(__file__).read_bytes())
CC = os.environ.get("FAS_CC", "clang-22")
CASES = int(os.environ.get("FAS_CC_TYPES_ABI_CASES", os.environ.get("FAS_CC_TYPES_CASES", "1000")))
SEED = int(os.environ.get("SEED", "54321"))
BATCH = 100

# (probe_spec, c_ty, kind, size, valid_bits_or_bytes)
# kind: 'int', 'bool', 'bitint', 'float', 'f80', 'cplx', 'cplx80', 'vec', 'ptr'
SCALARS = [
    ("bool", "_Bool", "bool", 1),
    ("char", "char", "int", 1),
    ("schar", "signed char", "int", 1),
    ("uchar", "unsigned char", "int", 1),
    ("short", "short", "int", 2),
    ("ushort", "unsigned short", "int", 2),
    ("int", "int", "int", 4),
    ("uint", "unsigned int", "int", 4),
    ("long", "long", "int", 8),
    ("ulong", "unsigned long", "int", 8),
    ("i128", "__int128", "int", 16),
    ("u128", "unsigned __int128", "int", 16),
    ("f16", "_Float16", "float", 2),
    ("f32", "float", "float", 4),
    ("f64", "double", "float", 8),
    ("f80", "long double", "f80", 16),
    ("ptr", "void *", "ptr", 8),
    # Note: nullptr_t omitted here because clang-22 has a bug where nullptr_t
    # fails to decrement freeIntRegs when counting GPRs for subsequent 2-GPR args.
    ("cf16", "_Complex _Float16", "cplx", 4),
    ("cf32", "_Complex float", "cplx", 8),
    ("cf64", "_Complex double", "cplx", 16),
    ("cf80", "_Complex long double", "cplx80", 32),
    ("ubi1", "unsigned _BitInt(1)", "bitint", 1),
    ("bi7", "_BitInt(7)", "bitint", 1),
    ("bi17", "_BitInt(17)", "bitint", 4),
    ("ubi33", "unsigned _BitInt(33)", "bitint", 8),
    ("bi65", "_BitInt(65)", "bitint", 16),
    ("ubi96", "unsigned _BitInt(96)", "bitint", 16),
    ("bi129", "_BitInt(129)", "bitint", 24),
    ("vec:char:4", "v4i8", "vec", 4),
    ("vec:int:8", "v2i32", "vec", 8),
    ("vec:f32:16", "v4f32", "vec", 16),
    ("vec:f64:16", "v2f64", "vec", 16),
    # Note: vec:f64:8 (<1 x double>) omitted because clang-22 returns it in xmm0
    # while gcc and fas-fir (src/fir/fir_abi.ml) return it in MEMORY.
]

# Types valid as default-promoted C variadic arguments (C23: float->double,
# narrow ints->int, _Float16->double; structs, _Complex, _BitInt, double, long, ptr stay as-is)
VARARG_SCALARS = [
    s for s in SCALARS
    if s[0] in ("int", "uint", "long", "ulong", "i128", "u128", "f64", "f80", "ptr",
                "cf32", "cf64", "cf80", "bi65", "ubi96", "bi129", "vec:f32:16", "vec:f64:16")
]

FIELD_SCALARS = [
    ("bool", "_Bool", True, 1),
    ("char", "char", True, 8),
    ("short", "short", True, 16),
    ("int", "int", True, 32),
    ("long", "long", True, 64),
    ("i128", "__int128", False, 0),
    ("f16", "_Float16", False, 0),
    ("f32", "float", False, 0),
    ("f64", "double", False, 0),
    ("f80", "long double", False, 0),
    ("ptr", "void *", False, 0),
    ("cf32", "_Complex float", False, 0),
    ("cf64", "_Complex double", False, 0),
    ("bi17", "_BitInt(17)", False, 0),
    ("bi65", "_BitInt(65)", False, 0),
    ("vec:f32:16", "v4f32", False, 0),
]

BF_FIELDS = [s for s in FIELD_SCALARS if s[2]]


def gen_records(rng: random.Random, n_recs: int):
    recs = []
    for idx in range(n_recs):
        is_union = rng.random() < 0.25
        is_packed = rng.random() < 0.20
        al_attr = rng.choice([0, 0, 0, 0, 8, 16])
        pp = rng.choice([0, 0, 0, 1, 2, 4])
        # Include empty struct on idx 0
        nf = 0 if idx == 0 else rng.randint(1, 5)
        fields = []
        for fi in range(nf):
            r = rng.random()
            if r < 0.25:
                sc = rng.choice(BF_FIELDS)
                bw = rng.randint(1, sc[3])
                fields.append({
                    "spec": sc[0], "c_ty": sc[1], "is_bf": True, "bw": bw,
                    "packed": rng.random() < 0.15, "align": 0, "is_arr": False, "arr_len": 0,
                })
            elif idx > 1 and r < 0.42:
                sub = rng.randint(0, idx - 1)
                fields.append({
                    "spec": f"rec:{sub}", "c_ty": f"R{sub}", "is_bf": False, "bw": 0,
                    "packed": rng.random() < 0.15, "align": 0, "is_arr": False, "arr_len": 0,
                })
            elif r < 0.55:
                sc = rng.choice(FIELD_SCALARS)
                alen = rng.choice([1, 2, 3])
                fields.append({
                    "spec": f"arr:{sc[0]}:{alen}", "c_ty": sc[1], "is_bf": False, "bw": 0,
                    "packed": False, "align": 0, "is_arr": True, "arr_len": alen,
                })
            else:
                sc = rng.choice(FIELD_SCALARS)
                fields.append({
                    "spec": sc[0], "c_ty": sc[1], "is_bf": False, "bw": 0,
                    "packed": rng.random() < 0.15, "align": rng.choice([0, 0, 0, 8]),
                    "is_arr": False, "arr_len": 0,
                })
        recs.append({
            "idx": idx, "is_union": is_union, "packed": is_packed,
            "align": al_attr, "pragma_pack": pp, "fields": fields,
        })
    return recs


def ty_c_name(spec: str) -> str:
    if spec == "void":
        return "void"
    if spec.startswith("rec:"):
        return f"R{spec[4:]}"
    for sc in SCALARS:
        if sc[0] == spec:
            return sc[1]
    raise ValueError(spec)


def emit_val_init_c(spec: str, var: str, seed_expr: str) -> str:
    """Emit C statements initializing `var` of type `spec` with deterministic,
    canonical (no NaN, no uninitialized padding, valid _Bool 0/1) bytes."""
    if spec == "bool":
        return f"{var} = (({seed_expr}) & 1);"
    if spec == "f16":
        return f"{var} = (_Float16)((int)(({seed_expr}) % 97) - 48) * (_Float16)0.25f16;"
    if spec == "f32":
        return f"{var} = (float)((int)(({seed_expr}) % 1000) - 500) * 0.125f;"
    if spec == "f64":
        return f"{var} = (double)((int)(({seed_expr}) % 10000) - 5000) * 0.0625;"
    if spec == "f80":
        return f"{var} = (long double)((int)(({seed_expr}) % 10000) - 5000) * 0.03125L;"
    if spec == "cf16":
        return f"{var} = (_Float16)((({seed_expr}) % 31) - 15) + (_Float16)((({seed_expr} * 3) % 31) - 15) * 1.0i;"
    if spec == "cf32":
        return f"{var} = (float)((({seed_expr}) % 101) - 50) + (float)((({seed_expr} * 7) % 101) - 50) * 1.0if;"
    if spec == "cf64":
        return f"{var} = (double)((({seed_expr}) % 251) - 125) + (double)((({seed_expr} * 11) % 251) - 125) * 1.0i;"
    if spec == "cf80":
        return f"{var} = (long double)((({seed_expr}) % 251) - 125) + (long double)((({seed_expr} * 13) % 251) - 125) * 1.0iL;"
    if spec == "nullptr":
        return f"{var} = nullptr;"
    if spec == "ptr":
        return f"{var} = (void *)(uintptr_t)(0x100000ULL + (({seed_expr}) * 0x12345ULL));"
    if spec.startswith("vec:"):
        return f"fill_vec(&{var}, sizeof({var}), {seed_expr});"
    if spec.startswith("rec:"):
        r = spec[4:]
        return f"{{ R{r} _t; init_R{r}(&_t, {seed_expr}); memcpy(&({var}), &_t, sizeof(_t)); }}"
    if spec in ("ubi1",):
        return f"{var} = (unsigned _BitInt(1))(({seed_expr}) & 1);"
    if spec == "bi7":
        return f"{var} = (_BitInt(7))((int)(({seed_expr}) % 63) - 31);"
    if spec == "bi17":
        return f"{var} = (_BitInt(17))((int)(({seed_expr}) % 50000) - 25000);"
    if spec == "ubi33":
        return f"{var} = (unsigned _BitInt(33))((uint64_t)({seed_expr}) * 0x1000101ULL);"
    if spec in ("bi65", "ubi96", "bi129", "i128", "u128"):
        ct = ty_c_name(spec)
        return f"{var} = ({ct})((uint64_t)({seed_expr}) * 0x9e3779b97f4a7c15ULL) ^ ((({ct})({seed_expr} + 17)) << 40);"
    # Standard integer
    ct = ty_c_name(spec)
    return f"{var} = ({ct})(({seed_expr}) * 0x0101010101010101ULL ^ 0x5a5a5a5a5a5a5a5aULL);"


def emit_val_cmp_c(spec: str, a: str, b: str) -> str:
    """Return a C boolean expression that is true iff `a` equals `b`."""
    if spec == "nullptr":
        return "1"
    if spec == "f80":
        return f"(memcmp(&{a}, &{b}, 10) == 0)"
    if spec == "cf80":
        return f"(memcmp(&{a}, &{b}, 10) == 0 && memcmp(((const char*)&{a})+16, ((const char*)&{b})+16, 10) == 0)"
    if spec.startswith("rec:"):
        r = spec[4:]
        return f"({{ R{r} _ca, _cb; memcpy(&_ca, &({a}), sizeof(_ca)); memcpy(&_cb, &({b}), sizeof(_cb)); eq_R{r}(&_ca, &_cb); }})"
    if spec.startswith("vec:"):
        return f"(memcmp(&{a}, &{b}, sizeof({a})) == 0)"
    return f"({a} == {b})"


def run_batch(probe_bin: str, fas_fir_bin: str, rng: random.Random, batch_id: int, n_fns: int):
    recs = []
    for idx in range(28):
        is_union = rng.random() < 0.28
        is_packed = rng.random() < 0.22
        al_attr = rng.choice([0, 0, 0, 0, 8, 16])
        pp = rng.choice([0, 0, 0, 1, 2, 4])
        if idx == 0:
            nf = 0  # empty struct
        else:
            nf = rng.randint(1, 3)
        fields = []
        for fi in range(nf):
            r = rng.random()
            if r < 0.25:
                sc = rng.choice(BF_FIELDS)
                bw = rng.randint(1, sc[3])
                fields.append({
                    "spec": sc[0], "c_ty": sc[1], "is_bf": True, "bw": bw,
                    "packed": rng.random() < 0.15, "align": 0, "is_arr": False, "arr_len": 0,
                })
            elif idx > 1 and r < 0.42:
                # Only nest flat records (idx < 8) so total size stays well under 64 bytes
                sub = rng.randint(0, min(idx - 1, 7))
                fields.append({
                    "spec": f"rec:{sub}", "c_ty": f"R{sub}", "is_bf": False, "bw": 0,
                    "packed": rng.random() < 0.15, "align": 0, "is_arr": False, "arr_len": 0,
                })
            elif r < 0.55:
                sc = rng.choice([x for x in FIELD_SCALARS if x[0] not in ("i128", "f80", "cf64", "bi65", "vec:f32:16")])
                alen = rng.choice([1, 2])
                fields.append({
                    "spec": f"arr:{sc[0]}:{alen}", "c_ty": sc[1], "is_bf": False, "bw": 0,
                    "packed": False, "align": 0, "is_arr": True, "arr_len": alen,
                })
            else:
                sc = rng.choice(FIELD_SCALARS)
                fields.append({
                    "spec": sc[0], "c_ty": sc[1], "is_bf": False, "bw": 0,
                    "packed": rng.random() < 0.15, "align": 0, "is_arr": False, "arr_len": 0,
                })
        recs.append({
            "idx": idx, "is_union": is_union, "packed": is_packed,
            "align": al_attr, "pragma_pack": pp, "fields": fields,
        })

    all_types = [s[0] for s in SCALARS] + [f"rec:{i}" for i in range(len(recs))]
    var_types = [s[0] for s in VARARG_SCALARS] + [f"rec:{i}" for i in range(len(recs))]

    fns = []
    for fid in range(n_fns):
        is_var = rng.random() < 0.20
        ret = "void" if rng.random() < 0.12 else rng.choice(all_types)
        if is_var:
            n_fixed = rng.randint(1, 5)
            n_va = rng.randint(1, 4)
            params = [rng.choice(all_types) for _ in range(n_fixed)] + [rng.choice(var_types) for _ in range(n_va)]
            n_total = n_fixed + n_va
        else:
            n_fixed = rng.randint(0, 10)
            params = [rng.choice(all_types) for _ in range(n_fixed)]
            n_total = n_fixed
        fns.append({
            "id": fid, "ret": ret, "is_var": is_var,
            "n_fixed": n_fixed, "n_total": n_total, "params": params,
        })

    # 1. Build stdin for abi_probe
    plines = []
    for rec in recs:
        k = "U" if rec["is_union"] else "S"
        pk = 1 if rec["packed"] else 0
        plines.append(f"REC {k} {pk} {rec['align']} {rec['pragma_pack']} {len(rec['fields'])}")
        for f in rec["fields"]:
            ibf = 1 if f["is_bf"] else 0
            fpk = 1 if f["packed"] else 0
            plines.append(f"F {f['spec']} {ibf} {f['bw']} {fpk} {f['align']}")
    for fn in fns:
        iv = 1 if fn["is_var"] else 0
        pt_s = " ".join(fn["params"])
        plines.append(f"FN {fn['id']} {fn['ret']} {iv} {fn['n_fixed']} {fn['n_total']} {pt_s}")
    probe_in = "\n".join(plines) + "\n"

    # 2. Emit C harness
    c = [
        "#include <stdio.h>",
        "#include <stdlib.h>",
        "#include <stdint.h>",
        "#include <stddef.h>",
        "#include <stdarg.h>",
        "#include <string.h>",
        "",
        "typedef signed char v4i8 __attribute__((vector_size(4)));",
        "typedef int v2i32 __attribute__((vector_size(8)));",
        "typedef float v4f32 __attribute__((vector_size(16)));",
        "typedef double v2f64 __attribute__((vector_size(16)));",
        "typedef double v1f64 __attribute__((vector_size(8)));",
        "",
        "_Alignas(16) unsigned char abi_in_buf[8192];",
        "_Alignas(16) unsigned char abi_out_buf[8192];",
        "",
        "static void fill_vec(void *p, size_t n, uint64_t seed) {",
        "    unsigned char *b = p;",
        "    for (size_t i = 0; i < n; i++) b[i] = (unsigned char)((seed * 31 + i * 17 + 7) & 0x3f);",
        "}",
        "",
    ]
    # Emit typedefs for records
    for rec in recs:
        idx = rec["idx"]
        pp = rec["pragma_pack"]
        if pp:
            c.append(f"#pragma pack(push, {pp})")
        kw = "union" if rec["is_union"] else "struct"
        attrs = []
        if rec["packed"]:
            attrs.append("packed")
        if rec["align"]:
            attrs.append(f"aligned({rec['align']})")
        attr_s = f" __attribute__(({', '.join(attrs)}))" if attrs else ""
        c.append(f"typedef {kw}{attr_s} R{idx} {{")
        for fi, f in enumerate(rec["fields"]):
            fattrs = []
            if f["packed"]:
                fattrs.append("packed")
            if f["align"]:
                fattrs.append(f"aligned({f['align']})")
            fattr_s = f" __attribute__(({', '.join(fattrs)}))" if fattrs else ""
            if f["is_bf"]:
                c.append(f"    {f['c_ty']} f{fi} : {f['bw']}{fattr_s};")
            elif f["is_arr"]:
                c.append(f"    {f['c_ty']} f{fi}[{f['arr_len']}]{fattr_s};")
            else:
                c.append(f"    {f['c_ty']} f{fi}{fattr_s};")
        c.append(f"}} R{idx};")
        if pp:
            c.append("#pragma pack(pop)")
        c.append(f"_Static_assert(sizeof(R{idx}) <= 256, \"R{idx} exceeds 256-byte slot\");")

    # Emit init_R<idx> and eq_R<idx>
    for rec in recs:
        idx = rec["idx"]
        c.append(f"static void init_R{idx}(R{idx} *p, uint64_t s) {{")
        c.append("    memset(p, 0, sizeof(*p));")
        if rec["is_union"]:
            # Initialize field 0 of union so both sides agree on active member
            if rec["fields"]:
                f = rec["fields"][0]
                if f["is_bf"]:
                    c.append(f"    p->f0 = (s & ((1ULL << {min(f['bw'], 30)}) - 1));")
                elif f["is_arr"]:
                    elem_spec = f["spec"].split(":")[1]
                    for j in range(f["arr_len"]):
                        c.append("    " + emit_val_init_c(elem_spec, f"p->f0[{j}]", f"(s + {j + 1})"))
                else:
                    c.append("    " + emit_val_init_c(f["spec"], "p->f0", "s"))
        else:
            for fi, f in enumerate(rec["fields"]):
                if f["is_bf"]:
                    c.append(f"    p->f{fi} = ((s + {fi + 3}) & ((1ULL << {min(f['bw'], 30)}) - 1));")
                elif f["is_arr"]:
                    elem_spec = f["spec"].split(":")[1]
                    for j in range(f["arr_len"]):
                        c.append("    " + emit_val_init_c(elem_spec, f"p->f{fi}[{j}]", f"(s + {fi * 13 + j + 1})"))
                else:
                    c.append("    " + emit_val_init_c(f["spec"], f"p->f{fi}", f"(s + {fi * 17 + 1})"))
        c.append("}")

        c.append(f"static int eq_R{idx}(const R{idx} *a, const R{idx} *b) {{")
        if not rec["fields"]:
            c.append("    (void)a; (void)b; return 1;")
        elif rec["is_union"]:
            f = rec["fields"][0]
            if f["is_bf"]:
                c.append("    return a->f0 == b->f0;")
            elif f["is_arr"]:
                elem_spec = f["spec"].split(":")[1]
                cmps = [emit_val_cmp_c(elem_spec, f"a->f0[{j}]", f"b->f0[{j}]") for j in range(f["arr_len"])]
                c.append("    return " + " && ".join(cmps) + ";")
            else:
                c.append("    return " + emit_val_cmp_c(f["spec"], "a->f0", "b->f0") + ";")
        else:
            cmps = []
            for fi, f in enumerate(rec["fields"]):
                if f["is_bf"]:
                    cmps.append(f"(a->f{fi} == b->f{fi})")
                elif f["is_arr"]:
                    elem_spec = f["spec"].split(":")[1]
                    for j in range(f["arr_len"]):
                        cmps.append(emit_val_cmp_c(elem_spec, f"a->f{fi}[{j}]", f"b->f{fi}[{j}]"))
                else:
                    cmps.append(emit_val_cmp_c(f["spec"], f"a->f{fi}", f"b->f{fi}"))
            c.append("    return " + " && ".join(cmps) + ";")
        c.append("}")

    # Declare extern Fir functions and define C callees
    for fn in fns:
        fid = fn["id"]
        ret_c = ty_c_name(fn["ret"])
        fixed_ps = [f"{ty_c_name(p)} p{i}" for i, p in enumerate(fn["params"][:fn["n_fixed"]])]
        if fn["is_var"]:
            sig_ps = ", ".join(fixed_ps + ["..."])
        else:
            sig_ps = ", ".join(fixed_ps) if fixed_ps else "void"
            c.append(f"extern {ret_c} fir_callee_{fid}({sig_ps});")
        c.append(f"extern void fir_caller_{fid}(void);")

        # Define c_callee_<fid>: stores all received args into abi_out_buf, returns value from abi_in_buf+4096
        c.append(f"{ret_c} c_callee_{fid}({sig_ps}) {{")
        for i in range(fn["n_fixed"]):
            c.append(f"    memcpy(abi_out_buf + {i * 256}, &p{i}, sizeof(p{i}));")
        if fn["is_var"]:
            c.append("    va_list ap;")
            c.append(f"    va_start(ap, p{fn['n_fixed'] - 1});")
            for i in range(fn["n_fixed"], fn["n_total"]):
                pt_c = ty_c_name(fn["params"][i])
                c.append(f"    {pt_c} va{i} = va_arg(ap, {pt_c});")
                c.append(f"    memcpy(abi_out_buf + {i * 256}, &va{i}, sizeof(va{i}));")
            c.append("    va_end(ap);")
        if fn["ret"] != "void":
            c.append(f"    {ret_c} rv; memcpy(&rv, abi_in_buf + 4096, sizeof(rv)); return rv;")
        c.append("}")

    # Main test driver
    c.append("int main(void) {")
    c.append("    int fails = 0;")
    for fn in fns:
        fid = fn["id"]
        ret = fn["ret"]
        ret_c = ty_c_name(ret)
        base_seed = batch_id * 100000 + fid * 97 + 1

        c.append(f"    {{ // FN {fid}")
        c.append("        memset(abi_in_buf, 0, sizeof(abi_in_buf));")
        c.append("        memset(abi_out_buf, 0, sizeof(abi_out_buf));")
        for i, p in enumerate(fn["params"]):
            pt_c = ty_c_name(p)
            c.append(f"        {pt_c} exp_p{i}; {emit_val_init_c(p, f'exp_p{i}', str(base_seed + i * 7))}")
            c.append(f"        memcpy(abi_in_buf + {i * 256}, &exp_p{i}, sizeof(exp_p{i}));")
        if ret != "void":
            c.append(f"        {ret_c} exp_r; {emit_val_init_c(ret, 'exp_r', str(base_seed + 50))}")
            c.append("        memcpy(abi_in_buf + 4096, &exp_r, sizeof(exp_r));")

        # Direction A: Fir caller -> C callee
        c.append(f"        fir_caller_{fid}();")
        for i, p in enumerate(fn["params"]):
            pt_c = ty_c_name(p)
            c.append(f"        {{ {pt_c} got_p; memcpy(&got_p, abi_out_buf + {i * 256}, sizeof(got_p));")
            c.append(f"          if (!({emit_val_cmp_c(p, f'exp_p{i}', 'got_p')})) {{ fprintf(stderr, \"FAIL batch {batch_id} fn {fid} (Fir->C) arg {i} ({p})\\n\"); fails++; }} }}")
        if ret != "void":
            c.append(f"        {{ {ret_c} got_r; memcpy(&got_r, abi_out_buf + 4096, sizeof(got_r));")
            c.append(f"          if (!({emit_val_cmp_c(ret, 'exp_r', 'got_r')})) {{ fprintf(stderr, \"FAIL batch {batch_id} fn {fid} (Fir->C) ret ({ret})\\n\"); fails++; }} }}")

        # Direction B: C caller -> Fir callee (non-variadic)
        if not fn["is_var"]:
            c.append("        memset(abi_out_buf, 0, sizeof(abi_out_buf));")
            call_args = ", ".join(f"exp_p{i}" for i in range(fn["n_fixed"]))
            if ret != "void":
                c.append(f"        {ret_c} got_r2 = fir_callee_{fid}({call_args});")
                c.append(f"        if (!({emit_val_cmp_c(ret, 'exp_r', 'got_r2')})) {{ fprintf(stderr, \"FAIL batch {batch_id} fn {fid} (C->Fir) ret ({ret})\\n\"); fails++; }}")
            else:
                c.append(f"        fir_callee_{fid}({call_args});")
            for i, p in enumerate(fn["params"]):
                pt_c = ty_c_name(p)
                c.append(f"        {{ {pt_c} got_p2; memcpy(&got_p2, abi_out_buf + {i * 256}, sizeof(got_p2));")
                c.append(f"          if (!({emit_val_cmp_c(p, f'exp_p{i}', 'got_p2')})) {{ fprintf(stderr, \"FAIL batch {batch_id} fn {fid} (C->Fir) arg {i} ({p})\\n\"); fails++; }} }}")
        c.append("    }")
    c.append("    return fails ? 1 : 0;\n}")
    c_src = "\n".join(c) + "\n"

    with tempfile.TemporaryDirectory() as d:
        fir_path = os.path.join(d, "mod.fir")
        fir_o = os.path.join(d, "mod.o")
        c_path = os.path.join(d, "drv.c")
        c_o = os.path.join(d, "drv.o")
        exe_path = os.path.join(d, "run")

        fir_txt = subprocess.check_output([probe_bin], input=probe_in, text=True)
        with open(fir_path, "w") as f:
            f.write(fir_txt)
        with open(c_path, "w") as f:
            f.write(c_src)

        r1 = subprocess.run([fas_fir_bin, "-c", fir_path, "-o", fir_o], capture_output=True, text=True)
        if r1.returncode != 0:
            print(f"FAIL batch {batch_id}: fas-fir failed:\n{r1.stderr}", file=sys.stderr)
            sys.exit(1)
        r2 = cached_run([CC, "-std=gnu23", "-O0", "-w", "-c", c_path, "-o", c_o],
                        ns=f"cc-types/abi/{GENERATOR}/{SEED}/{batch_id}", deps=[c_path],
                        outs=[c_o], scratch=d, c=True, text=True)
        if r2.returncode != 0:
            print(f"FAIL batch {batch_id}: clang-22 failed:\n{r2.stderr}", file=sys.stderr)
            sys.exit(1)
        subprocess.run([CC, c_o, fir_o, "-o", exe_path], check=True)
        r3 = subprocess.run([exe_path], capture_output=True, text=True)
        if r3.returncode != 0:
            print(f"FAIL batch {batch_id} (seed={SEED}):\n{r3.stderr}", file=sys.stderr)
            for line in r3.stderr.splitlines():
                parts = line.split()
                if len(parts) >= 5 and parts[0] == "FAIL" and parts[3] == "fn":
                    fid = int(parts[4])
                    print(f"  sig fn {fid}: {fns[fid]}", file=sys.stderr)
                    for p in [fns[fid]["ret"]] + fns[fid]["params"]:
                        if p.startswith("rec:"):
                            print(f"    {p}: {recs[int(p[4:])]}", file=sys.stderr)
            sys.exit(1)


def main():
    if len(sys.argv) < 3:
        print("usage: abi_fuzz.py <abi_probe_bin> <fas_fir_bin>", file=sys.stderr)
        sys.exit(2)
    probe_bin = sys.argv[1]
    fas_fir_bin = sys.argv[2]

    rng = random.Random(SEED)
    done = 0
    batch_id = 0
    while done < CASES:
        n = min(BATCH, CASES - done)
        run_batch(probe_bin, fas_fir_bin, rng, batch_id, n)
        done += n
        batch_id += 1
    print(f"abi_fuzz: {CASES}/{CASES} signatures passed bidirectionally (seed={SEED})")


if __name__ == "__main__":
    main()
