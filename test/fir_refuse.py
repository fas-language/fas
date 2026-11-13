#!/usr/bin/env python3
# Malformed-input fuzzer for fas-fir (roadmap v0.8: "every unsupported path
# gives a message, never a crash"). Mutates valid .fir files and checks that
# each mutant is either compiled (exit 0) or refused with a clean
# "fas-fir: ..." message (exit 1, or 2 for usage). Anything else is a crash:
# an uncaught OCaml exception, a "native backend internal error", a signal,
# or a run longer than the timeout. Crashing inputs are kept in ./crashes.
#
#   fir_refuse.py TOOL COUNT SEED FILE.fir...
#
# Most mutations are "gentle" (whole-word swaps within one family: opcodes,
# types, predicates, and numbers), so that many mutants get past the parser
# and verifier into the backend; the rest edit tokens blindly (delete,
# duplicate, swap, truncate) to exercise the parser.
import collections, os, random, re, shutil, subprocess, sys, tempfile
from concurrent.futures import ThreadPoolExecutor

tool, n, seed = sys.argv[1], int(sys.argv[2]), int(sys.argv[3])
files = sys.argv[4:]
corpus = [open(f, encoding="latin-1").read() for f in files]
TOK = re.compile(r"\s+|[%@$#!][\w.$-]+|-?\d+(?:\.\d+)?|\w+|\"(?:[^\"\\]|\\.)*\"|.", re.S)
TYPES = ["i1", "i8", "i16", "i32", "i64", "i128", "i7", "f32", "f64", "ptr", "void", "<4 x i32>", "[2 x i8]", "{i32, f64}"]
NUMS = ["0", "-1", "1", "65535", "4294967296", "-9223372036854775808", "18446744073709551616",
        "99999999999999999999999", "0.5", "1e999"]
WORDS = ["ret", "br", "call", "load", "store", "alloca", "phi", "add", "sdiv", "fma", "asm", "switch",
         "unreachable", "define", "declare", "global", "loc", "scope", "var", "type", "func", "block"]
FAM = [["add", "sub", "mul", "and", "or", "xor", "shl", "lshr", "ashr", "sdiv", "udiv", "srem", "urem"],
       ["fadd", "fsub", "fmul", "fdiv", "frem"],
       ["sext", "zext", "trunc", "bitcast"],
       ["i8", "i16", "i32", "i64", "i1", "i128", "i24", "i65", "i256"],
       ["f32", "f64"],
       ["eq", "ne", "slt", "sle", "sgt", "sge", "ult", "ule", "ugt", "uge"]]


def gentle(rng, text):
    for _ in range(rng.randint(1, 3)):
        op = rng.randrange(3)
        if op < 2:
            fam = rng.choice(FAM)
            a, b = rng.choice(fam), rng.choice(fam)
            occ = [m.start() for m in re.finditer(r"\b%s\b" % a, text)]
            if not occ:
                continue
            if op == 0:
                text = re.sub(r"\b%s\b" % a, b, text)
            else:
                k = rng.choice(occ)
                text = text[:k] + b + text[k + len(a):]
        else:
            occ = list(re.finditer(r"(?<![\w%@#!$.])-?\d+\b", text))
            if not occ:
                continue
            m = rng.choice(occ)
            text = text[:m.start()] + rng.choice(NUMS + ["2", "3", "7", "8", "16", "64", "128", "4096"]) + text[m.end():]
    return text


def blind(rng, text):
    toks = TOK.findall(text)
    for _ in range(rng.randint(1, 4)):
        if not toks:
            break
        k = rng.randrange(len(toks))
        op = rng.randrange(9)
        if op == 0:
            del toks[k]
        elif op == 1:
            toks.insert(k, toks[rng.randrange(len(toks))])
        elif op == 2:
            j = rng.randrange(len(toks))
            toks[k], toks[j] = toks[j], toks[k]
        elif op == 3:
            toks[k] = rng.choice(TYPES)
        elif op == 4:
            toks[k] = rng.choice(NUMS)
        elif op == 5:
            toks[k] = rng.choice(WORDS)
        elif op == 6:
            s = "".join(toks)
            return s[: rng.randrange(len(s) + 1)]
        elif op == 7 and toks[k][:1] in "%@#!$":
            toks[k] = toks[k][0] + rng.choice(["0", "1", "99", "x", "main", toks[k][1:] + "z"])
        elif op == 8:
            toks[k] = rng.choice(["(", ")", "{", "}", "[", "]", ",", "=", ":", "\"\\zz\"", "\"\\00\"", "\n"])
    return "".join(toks)


def classify(code, err):
    if code == 0:
        return None
    if code in (1, 2) and err.startswith("fas-fir: ") and "internal error" not in err \
            and "Fatal error" not in err and "exception" not in err.lower():
        return None
    first = err.strip().splitlines()[0][:200] if err.strip() else "(no message)"
    return f"exit {code}: {first}"


outcomes = collections.Counter()


def one(i):
    rng = random.Random(seed * 1000003 + i)
    idx = rng.randrange(len(corpus))
    text = corpus[idx]
    src = gentle(rng, text) if rng.random() < 0.7 else blind(rng, text)
    mode = rng.choice([["-c"], ["-c", "-g"], ["--legalize"], ["-g"], ["--check"]])
    d = tempfile.mkdtemp()
    try:
        open(os.path.join(d, "m.fir"), "w", encoding="latin-1").write(src)
        try:
            r = subprocess.run([tool, *mode, "m.fir", "-o", "o"], capture_output=True, timeout=30, cwd=d)
            outcomes[r.returncode] += 1
            bad = classify(r.returncode, r.stderr.decode("latin-1"))
        except subprocess.TimeoutExpired:
            bad = "timeout"
    finally:
        shutil.rmtree(d, ignore_errors=True)
    if bad:
        os.makedirs("crashes", exist_ok=True)
        q = f"crashes/{seed}_{i}.fir"
        open(q, "w", encoding="latin-1").write(src)
        return f"{q} [{' '.join(mode)}] from {os.path.basename(files[idx])}: {bad}"
    return None


with ThreadPoolExecutor(os.cpu_count()) as ex:
    res = [r for r in ex.map(one, range(n)) if r]
seen = {}
for r in res:
    seen.setdefault(re.sub(r"\d+", "N", r.split(": ", 1)[-1]), r)
for r in seen.values():
    print(r)
print(f"inputs {n}: compiled {outcomes[0]}, refused {outcomes[1] + outcomes[2]}, crashes {len(res)} ({len(seen)} distinct)")
sys.exit(1 if res else 0)
