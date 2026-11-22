"""Content-addressed results for deterministic, non-interactive commands."""

from contextlib import contextmanager
from dataclasses import dataclass
import fcntl
from functools import lru_cache
import hashlib
import json
import os
from pathlib import Path
import random
import re
import shutil
import stat
import subprocess
import tempfile
import time

DEFAULT = Path("/home/glonex/fas-experimental-backend/cc-tests/refcache")
ENV_NAMES = frozenset("""
PATH CPATH C_INCLUDE_PATH CPLUS_INCLUDE_PATH OBJC_INCLUDE_PATH LIBRARY_PATH
COMPILER_PATH GCC_EXEC_PREFIX SDKROOT SYSROOT SOURCE_DATE_EPOCH TZ LANG LANGUAGE
LC_ALL GCC_COLORS TERM CLANG_CONFIG_FILE_SYSTEM_DIR CLANG_CONFIG_FILE_USER_DIR
HOME GCC_COMPARE_DEBUG DEPENDENCIES_OUTPUT SUNPRO_DEPENDENCIES
""".split())
ENV_PREFIXES = ("LC_", "LD_", "CCC_", "LLVM_", "FAS_OPT", "FAS_LLC", "FAS_CC")
CRT_FILES = """
crt1.o Scrt1.o rcrt1.o crti.o crtn.o crtbegin.o crtbeginS.o crtbeginT.o
crtend.o crtendS.o libc.so libc.so.6 libc.a libc_nonshared.a libgcc.a libgcc_s.so
libm.so libm.so.6 libm.a libmvec.so.1 libatomic.so libatomic.so.1
ld-linux-x86-64.so.2
""".split()
TIME_MACRO = re.compile(rb"\b__(?:DATE|TIME|TIMESTAMP)__\b")


def digest(data):
    return hashlib.sha256(data).hexdigest()


def encode(value):
    return json.dumps(value, sort_keys=True, separators=(",", ":")).encode()


def atomic_write(path, data, mode=0o644):
    path.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.NamedTemporaryFile(dir=path.parent, prefix=".write-", delete=False) as tmp:
        try:
            tmp.write(data)
            os.fchmod(tmp.fileno(), mode)
            tmp.close()
            os.replace(tmp.name, path)
        finally:
            Path(tmp.name).unlink(missing_ok=True)


def signature(st):
    return [st.st_dev, st.st_ino, st.st_size, st.st_mtime_ns, st.st_ctime_ns]


def fingerprint(path, prior=None, scan=False):
    st = path.stat()
    if not stat.S_ISREG(st.st_mode):
        raise OSError(f"not a regular input: {path}")
    before = signature(st)
    if prior and prior["stat"] == before:
        return prior
    sha, volatile = fingerprint_bytes(path, tuple(before), scan)
    return {"stat": before, "sha": sha, "volatile": volatile}


@lru_cache(maxsize=8192)
def fingerprint_bytes(path, before, scan):
    with path.open("rb") as stream:
        if tuple(signature(os.fstat(stream.fileno()))) != before:
            raise OSError(f"not a regular input: {path}")
        if scan:
            data = stream.read()
            sha, volatile = digest(data), bool(TIME_MACRO.search(data))
        else:
            sha, volatile = hashlib.file_digest(stream, "sha256").hexdigest(), False
    if tuple(signature(path.stat())) != before:
        raise OSError(f"input changed while hashing: {path}")
    return sha, volatile


def relevant_env(env, extra=()):
    return {k: v for k, v in env.items()
            if k in ENV_NAMES or k in extra or k.startswith(ENV_PREFIXES)}


def dependency_words(text):
    # Use an explicit target (-MT refcache), no phony rules, and Make's
    # continuation and escaping conventions for spaces, #, and $.
    prefix = "refcache:"
    if not text.startswith(prefix):
        raise ValueError("unexpected compiler dependency format")
    text = text[len(prefix):].replace("\\\n", "").replace("$$", "$")
    return [re.sub(r"\\(.)", r"\1", word)
            for word in re.findall(r"(?:\\.|[^\s\\])+", text)]


@dataclass
class Invocation:
    ns: str
    argv: tuple
    deps: tuple = ()
    outs: tuple = ()
    cwd: str = "."
    scratch: str | None = None
    cwd_sensitive: bool = True
    c: bool = False
    env_keys: tuple = ()
    version: bool = True

    def canonical(self, value):
        if self.scratch:
            value = value.replace(self.scratch + "/", "$REFCACHE_SCRATCH/")
            return re.sub(re.escape(self.scratch) + r"(?=$|[,=:])", lambda _: "$REFCACHE_SCRATCH", value)
        return value

    def expand(self, value):
        return value.replace("$REFCACHE_SCRATCH", self.scratch) if self.scratch else value

    def path(self, value):
        return Path(os.path.abspath(Path(self.cwd) / value))

    def descriptor(self):
        cwd = self.canonical(self.cwd) if self.cwd_sensitive else None
        return dict(ns=self.ns, argv=list(map(self.canonical, self.argv)),
                    deps=list(map(self.canonical, self.deps)), outs=list(map(self.canonical, self.outs)),
                    cwd=cwd, scratch=bool(self.scratch), c=self.c, env_keys=list(self.env_keys), version=self.version)


class Cache:
    def __init__(self, root=None):
        setting = root if root is not None else os.environ.get("FAS_REFCACHE") or DEFAULT
        self.root = None if setting == "off" else Path(setting).absolute()

    @contextmanager
    def lock(self, exclusive=False):
        self.root.mkdir(parents=True, exist_ok=True)
        with (self.root / "lock").open("ab") as lock:
            fcntl.flock(lock, fcntl.LOCK_EX if exclusive else fcntl.LOCK_SH)
            yield

    def read_json(self, path):
        checksum, data = path.read_bytes().split(b"\n", 1)
        if checksum.decode() != digest(data):
            raise ValueError(f"bad manifest checksum: {path}")
        return json.loads(data)

    def write_json(self, path, value):
        data = encode(value)
        atomic_write(path, digest(data).encode() + b"\n" + data)

    def blob_path(self, sha):
        if not re.fullmatch(r"[0-9a-f]{64}", sha):
            raise ValueError("invalid blob name")
        return self.root / "blobs" / sha[:2] / sha

    def put(self, data):
        sha = digest(data)
        path = self.blob_path(sha)
        if not path.is_file() or path.read_bytes() != data:
            atomic_write(path, data)
        return sha

    def get(self, sha):
        data = self.blob_path(sha).read_bytes()
        if digest(data) != sha:
            raise ValueError(f"corrupt blob: {sha}")
        return data

    def identity(self, inv, env):
        argv0 = inv.argv[0]
        search = os.pathsep.join(str(inv.path(p)) for p in os.get_exec_path(env))
        found = shutil.which(str(inv.path(argv0)) if "/" in argv0 else argv0, path=search)
        if not found:
            raise OSError(f"cannot resolve executable {argv0}")
        binary = Path(found).resolve()
        if not inv.version:
            return [inv.canonical(str(binary)), fingerprint(binary)["sha"]]
        path = self.root / "identity" / digest(encode([str(binary), argv0, inv.c, relevant_env(env, inv.env_keys)]))
        try:
            prior = self.read_json(path)
            files = {name: fingerprint(Path(name), fp) for name, fp in prior["files"].items()}
            if files == prior["files"]:
                return prior["id"]
        except (OSError, ValueError, KeyError, TypeError):
            pass
        # Invoke the same driver spelling (the selected mode can depend on argv[0]).
        version = subprocess.run([argv0, "--version"], cwd=inv.cwd, env=env,
                                 stdin=subprocess.DEVNULL, capture_output=True, timeout=10)
        files = {str(binary): fingerprint(binary)}
        glibc = None
        if inv.c:
            try:
                glibc = os.confstr("CS_GNU_LIBC_VERSION")
            except (AttributeError, ValueError):
                pass
            for directory in (Path("/usr/lib/x86_64-linux-gnu"), Path("/lib64")):
                for name in CRT_FILES:
                    candidate = directory / name
                    if candidate.is_file():
                        files[str(candidate)] = fingerprint(candidate)
            # GCC's actual startup objects and compiler proper are outside the
            # system library directory. Resolve them through the selected driver.
            for flag in ("-print-file-name=crtbegin.o", "-print-file-name=crtbeginS.o",
                         "-print-file-name=crtbeginT.o", "-print-file-name=crtend.o",
                         "-print-file-name=crtendS.o", "-print-file-name=libgcc.a",
                         "-print-file-name=libgcc_s.so", "-print-prog-name=cc1",
                         "-print-prog-name=as", "-print-prog-name=ld"):
                probe = subprocess.run([argv0, flag], cwd=inv.cwd, env=env,
                                       stdin=subprocess.DEVNULL, capture_output=True, timeout=10)
                if not probe.returncode:
                    candidate = inv.path(os.fsdecode(probe.stdout.strip()))
                    if candidate.is_file():
                        files[str(candidate)] = fingerprint(candidate)
        identity = [inv.canonical(str(binary)), version.returncode, digest(version.stdout),
                    digest(version.stderr), glibc, sorted([inv.canonical(p), fp["sha"]] for p, fp in files.items())]
        self.write_json(path, {"id": identity, "files": files})
        return identity

    def run(self, argv, *, ns, deps=(), outs=(), cwd=None, scratch=None, cwd_sensitive=True,
            c=False, env=None, env_keys=(), input=None, timeout=None, check=False, text=False,
            refresh=False, inherit_stdin=False, version=True):
        cwd = os.path.abspath(cwd or os.getcwd())
        inv = Invocation(ns, tuple(map(os.fspath, argv)), tuple(map(os.fspath, deps)),
                         tuple(map(os.fspath, outs)), cwd, os.path.abspath(scratch) if scratch else None,
                         cwd_sensitive, c, tuple(env_keys), version)
        env = dict(os.environ if env is None else env)
        def execute(argv):
            return subprocess.run(argv, cwd=cwd, env=env, input=input, capture_output=True,
                                  stdin=None if input is not None or inherit_stdin else subprocess.DEVNULL,
                                  timeout=timeout)

        result = None
        if self.root is not None and not refresh and not inherit_stdin:
            try:
                with self.lock():
                    result = self.cached(inv, env, input, execute)
            except OSError:
                pass
        if result is None:
            result = execute(inv.argv)
        if text:
            # Match subprocess's locale decoding and universal newline handling.
            import locale
            encoding = locale.getpreferredencoding(False)
            result.stdout = result.stdout.decode(encoding).replace("\r\n", "\n").replace("\r", "\n")
            result.stderr = result.stderr.decode(encoding).replace("\r\n", "\n").replace("\r", "\n")
        if check:
            result.check_returncode()
        return result

    def cached(self, inv, env, input, execute):
        descriptor = inv.descriptor()
        try:
            identity = self.identity(inv, env)
        except subprocess.TimeoutExpired:
            return execute(inv.argv)
        run_env = relevant_env(env, inv.env_keys)
        request = [descriptor, identity, run_env, digest(input) if input is not None else None]
        locator = digest(encode(request))
        index = self.root / "index" / locator[:2] / locator
        try:
            entry = self.read_json(index)
            if entry["request"] != request:
                raise ValueError("wrong request")
            current = self.dependencies(inv, entry["deps"], entry["deps"])
            if self.result_key(request, current) == entry["key"]:
                stdout, stderr, outputs = self.load_result(entry)
                for name, data, mode in outputs:
                    target = inv.path(inv.expand(name))
                    if data is None:
                        target.unlink(missing_ok=True)
                    elif target.parent.is_dir() and not target.is_symlink():
                        atomic_write(target, data, mode)
                    else:
                        raise OSError(f"cannot replay output {target}")
                return subprocess.CompletedProcess(inv.argv, entry["code"], stdout, stderr)
        except (OSError, ValueError, KeyError, TypeError):
            pass

        before = {name: self.output_stat(inv.path(name)) for name in inv.outs}
        try:
            initial = self.dependencies(inv, map(lambda p: inv.canonical(str(inv.path(p))), inv.deps))
        except OSError:
            return execute(inv.argv)
        started = time.time_ns()
        if inv.c:
            if (input and TIME_MACRO.search(input)) or any(
                flag in env for flag in ("DEPENDENCIES_OUTPUT", "SUNPRO_DEPENDENCIES")
            ) or any(flag.startswith(("-M", "@")) for flag in inv.argv[1:]):
                return execute(inv.argv)
            with tempfile.TemporaryDirectory(prefix=".deps-", dir=self.root) as tmp:
                depfile = Path(tmp) / "deps"
                source = input is not None or any(
                    Path(name).suffix in (".c", ".h", ".S", ".cc", ".cpp", ".cxx")
                    for name in inv.deps
                )
                linking = not {"-E", "-S", "-c", "-fsyntax-only"}.intersection(inv.argv)
                mapfile = Path(tmp) / "link.map"
                flags = ["-MD", "-MF", str(depfile), "-MT", "refcache"] if source else []
                if linking:
                    flags.append(f"-Wl,-Map={mapfile}")
                # GCC uses the -o operand as the dependency destination for
                # -E -MD without an explicit -MF.
                if "--" in inv.argv:
                    at = inv.argv.index("--")
                else:
                    at = len(inv.argv)
                try:
                    result = execute((*inv.argv[:at], *flags, *inv.argv[at:]))
                except subprocess.TimeoutExpired as exc:
                    exc.cmd = inv.argv
                    raise
                result.args = inv.argv
                if result.returncode and flags:
                    return execute(inv.argv)
                try:
                    names = dependency_words(os.fsdecode(depfile.read_bytes())) if source else []
                    if linking:
                        loads = [line[5:] for line in os.fsdecode(mapfile.read_bytes()).splitlines()
                                 if line.startswith("LOAD ")]
                        if not loads:
                            # This map format is specific to GNU ld.
                            return result
                        # The driver creates private temporary objects when it
                        # compiles and links in a single invocation.
                        names.extend(name for name in loads
                                     if inv.path(name).is_file() or not name.endswith(".o"))
                except (OSError, ValueError):
                    return result
        else:
            result, names = execute(inv.argv), []
        if result.returncode < 0:
            return result

        try:
            paths = set(initial) | {inv.canonical(str(inv.path(p))) for p in names}
            deps = self.dependencies(inv, paths)
            if any(deps[p] != fp for p, fp in initial.items()) or any(
                fp["volatile"] or fp["stat"][-1] > started for fp in deps.values()
            ):
                return result
            outputs = {}
            payloads = [result.stdout, result.stderr]
            for name in inv.outs:
                target = inv.path(name)
                current = self.output_stat(target)
                previous = before[name]
                if (current is None and previous is None) or (
                    current is not None and previous is not None
                    and signature(current) == signature(previous)
                ):
                    continue
                record = None
                if current is not None:
                    if target.is_symlink() or not stat.S_ISREG(current.st_mode):
                        return result
                    data = target.read_bytes()
                    payloads.append(data)
                    record = [self.put(data), stat.S_IMODE(current.st_mode)]
                outputs[inv.canonical(name)] = record
            if inv.scratch and any(os.fsencode(inv.scratch) in data for data in payloads):
                return result
            snapshots = {}
            for path in deps:
                if path.startswith("$REFCACHE_SCRATCH/"):
                    source = inv.path(inv.expand(path))
                    snapshots[path] = [self.put(source.read_bytes()), stat.S_IMODE(source.stat().st_mode)]
            entry = dict(request=request, key=self.result_key(request, deps), deps=deps,
                         snapshots=snapshots, stdin=None if input is None else self.put(input),
                         code=result.returncode, stdout=self.put(result.stdout), stderr=self.put(result.stderr),
                         outputs=outputs, cwd=inv.canonical(inv.cwd))
            self.write_json(index, entry)
        except (OSError, ValueError):
            pass
        return result

    @staticmethod
    def output_stat(path):
        try:
            return path.stat()
        except FileNotFoundError:
            return None

    def dependencies(self, inv, paths, prior=None):
        return {name: fingerprint(inv.path(inv.expand(name)), (prior or {}).get(name), scan=inv.c)
                for name in sorted(paths)}

    @staticmethod
    def result_key(request, deps):
        return digest(encode([request, {name: fp["sha"] for name, fp in deps.items()}]))

    def load_result(self, entry):
        return self.get(entry["stdout"]), self.get(entry["stderr"]), [
            (name, None, None) if record is None else (name, self.get(record[0]), record[1])
            for name, record in entry["outputs"].items()
        ]

    def indexes(self):
        return (self.root / "index").glob("*/*")

    def entries(self):
        for path in self.indexes():
            if path.name.startswith("."):
                continue
            try:
                yield path, self.read_json(path)
            except (OSError, ValueError):
                continue

    def gc(self, maximum):
        with self.lock(exclusive=True):
            records = sorted(self.entries(), key=lambda record: record[0].stat().st_mtime_ns, reverse=True)
            blobs = {p.name: p.stat().st_size for p in (self.root / "blobs").glob("*/*") if p.is_file()}
            kept = set()
            size = 0
            for path, entry in records:
                reachable = {entry["stdout"], entry["stderr"], *(record[0] for record in entry["snapshots"].values())}
                reachable.update(record[0] for record in entry["outputs"].values() if record is not None)
                if entry["stdin"] is not None:
                    reachable.add(entry["stdin"])
                additional = path.stat().st_size + sum(blobs.get(sha, 0) for sha in reachable - kept)
                if size + additional <= maximum and reachable <= blobs.keys():
                    kept.update(reachable)
                    size += additional
                else:
                    path.unlink()
            for path in (self.root / "blobs").glob("*/*"):
                if path.name not in kept:
                    path.unlink()
            good = {path for path, _ in records}
            for path in self.indexes():
                if path not in good:
                    path.unlink()
            identities = list((self.root / "identity").glob("*"))
            for path in sorted(identities, key=lambda p: p.stat().st_mtime_ns, reverse=True):
                nbytes = path.stat().st_size
                if path.name.startswith(".") or size + nbytes > maximum:
                    path.unlink()
                else:
                    size += nbytes
            return size

    def verify(self, count):
        with self.lock():
            records = list(self.entries())
            errors = []
            checked = 0
            # Shuffle to sample without selecting entries whose inputs have expired.
            random.shuffle(records)
            for path, entry in records:
                if checked >= count:
                    break
                try:
                    ok = self.verify_entry(entry)
                except (OSError, ValueError, KeyError, subprocess.TimeoutExpired) as exc:
                    errors.append(f"{path.name}: {exc}")
                    checked += 1
                    continue
                if ok is not None:
                    checked += 1
                    if not ok:
                        errors.append(f"{path.name}: result changed")
            return checked, errors

    def verify_entry(self, entry):
        descriptor, identity, recorded_env, _ = entry["request"]
        env = dict(os.environ)
        for key in relevant_env(env, descriptor["env_keys"]):
            env.pop(key)
        env.update(recorded_env)
        with tempfile.TemporaryDirectory(prefix=".verify-", dir=self.root) as tmp:
            scratch = tmp if descriptor["scratch"] else None
            inv = Invocation(descriptor["ns"], tuple(descriptor["argv"]), cwd=entry["cwd"],
                             scratch=scratch, c=descriptor["c"], env_keys=tuple(descriptor["env_keys"]),
                             version=descriptor["version"])
            inv.cwd = inv.expand(inv.cwd)
            inv.argv = tuple(map(inv.expand, inv.argv))
            for name, (sha, mode) in entry["snapshots"].items():
                atomic_write(inv.path(inv.expand(name)), self.get(sha), mode)
            try:
                if not Path(inv.cwd).is_dir() or self.identity(inv, env) != identity:
                    return None
                current = self.dependencies(inv, entry["deps"], entry["deps"])
            except OSError:
                return None
            if self.result_key(entry["request"], current) != entry["key"]:
                return None
            # Rewrite only declared outputs, leaving live inputs untouched.
            output_args = {}
            for i, name in enumerate(descriptor["outs"]):
                target = inv.expand(name)
                if scratch and str(inv.path(target)).startswith(scratch + "/"):
                    output_args[target] = target
                elif name in descriptor["argv"]:
                    output_args[target] = str(Path(tmp) / f"verify-output-{i}")
                else:
                    return None
            for target in output_args.values():
                inv.path(target).parent.mkdir(parents=True, exist_ok=True)
            command = tuple(output_args.get(arg, arg) for arg in inv.argv)
            input = None if entry["stdin"] is None else self.get(entry["stdin"])
            result = subprocess.run(command, cwd=inv.cwd, env=env, input=input, capture_output=True,
                                    stdin=subprocess.DEVNULL if input is None else None, timeout=60)
            stdout, stderr, outputs = self.load_result(entry)
            if (result.returncode, result.stdout, result.stderr) != (entry["code"], stdout, stderr):
                return False
            expected = {inv.expand(name): (data, mode) for name, data, mode in outputs}
            for name, path in output_args.items():
                output = Path(path) if Path(path).is_absolute() else inv.path(path)
                data, mode = expected.get(name, (None, None))
                if output.exists():
                    if output.read_bytes() != data or stat.S_IMODE(output.stat().st_mode) != mode:
                        return False
                elif data is not None:
                    return False
            return True


def run(argv, **kwargs):
    return Cache().run(argv, **kwargs)
