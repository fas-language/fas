#!/bin/sh
# Fas errors: `error`, `fails`, `fail`, `try`, `catch`, `errdefer`,
# errno_err/err_name, fails main, and the C ABI of fails results.
# Every program runs on the native backend and on the hidden LLVM test
# oracle (removal A3, Cli.oracle); both must print the same thing.
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=$(printenv CC || printf 'clang-22')
OCAML_FAS=$(printenv OCAML_FAS || printf '%s/_build/default/bin/main.exe' "$ROOT")
export FAS_CC="$CC"
ERR_TMP=$(mktemp -d)
trap 'rm -rf "$ERR_TMP"' EXIT HUP INT TERM

fail() {
  echo "errors: $*" >&2
  exit 1
}

# run NAME EXPECTED_EXIT FILE... : build with both backends, compare output.
run() {
  name=$1
  want=$2
  shift 2
  for backend in native llvm; do
    if [ "$backend" = llvm ]; then
      FAS_ORACLE=llvm "$OCAML_FAS" -o "$ERR_TMP/$name-$backend" "$@"
    else
      env -u FAS_ORACLE "$OCAML_FAS" -o "$ERR_TMP/$name-$backend" "$@"
    fi
    set +e
    "$ERR_TMP/$name-$backend" >"$ERR_TMP/$name-$backend.out" \
      2>"$ERR_TMP/$name-$backend.err"
    got=$?
    set -e
    [ "$got" -eq "$want" ] ||
      fail "$name ($backend) exited $got, expected $want"
  done
  cmp "$ERR_TMP/$name-native.out" "$ERR_TMP/$name-llvm.out" ||
    fail "$name: native and llvm output differ"
  cmp "$ERR_TMP/$name-native.err" "$ERR_TMP/$name-llvm.err" ||
    fail "$name: native and llvm stderr differ"
}

cd "$ROOT/test"

run errors 10 errors.fas
cat >"$ERR_TMP/errors.want" <<'EOF'
unknown unknown error
open errno 20
compare ok
123 7
errdefer BadDigit
closes failed: BadDigit
errdefer BadDigit
closes failed again
g 203
twice 42
twice errdefer
twice 5
errdefer BadDigit
loop errdefer 3
in_loop BadDigit g 304
indirect 86
sw 1 2 3 3
half 2.5 1.5
flag 1 1
ptr ok fallback
small 200 1 wide -5000000000 7
name NotFound code 2
EOF
cmp "$ERR_TMP/errors-native.out" "$ERR_TMP/errors.want" ||
  fail "errors.fas printed the wrong output"

# syscall[T fails]: every T, try and catch, a generic T, and an error
# escaping main (exit 1, BadFd on stderr).
run syscall_fails 1 syscall_fails.fas
cat >"$ERR_TMP/syscall_fails.want" <<'EOF'
hello
generic
wrote 6
generic 8
pid positive 1
page 4242
open default -1
open NotFound code 2
close BadFd
nosys default 77
nosys NoSys
before
EOF
cmp "$ERR_TMP/syscall_fails-native.out" "$ERR_TMP/syscall_fails.want" ||
  fail "syscall_fails.fas printed the wrong output"
grep -q -x BadFd "$ERR_TMP/syscall_fails-native.err" ||
  fail "syscall_fails.fas: the escaping error was not reported"

# The fails result ABI, checked from both sides against clang.
run errors_abi 0 errors_abi.fas errors_abi.c

# A fails main: the error name goes to stderr and the exit status is 1.
cat >"$ERR_TMP/main_try.fas" <<'EOF'
error Broken
fn step(x i32) i32 fails {
    if x > 2 { fail Broken }
    return x
}
fn main() i32 fails {
    a i32 = try step(1)
    b i32 = try step(a + 2)
    return b
}
EOF
run main_try 1 "$ERR_TMP/main_try.fas"
[ "$(cat "$ERR_TMP/main_try-native.err")" = Broken ] ||
  fail "fails main did not print the error name"

cat >"$ERR_TMP/main_errno.fas" <<'EOF'
fn main() fails {
    fail errno_err()
}
EOF
run main_errno 1 "$ERR_TMP/main_errno.fas"
[ "$(cat "$ERR_TMP/main_errno-native.err")" = Unexpected ] ||
  fail "errno_err() with errno 0 is not Unexpected"

cat >"$ERR_TMP/main_ok.fas" <<'EOF'
fn main(argc i32, argv addr) i32 fails {
    return argc + 40
}
EOF
run main_ok 41 "$ERR_TMP/main_ok.fas"

cat >"$ERR_TMP/main_void.fas" <<'EOF'
fn main() fails {
}
EOF
run main_void 0 "$ERR_TMP/main_void.fas"

# Rejections: each program must fail `check` with the given message.
prelude='error A, B
fn f() i32 fails { fail A }
fn g() fails { fail B }
'
reject() {
  name=$1
  message=$2
  printf '%s%s\n' "$prelude" "$3" >"$ERR_TMP/$name.fas"
  if "$OCAML_FAS" check "$ERR_TMP/$name.fas" >"$ERR_TMP/$name.diag" 2>&1; then
    fail "$name: accepted"
  fi
  grep -F -q -- "$message" "$ERR_TMP/$name.diag" ||
    { cat "$ERR_TMP/$name.diag" >&2; fail "$name: wrong diagnostic"; }
}

reject unhandled '`f` can fail and the error is not handled' \
  'fn main() i32 { x i32 = f()
 return x }'
reject unhandled_stmt '`f` can fail and the error is not handled' \
  'fn main() i32 fails { f()
 return 0 }'
reject try_outside '`try` passes the error up, so it needs a `fails` function' \
  'fn main() i32 { x i32 = try f()
 return x }'
reject catch_falls 'this `catch` block can fall through' \
  'fn main() i32 { x i32 = f() catch { }
 return x }'
reject catch_void_value 'the call has no result, so `catch` has no value to use' \
  'fn main() i32 { g() catch 1
 return 0 }'
reject void_value 'value for `x` is `void`' \
  'fn main() i32 fails { x i32 = try g()
 return x }'
reject fail_outside '`fail` needs a `fails` function' \
  'fn main() i32 { fail A }'
reject fail_in_defer '`fail` is not allowed inside defer or errdefer' \
  'fn h() fails { defer { fail A }
 return }
fn main() i32 { return 0 }'
reject errdefer_outside '`errdefer` needs a `fails` function' \
  'fn main() i32 { errdefer { }
 return 0 }'
reject fail_int 'out of range for error' \
  'fn main() i32 fails { fail 3 }'
reject same_code 'errors `C` and `D` have the same code; rename one' \
  'error C = 5, D = 5
fn main() i32 { return 0 }'
reject different_codes 'error `A` is declared with different codes' \
  'error A = 7
fn main() i32 { return 0 }'
reject pin_high 'a pinned error code must be an errno code in 1-4095' \
  'error C = 4096
fn main() i32 { return 0 }'
reject pin_zero 'a pinned error code must be an errno code in 1-4095' \
  'error C = 0
fn main() i32 { return 0 }'
reject switch_default 'a switch on an error needs a `default` arm' \
  'fn main() i32 { e error = A
 switch e {
 case A: return 1
 case B: return 2
 }
 return 0 }'
reject extern_c 'a C function is never `fails`' \
  'extern "C" { fn ex() i32 fails { return 1 } }
fn main() i32 { return 0 }'
reject bitcast_width 'illegal `bitcast` from `error` to `u64`' \
  'fn main() i32 { x u64 = bitcast[u64](A)
 return 0 }'

# syscall[T fails]: only on a fails form, only 8-byte results, always handled
reject syscall_plain_try '`syscall` does not fail' \
  'fn main() i32 fails { x i64 = try syscall(39)
 return 0 }'
reject syscall_unhandled '`syscall[fails]` can fail and the error is not handled' \
  'fn main() i32 { x i64 = syscall[fails](39)
 return 0 }'
reject syscall_narrow 'syscall result must be `i64`, `u64` or `addr`, got `i32`; narrow' \
  'fn main() i32 fails { x i32 = try syscall[i32 fails](39)
 return 0 }'
reject syscall_float 'syscall result must be `i64`, `u64` or `addr`, got `f64`' \
  'fn main() i32 fails { x f64 = try syscall[f64 fails](39)
 return 0 }'
reject syscall_no_fails '`syscall` takes type arguments only with `fails`' \
  'fn main() i32 { x u64 = syscall[u64](39)
 return 0 }'

# Redeclaring an error with the same name is the same error (modules
# may each declare it).
printf '%s%s\n' "$prelude" 'error A
fn main() i32 { return 0 }' >"$ERR_TMP/same_name.fas"
"$OCAML_FAS" check "$ERR_TMP/same_name.fas" ||
  fail "same_name: redeclaring an error was refused"

echo "errors: ok"
