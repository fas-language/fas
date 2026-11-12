#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
FIXTURE=$ROOT/test/masked_memory_guard.fas
HARNESS=$ROOT/test/masked_memory_guard.c
GUARD_TMP=$(mktemp -d)
trap 'rm -rf "$GUARD_TMP"' EXIT HUP INT TERM

fail() {
  echo "masked memory guard: $*" >&2
  exit 1
}

for target in portable force-native; do
  ll="$GUARD_TMP/$target.ll"
  "$OCAML_FAS" --target="$target" --emit-llvm "$FIXTURE" >"$ll"
  if grep -Eq 'getelementptr inbounds| (nsw|nuw)( |$)' "$ll"; then
    fail "$target LLVM contains an unproved pointer or arithmetic fact"
  fi
  if [ "$target" = force-native ]; then
    grep -Eq 'call <8 x i32> @llvm\.masked\.load\.v8i32\.p0\(ptr %[^,]+, i32 1, <8 x i1> %[^,]+, <8 x i32> %[^)]+\)' "$ll" \
      || fail "$target u32x8 masked load is not the llvm masked load intrinsic with alignment 1"
    grep -Eq 'call <16 x i8> @llvm\.masked\.load\.v16i8\.p0\(ptr %[^,]+, i32 1, <16 x i1> %[^,]+, <16 x i8> ' "$ll" \
      || fail "$target u8x16 runtime-mask masked load is not the llvm masked load intrinsic with alignment 1"
  else
    if grep -q 'llvm\.masked\.load' "$ll"; then
      fail "$target masked loads must use the per-lane form, found an llvm masked load intrinsic"
    fi
  fi
  grep -Eq 'call void @llvm\.masked\.store\.v8i32\.p0\(<8 x i32> %[^,]+, ptr %[^,]+, i32 1, <8 x i1> %[^)]+\)' "$ll" \
    || fail "$target u32x8 masked store is not the llvm masked store intrinsic with alignment 1"
  "$LLVM_OPT" -passes=verify "$ll" -disable-output
  "$LLVM_OPT" -S "-passes=default<O2>" "$ll" -o "$GUARD_TMP/$target.O2.ll"
  "$LLVM_OPT" -passes=verify "$GUARD_TMP/$target.O2.ll" -disable-output
done

if grep -qw avx2 /proc/cpuinfo; then
  "$OCAML_FAS" --target=force-native -S -o "$GUARD_TMP/native.s" "$FIXTURE"
  [ "$(grep -c 'vpmaskmovd' "$GUARD_TMP/native.s")" -ge 2 ] \
    || fail "force-native assembly has no vpmaskmovd load and store"
else
  echo "masked memory guard: force-native assembly check skipped (host has no avx2)" >&2
fi

for target in portable force-native; do
  for level in 0 2; do
    exe="$GUARD_TMP/guard.$target.O$level"
    "$OCAML_FAS" --target="$target" "-O$level" -o "$exe" "$FIXTURE" "$HARNESS" \
      2>"$exe.err"
    [ ! -s "$exe.err" ] || fail "$target -O$level build wrote diagnostics"
    timeout 30 "$exe" || fail "$target -O$level guard run failed"
  done
done

printf 'masked memory guard: straddling masked loads and stores verified at O0/O2, portable and force-native: ok\n'
