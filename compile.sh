#!/bin/sh
# Builds a statically linked fas (and fas-fir) that runs on any x86-64 Linux
# machine without OCaml or shared libraries, and copies std/ next to it.
#
#   ./compile.sh                 build the committed HEAD
#   ./compile.sh REV             build a commit, branch or tag
#   ./compile.sh --worktree      build the working tree, uncommitted edits too
#   OUT=dir ./compile.sh ...     output directory (default ./fas-static)
#
# Needs: git, dune and OCaml (the same as a normal build), plus the static
# glibc archives (libc.a; Ubuntu/Debian: libc6-dev). glibc warns that
# getpwnam & co. need the build machine's glibc at run time; fas does not
# use them.
#
# The result is OUT/fas, OUT/fas-fir and OUT/std. Copy the whole directory
# to the other machine. C imports and --hosted still need a C compiler
# there (FAS_CC, default clang-22); freestanding Fas programs do not.
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
OUT=$(printenv OUT || printf '%s/fas-static' "$ROOT")
REV=HEAD
WORKTREE=0
case "${1:-}" in
  --worktree) WORKTREE=1 ;;
  -h | --help) sed -n '2,19p' "$0"; exit 0 ;;
  "") ;;
  *) REV=$1 ;;
esac

SRC=$(mktemp -d)
trap 'rm -rf "$SRC"' EXIT HUP INT TERM

if [ "$WORKTREE" -eq 1 ]; then
  # tracked and untracked-but-not-ignored files, as they are on disk
  (cd "$ROOT" && git ls-files -z --cached --others --exclude-standard) |
    (cd "$ROOT" && xargs -0 tar -cf - --ignore-failed-read 2>/dev/null) |
    tar -xf - -C "$SRC"
  echo "compile.sh: building the working tree"
else
  git -C "$ROOT" archive "$REV" | tar -xf - -C "$SRC"
  echo "compile.sh: building $(git -C "$ROOT" rev-parse --short "$REV")"
fi

# the only difference from a normal build: link statically
cat >"$SRC/bin/dune" <<'EOF'
(executable
 (name main)
 (link_flags (-ccopt -static))
 (libraries fas_compiler))
EOF
cat >"$SRC/tools/fas_fir/dune" <<'EOF'
(executable
 (name fas_fir)
 (link_flags (-ccopt -static))
 (libraries fas_compiler))
EOF

# glibc's "statically linked applications requires at runtime" warnings are
# expected; everything else is shown
(cd "$SRC" && dune build --root . ./bin/main.exe ./tools/fas_fir/fas_fir.exe 2>&1 |
  grep -v 'statically linked applications requires at runtime\|in function\|^/usr/bin/ld: .*: in function' || true)

[ -x "$SRC/_build/default/bin/main.exe" ] || { echo "compile.sh: build failed" >&2; exit 1; }

mkdir -p "$OUT"
rm -rf "$OUT/std"
install -m 755 "$SRC/_build/default/bin/main.exe" "$OUT/fas"
install -m 755 "$SRC/_build/default/tools/fas_fir/fas_fir.exe" "$OUT/fas-fir"
cp -R "$SRC/std" "$OUT/std"
strip "$OUT/fas" "$OUT/fas-fir" 2>/dev/null || true

if file "$OUT/fas" | grep -q 'statically linked\|static-pie linked'; then :; else
  echo "compile.sh: warning: $OUT/fas is not static:" >&2
  file "$OUT/fas" >&2
fi
"$OUT/fas" --version >/dev/null 2>&1 || true
echo "compile.sh: done: $OUT/fas, $OUT/fas-fir, $OUT/std"
