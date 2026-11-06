#!/usr/bin/env bash
# Shared hosted/local correctness recipe. Run with the OCaml tools on PATH.
set -euo pipefail
cd "$(dirname "$0")/../../.."
: "${RUNNER_TEMP:?Set RUNNER_TEMP to a new output directory outside the repository}"
mkdir -p "$RUNNER_TEMP"
dpkg-query -W > "$RUNNER_TEMP/packages.tsv"

python3 src/linker/corpus/fetch.py musl --output "$RUNNER_TEMP/linker-musl" --build-musl
python3 src/linker/corpus/fetch.py zlib --output "$RUNNER_TEMP/linker-zlib"
python3 src/linker/corpus/fetch.py chibicc --output "$RUNNER_TEMP/linker-chibicc-source"
python3 src/linker/corpus/fetch.py sqlite --output "$RUNNER_TEMP/linker-sqlite-source"

dune build --profile release src/linker/bin/main.exe
FAS_MUSL_SYSROOT="$RUNNER_TEMP/linker-musl/sysroot" \
  dune runtest --profile release --force src/linker 2>&1 | tee "$RUNNER_TEMP/linker-regressions.log"
python3 src/linker/scale/run.py _build/default/src/linker/bin/main.exe \
  --output "$RUNNER_TEMP/linker-scale" --profile full
for mode in dynamic static static-pie; do
  flags=()
  if [ "$mode" != dynamic ]; then flags+=("--$mode"); fi
  python3 src/linker/corpus/run.py --output "$RUNNER_TEMP/linker-zlib-$mode" \
    --zlib "$RUNNER_TEMP/linker-zlib/zlib-1.3.1" "${flags[@]}"
done
python3 src/linker/corpus/musl.py --output "$RUNNER_TEMP/linker-musl-zlib" \
  --zlib "$RUNNER_TEMP/linker-zlib/zlib-1.3.1" \
  --musl-sysroot "$RUNNER_TEMP/linker-musl/sysroot" --cc gcc --cc clang
python3 src/linker/corpus/run.py --output "$RUNNER_TEMP/linker-chibicc-selfhost" \
  --chibicc-selfhost "$RUNNER_TEMP/linker-chibicc-source/chibicc-90d1f7f199cc55b13c7fdb5839d1409806633fdb"
flags=()
if [[ "${FAS_SQLITE_EXTRAQUICK:-0}" == 1 ]]; then flags+=(--sqlite-extraquick); fi
python3 src/linker/corpus/run.py --output "$RUNNER_TEMP/linker-sqlite" \
  --sqlite "$RUNNER_TEMP/linker-sqlite-source/sqlite-src-3540100" "${flags[@]}"

make -C src/linker install DESTDIR="$RUNNER_TEMP/linker-install" PREFIX=/usr
"$RUNNER_TEMP/linker-install/usr/bin/fas-ld" --version
as -o "$RUNNER_TEMP/install-start.o" src/linker/test/fixtures/start.s
as -o "$RUNNER_TEMP/install-support.o" src/linker/test/fixtures/support.s
"$RUNNER_TEMP/linker-install/usr/bin/fas-ld" -o "$RUNNER_TEMP/install-program" \
  "$RUNNER_TEMP/install-start.o" "$RUNNER_TEMP/install-support.o"
"$RUNNER_TEMP/install-program"
python3 src/linker/ci/verify.py --output "$RUNNER_TEMP"
