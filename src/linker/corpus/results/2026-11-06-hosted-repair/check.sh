#!/bin/bash
set -euo pipefail
cd /qualification/gcc-crt
sha256sum /usr/lib/x86_64-linux-gnu/crt1.o
gcc -dumpfullversion
ldd --version
/qualification/source/_build/default/src/linker/bin/main.exe --version
for mode in fixed pie static static-pie; do
  case "$mode" in
    fixed) flags=(-no-pie);;
    pie) flags=(-pie);;
    static) flags=(-static);;
    static-pie) flags=(-static-pie);;
  esac
  gcc -B/qualification/gcc-crt/tools/ -fno-use-linker-plugin "${flags[@]}" check.c -o "fas-$mode"
  "./fas-$mode"
  gcc -fno-use-linker-plugin "${flags[@]}" check.c -o "gnu-$mode"
  "./gnu-$mode"
  echo "PASS $mode Fas and GNU"
done
