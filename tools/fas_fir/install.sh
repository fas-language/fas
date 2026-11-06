#!/bin/sh
# Builds the compiler and fas-fir from this checkout and installs them as
#   ~/.local/bin/fas-native   (fas with --backend=native always on)
#   ~/.local/bin/fas-fir      (Fir text -> native object / executable)
# The binaries are copied to ~/.local/share/fas-native so a later rebuild or
# a cleaned build dir never breaks the installed tools. Rerun to update.
set -eu
root=$(cd "$(dirname "$0")/../.." && pwd)
build=${FAS_INSTALL_BUILD_DIR:-$root/_build_install}
share=${FAS_INSTALL_SHARE:-$HOME/.local/share/fas-native}
bin=${FAS_INSTALL_BIN:-$HOME/.local/bin}
(cd "$root" && dune build --build-dir="$build" bin/main.exe tools/fas_fir/fas_fir.exe)
mkdir -p "$share" "$bin"
# install via a temp name + rename so a running copy is never overwritten
for f in bin/main.exe:fas tools/fas_fir/fas_fir.exe:fas-fir; do
  src=${f%%:*}; dst=${f##*:}
  cp "$build/default/$src" "$share/.$dst.tmp"
  chmod 755 "$share/.$dst.tmp"
  mv -f "$share/.$dst.tmp" "$share/$dst"
done
rm -rf "$share/std"; cp -r "$root/std" "$share/std"
cat > "$bin/fas-native" <<W
#!/bin/sh
exec "$share/fas" --backend=native "\$@"
W
cat > "$bin/fas-fir" <<W
#!/bin/sh
exec "$share/fas-fir" "\$@"
W
chmod 755 "$bin/fas-native" "$bin/fas-fir"
echo "installed fas-native and fas-fir into $bin (binaries and std/ in $share)"
