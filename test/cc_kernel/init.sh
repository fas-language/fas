#!/bin/sh
# Enter a fresh shell and run a command before asking the kernel to power off.
set -eu
/bin/sh -c 'printf "%s\n" FCC_KERNEL_SHELL_OK'
exec /bin/busybox poweroff -f
