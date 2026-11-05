# Source build/install entry point. DESTDIR supports staging for packaging.
PROJECT_ROOT := $(abspath $(dir $(lastword $(MAKEFILE_LIST)))/../..)
DUNE ?= dune
INSTALL ?= install
PREFIX ?= /usr/local
BINDIR ?= $(PREFIX)/bin
DESTDIR ?=

.PHONY: all test install
all:
	$(DUNE) build --root "$(PROJECT_ROOT)" --profile release src/linker/bin/main.exe

test:
	$(DUNE) runtest --root "$(PROJECT_ROOT)" --profile release src/linker

install: all
	$(INSTALL) -d -- "$(DESTDIR)$(BINDIR)"
	$(INSTALL) -m 755 -- "$(PROJECT_ROOT)/_build/default/src/linker/bin/main.exe" "$(DESTDIR)$(BINDIR)/fas-ld"
