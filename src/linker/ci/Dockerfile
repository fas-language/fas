# Ubuntu 24.04 userspace; packages are recorded for each qualification run.
FROM ubuntu:24.04@sha256:fe81560990fae86dc4743202b2cb4d3df98d0bcf949173cecef15ce1127ef852
ARG DEBIAN_FRONTEND=noninteractive
ENV OPAMROOTISOK=1 OPAMJOBS=2
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential binutils clang lld gdb valgrind python3 tcl-dev \
    curl ca-certificates git opam pkg-config unzip bzip2 xz-utils \
    && rm -rf /var/lib/apt/lists/*
RUN opam init --bare --disable-sandboxing -y \
    && opam switch create /opt/fas-ocaml ocaml-base-compiler.5.4.1 -y \
    && opam install --switch=/opt/fas-ocaml dune.3.24.2 -y \
    && opam clean --switch=/opt/fas-ocaml -a -c -s --logs -y \
    && chmod -R a+rX /opt/fas-ocaml
# Chibicc's upstream driver suite inspects static executables with file(1).
RUN apt-get update && apt-get install -y --no-install-recommends file \
    && rm -rf /var/lib/apt/lists/*
ENV PATH="/opt/fas-ocaml/_opam/bin:${PATH}"
WORKDIR /qualification/source
