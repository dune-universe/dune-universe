#!/bin/sh

set -eu

export OCAML_VERSION=4.02.1 # OPAM version to install
export OPAM_VERSION=1.2     # OPAM packages needed to build tests
export OPAM_PACKAGES='dune ounit'

# install ocaml from apt
sudo apt update -qq
sudo apt install -qq ocaml

# install opam
curl -L https://github.com/OCamlPro/opam/archive/${OPAM_VERSION}.tar.gz | tar xz -C /tmp
( cd /tmp/opam-${OPAM_VERSION}
  ./configure
  make lib-ext
  make
  sudo make install
)

opam init -a
opam config setup -a

# install packages from opam
opam switch $OCAML_VERSION
eval "$(opam env)"
opam install -q -y ${OPAM_PACKAGES}

export PATH=/home/travis/.opam/$OCAML_VERSION/bin:$PATH
dune build @all
dune runtest
dune clean
