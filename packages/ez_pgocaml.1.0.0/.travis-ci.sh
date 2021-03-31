#!/bin/sh

export OPAMYES=1 OPAMVERBOSE=1
eval $(opam env)

echo Architecture
uname -a
echo OCaml version
ocaml -version
echo OPAM versions
opam --version
opam --git-version

PREFIX=$HOME/.opam/$OCAML_VERSION

opam install .
opam remove .
