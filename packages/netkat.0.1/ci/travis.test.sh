#!/bin/bash
if ! opam switch $OCAML; then
  rm -rf ~/.opam/$OCAML
  opam init --yes --bare
  opam switch create $OCAML
fi
eval $(opam env)
opam pin remove --yes $PACKAGE

# if the opam repo gets corrupted for some reason, simply reinstall opam
if ! opam update; then
  rm -rf ~/.opam
  opam init --yes --bare
  opam switch create $OCAML
  eval $(opam env)
fi

opam upgrade --yes --all
for PIN in $PINS; do
  IFS='@' read PIN_NAME PIN_REPO <<< "${PIN}"
  opam pin add --yes --no-action $PIN_NAME $PIN_REPO
done
opam pin add --yes --no-action --kind path $PACKAGE .
opam install --yes --deps-only $PACKAGE
opam install --yes --verbose --build-doc --build-test --keep-build-dir $PACKAGE
