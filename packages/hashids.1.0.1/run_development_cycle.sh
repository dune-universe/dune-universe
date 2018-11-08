#!/bin/bash

# Copyright 2016-2017 Vincent Jacques <vincent@vincent-jacques.net>

set -o errexit

eval `opam config env`
opam install --yes General bisect_ppx bisect-summary
clear

# Debug, tests, coverage
# ======================

rm -f _build/.aliases/default/runtest-*
# https://github.com/aantron/bisect_ppx/blob/master/doc/advanced.md#Dune suggests
# modifying the dune file for release. Let's modify it for tests instead.
sed -i "s/^;\(.*bisect_ppx.*\)$/\1/" dune
dune runtest
sed -i "s/^\(.*bisect_ppx.*\)$/;\1/" dune
echo
bisect-summary _build/default/bisect????.out
echo
bisect-ppx-report -html _build/bisect _build/default/bisect????.out
echo "See coverage report (for General's unit tests) in $(pwd)/_build/bisect/index.html"

# OPAM package
# ============

echo
opam pin add --yes --no-action --kind=path .
opam reinstall --yes hashids

# Examples
# ========

# @todo Compare behavior with the Python and JavaScript implementations

cd examples
dune build --root=. example.exe
diff <(_build/default/example.exe) <(echo "Jys1FWfnhqHy")
cd ..

# Documentation
# =============

if (which sphinxcontrib-ocaml-autodoc && which sphinx-build) >/dev/null
then
    echo
    rm -rf docs _build/sphinx/doctrees
    sphinx-build doc docs -d _build/sphinx/doctrees
    rm -f docs/.buildinfo
    echo "See documentation in $(pwd)/docs/index.html"
fi

echo
echo "Development cycle OK"
