#!/usr/bin/env bash
if [ "$#" -ne 1 ]; then
    >&2 echo "Usage: $0 URL"
    exit 1
fi
set -ex
URL="$1"
cd ~/opam-repository/
git pull
opam update

# ppx_show still not available on opam
cd ~
git clone https://gitlab.inria.fr/tmartine/ppx_show
cd ppx_show
opam install --yes dune
dune build ppx_show.opam
opam pin add --yes --no-action "file://$PWD/"

cd ~
git clone "$URL" override
opam pin add --yes --no-action file:///override/
opam depext --yes override
opam install --yes --deps-only override
cd override
make
make tests
make examples
