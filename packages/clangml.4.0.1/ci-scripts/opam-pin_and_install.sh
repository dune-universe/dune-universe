#!/usr/bin/env bash
if [ "$#" -ne 1 ]; then
    >&2 echo "Usage: $0 URL"
    exit 1
fi
set -ex
URL="$1"
cd ~/opam-repository
git pull
opam update

## Pin override
#cd ~
#git clone https://gitlab.inria.fr/tmartine/override.git
#cd ~/override
#opam install --yes dune
#make override.opam
#opam pin add --yes --no-action file://$HOME/override
#
#opam pin add --yes --no-action https://gitlab.inria.fr/tmartine/pattern.git

opam pin add --yes --no-action "$URL"
opam depext --yes --verbose clangml
opam install --yes --with-test clangml
