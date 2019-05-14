#!/usr/bin/env bash
if [ "$#" -ne 1 ]; then
    >&2 echo "Usage: $0 URL"
    exit 1
fi
set -ex
URL="$1"
git pull
opam update
git clone "$URL" override
opam pin add --yes --no-action file:///override/
opam depext --yes override
opam install --yes --deps-only override
cd override
make
make tests
opam install cppo ppx_deriving
make examples
