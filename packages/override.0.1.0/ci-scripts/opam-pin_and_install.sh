#!/usr/bin/env bash
if [ "$#" -ne 1 ]; then
    >&2 echo "Usage: $0 URL"
    exit 1
fi
set -ex
URL="$1"
git pull
opam update
opam pin add --yes --no-action "$URL"
opam depext --yes --install clangml
