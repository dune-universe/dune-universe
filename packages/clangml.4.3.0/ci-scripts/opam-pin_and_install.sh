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

## Pin dependencies

opam depext --yes --verbose --install dune

for package in metapp metaquot traverse refl pattern; do
    git clone https://github.com/thierry-martinez/"$package".git
    ( cd "$package" && dune build "$package".opam )
    opam pin add --yes --no-action -k path "$package"
done

opam pin add --yes --no-action "$URL"
sudo apt-get update
opam depext --yes --verbose clangml
opam install --yes --with-test clangml
