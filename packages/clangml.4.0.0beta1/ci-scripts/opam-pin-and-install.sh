#!/usr/bin/env bash
set -ex
URL="$1"
git pull
opam update
opam pin add -yn "$URL"
opam depext -yi clangml
