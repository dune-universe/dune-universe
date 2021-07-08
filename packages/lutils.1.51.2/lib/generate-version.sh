#!/bin/sh

set -e

SHA=$(git log -1 --pretty=format:"%h" || echo "opam")
VERSION=$(git describe --tags || basename `pwd` | cut -d '.' -f2-4)
echo "let str=\"${VERSION}\"" > lutilsVersion.ml
echo "let sha=\"${SHA}\"" >> lutilsVersion.ml 