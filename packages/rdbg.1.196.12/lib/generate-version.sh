#!/bin/sh

set -e

SHA=$(git log -1 --pretty=format:"%h" || echo "opam")
VERSION=$(git describe --tags || basename `pwd` | cut -d '.' -f2-4)
BRANCH=$(git branch | grep "*" | cut -d ' ' -f 2 || basename `pwd` | echo "opam")

echo "let str=\"${VERSION}\"" > rdbgVersion.ml
echo "let sha=\"${SHA}\"" >> rdbgVersion.ml
echo "let branch=\"${BRANCH}\"" >> rdbgVersion.ml
