#!/bin/sh

set -e

SHA=$(git log -1 --pretty=format:"%h" || echo "opam")
VERSION=$(git describe --tags || basename `pwd` | cut -d '.' -f2-4)
BRANCH=$(git branch | grep "*" | cut -d ' ' -f 2 || basename `pwd` | echo "opam")

echo "(** Automatically generated from lib/generate-version.sh *) " > lv6version.ml
echo "let tool = \"lv6\"" >> lv6version.ml
echo "let str=\"${VERSION}\"" >> lv6version.ml
echo "let sha=\"${SHA}\"" >> lv6version.ml
echo "let branch=\"${BRANCH}\"" >> lv6version.ml
echo "let maintainer = \"erwan.jahier@univ-grenoble-alpes.fr\"">> lv6version.ml
