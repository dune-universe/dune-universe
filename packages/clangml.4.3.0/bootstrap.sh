#!/usr/bin/env bash
set -e -x
rm -rf bootstrap
git checkout origin/bootstrap bootstrap
git reset HEAD bootstrap
m4/download.sh
./autogen.sh
