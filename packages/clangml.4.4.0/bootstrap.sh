#!/usr/bin/env bash
set -e -x
branch="$1"
if [ -z "$branch" ]; then
	branch=origin
fi
rm -rf bootstrap
git checkout "$branch"/bootstrap bootstrap
git reset HEAD bootstrap
m4/download.sh
./autogen.sh
