#!/usr/bin/env bash
set -ex
commit=$(git rev-parse HEAD)
git checkout snapshot
git reset --hard master
git reset --soft origin/snapshot
grep -q AM_MAINTAINER_MODE configure.ac || \
echo AM_MAINTAINER_MODE >>configure.ac
./bootstrap.sh
git add -f configure.ac Makefile.in aclocal.m4 configure bootstrap
if git commit -m "bootstrapped repository for commit $commit"; then
  git push origin snapshot
fi
git checkout master
