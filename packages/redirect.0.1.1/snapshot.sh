#!/bin/bash
set -ex

git checkout --orphan snapshot || git checkout snapshot
git reset --hard origin/master
git reset --soft origin/snapshot || true
project_name=$(grep "^(name [^)]*)$" dune-project)
project_name=${project_name#(name }
project_name=${project_name%)}
dune build ${project_name}.opam
git add --force ${project_name}.opam
if git commit --message="Snapshot for commit $(git rev-parse origin/master)"
then
  git remote set-url origin $(git remote get-url origin \
    | sed 's#^https://\([^/@]*@\)\?\([^/@]*\)/#git@\2:#')
  git push --set-upstream origin snapshot
fi
