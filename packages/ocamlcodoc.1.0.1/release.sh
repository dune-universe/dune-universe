#!/bin/bash
set -e
version="`cat VERSION`"
tagname="v$version"
git config --global user.email "Thierry.Martinez@inria.fr"
git config --global user.name "Thierry Martinez"
current_dir="`pwd`"
[ -f commit_message ] || git log -1 --format=%B >commit_message
git tag -f -a "$tagname" -m "Version $version"
git push -f git@gitlab.inria.fr:tmartine/ocamlcodoc.git "$tagname"
archive="ocamlcodoc-$tagname.tar.gz"
url="https://gitlab.inria.fr/tmartine/ocamlcodoc/-/archive/$tagname/$archive"
wget "$url"
sha512=`sha512sum "$archive" | cut -d " " -f 1`
cd ~/opam-repository
git pull origin master
if [[ "$version" = 1.0.0 ]]; then
    branch="ocamlcodoc.1"
else
    branch="ocamlcodoc.$version"
fi
git checkout -B "$branch"
repo="packages/ocamlcodoc/ocamlcodoc.$version"
mkdir -p "$repo"
opamfile="$repo/opam"
cp $current_dir/ocamlcodoc.opam "$opamfile"
cat >>$opamfile <<EOF
url {
  src: "$url"
  checksum: "sha512=$sha512"
}
EOF
git add "$opamfile"
git commit -F "$current_dir/commit_message"
git push perso "$branch"
git checkout master
