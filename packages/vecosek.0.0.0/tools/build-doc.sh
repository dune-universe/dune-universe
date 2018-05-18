#!/bin/sh

set -e

export version=$(_build/default/src/app/main.exe --version)
if [ "$1" = "" ] ; then
    export output_dir=_build/doc/html/$version/
else
    export output_dir=_build/doc/html/$1/
fi

ocaml please.ml configure
jbuilder build @install
jbuilder build @doc

rm -fr $output_dir
mkdir -p $output_dir
cp -r _build/default/_doc/_html/* $output_dir/

sed 's@https://smondet.gitlab.io/vecosek/master/@./@g' README.md | \
    pandoc -c odoc.css -s -V pagetitle=Vecosek -o $output_dir/index.html
echo "Done: file://$PWD/$output_dir/index.html"
