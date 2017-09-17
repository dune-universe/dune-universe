#!/bin/bash

# Copyright 2017 Vincent Jacques <vincent@vincent-jacques.net>

set -o errexit

eval `opam config env`
opam install --yes js_of_ocaml-ppx js_of_ocaml-compiler cairo2 jbuilder

if ! [ -d node_modules ]
then
    npm install canvas pixelmatch browserify
fi

clear

jbuilder runtest --dev

# https://github.com/mapbox/pixelmatch#install
node_modules/.bin/browserify -s pixelmatch node_modules/pixelmatch/index.js > _build/default/pixelmatch.js
echo
echo "Have a look at $(pwd)/drawing_tests_in_browser.html"
echo

# OPAM package
# ============

opam pin --yes --no-action add .
opam reinstall --yes JsOfOCairo

cd demo
./demo.sh
