#!/bin/sh

cd $(dirname "$0")

pp () {
  ../_build/default/.ppx/ppx_deriving_madcast/ppx.exe \
      "$1" \
      -o "${1%.ml}.pp.ml"
}

mkdir -p _build

printf 'let f = [%%madcast: %s]\n' "$1" > _build/show_tmp.ml

pp _build/show_tmp.ml

cat _build/show_tmp.pp.ml | \
    tr '\n' '\r' | \
    sed 's|let f =\s\+let cast :[^=]\+=||' | \
    sed 's|in\s\+cast||' | \
    tr '\r' '\n' | \
    grep .

rm _build/show_tmp.ml _build/show_tmp.pp.ml
