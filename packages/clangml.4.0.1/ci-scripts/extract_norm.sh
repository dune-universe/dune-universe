#!/usr/bin/env bash
set -ex

$build_dir/_build/default/tools/norm_extractor/norm_extractor.exe \
  --trigraphs -x c++ --std $std -i -o $build_dir/norm_$std.ml \
  `sed -n -e 's/^\\\\include{\\([^}]*\\)}/\\1.tex/p' std.tex`
