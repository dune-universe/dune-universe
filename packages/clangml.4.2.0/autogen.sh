#!/usr/bin/env bash
set -e -x
if [ ! -f m4/ax_compare_version.m4 ]; then
    set +x
    >&2 echo 'autogen.sh needs to be run *after* m4/download.sh'
    >&2 echo '(autoconf needs macro directory to be initialized)'
    exit 1
fi
autoreconf
touch config/clangml_config.ml
dune build clangml.opam
