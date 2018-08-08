#!/bin/sh

# This is a sample shell script which tries to call the corresponding OCamlSpotter
# with the current OPAM switch.

DIR=`opam config var bin`

if [ -x $DIR/ocamlspot.opt ]; then 
  $DIR/ocamlspot.opt $*
else 
  if [ -x $DIR/ocamlspot.byt ]; then 
    $DIR/ocamlspot.byt $*
  else 
    echo "ERROR: No ocamlspot.opt or ocamlspot.byt found at $DIR"
  fi
fi
