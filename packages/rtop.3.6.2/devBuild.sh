#! /usr/bin/bash

export PATH=$PATH:/cygdrive/c/Users/jwalke/github/graph/_esy/default/store/i/refmterr-3.3.2-f388f53c/bin/
export PATH=$PATH:/cygdrive/c/Users/jwalke/github/graph/_esy/default/store/i/esy_ocaml__s__reason-b35c1f63/bin/
echo $PATH
refmterr dune build -p reason,rtop
