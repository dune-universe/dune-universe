#!/bin/bash

RED='\033[0;31m'
NC='\033[0m' # No Color

function call {
    echo -e "${RED} $* ${NC}"
    $*
}

call ./opam-bin install

call opam update local-bin
call opam switch remove test_bin -y
call opam switch create test_bin --empty
call opam install ocaml.4.07.1 ocamlfind -y

