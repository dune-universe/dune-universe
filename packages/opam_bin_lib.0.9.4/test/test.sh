#!/bin/bash

CURDIR=$(pwd)
export OPAMROOT=$CURDIR/root

RED='\033[0;31m'
NC='\033[0m' # No Color

function call {
    echo -e "${RED} $* ${NC}"
    $* || exit 2
}

function call_safe {
    echo -e "${RED} $* ${NC}"
    $*
}

if [ "$1" != "test2" ]; then

    rm -rf root
    opam init --bare -n
    ../opam-bin install
    opam switch create test1 4.07.1 -y
    opam install why3 -y

fi

export OPAM_BIN_FORCE=true
opam switch create test2 4.07.1 -y
opam install why3 -y
