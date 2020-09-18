#!/bin/bash

RED='\033[0;31m'
NC='\033[0m' # No Color

function call {
    echo -e "${RED} $* ${NC}"
    $*
}

call ./opam-bin install
call ./opam-bin clean
call opam update local-bin

call opam switch remove opam_bin -y
call opam switch create opam_bin 4.07.1 -j 1
call opam install alt-ergo -y -j 1
rm -rf $HOME/.opam/opam_bin.backup
cp -dpR $HOME/.opam/opam_bin $HOME/.opam/opam_bin.backup
mv $HOME/.opam/_opam-bin/opam-bin.log $HOME/.opam/opam_bin.backup/opam-bin.log
call opam switch remove opam_bin -y

call ./opam-bin clean log

call opam update local-bin
call opam switch remove test_bin -y
export OPAM_BIN_FORCE=1
call opam switch create test_bin 4.07.1 -j 1
call opam install alt-ergo -y -j 1

