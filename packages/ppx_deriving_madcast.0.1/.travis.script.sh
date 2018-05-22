#!/bin/sh
set -e

fold_start () { printf 'travis_fold:start:%s\r\033[33;1m%s\033[0m\n' "$1" "$2"; }
fold_end () { printf 'travis_fold:end:%s\r' "$1"; }

## Initialize OPAM
export PATH=~/.local/bin:$PATH
eval `opam config env`

## Make
fold_start make 'Run `make`...'
make
fold_end make

## Make install
if [ -z "$RUN_TESTS" ]; then
    fold_start install 'Run `make install`...'
    make install
    fold_end install
fi

## Run tests
if [ -n "$RUN_TESTS" ]; then
    fold_start tests 'Run tests...'
    make test
    fold_end tests
fi
