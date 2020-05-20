#!/bin/bash

export OPAMYES=1
eval $(opam config env)

opam pin add -n webtest-js .
opam install --deps-only webtest-js
dune build @nodetest
dune build @firefoxtest
