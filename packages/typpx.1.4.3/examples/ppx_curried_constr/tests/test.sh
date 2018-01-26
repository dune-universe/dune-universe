#!/bin/sh
# Test only works when ppx_curried_constr is installed

ocamlfind ocamlc -package ppx_curried_constr -o test_ test.ml
./test_
