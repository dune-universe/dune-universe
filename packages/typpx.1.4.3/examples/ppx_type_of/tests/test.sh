#!/bin/sh
# Test only works when ppx_type_of is installed

ocamlfind ocamlc -package ppx_type_of -o test_ test.ml
./test_
