#!/bin/sh
ocamlfind ocamlopt -predicates ppx_driver -package ppx_test,ocaml-migrate-parsetree.driver-main -o ppx -linkpkg

