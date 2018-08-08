#!/bin/sh

ocamlc -c -bin-annot hogekura.ml
../../ocamlspot --debug hogekura.ml:l4c6

