#!/usr/bin/env bash
set -x
ocamlfind c   -c -syntax camlp5 -package GT.syntax show.ml -verbose
ocamlfind opt -o sample -syntax camlp5 -package GT.syntax,GT.syntax.all,GT -linkpkg sample.ml
ocamlfind opt -o murec  -syntax camlp5 -package GT.syntax,GT.syntax.all,GT -linkpkg  murec.ml
