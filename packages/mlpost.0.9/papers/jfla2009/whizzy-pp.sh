#!/bin/sh
# echo $* > whizzy-pp-command-line
file=`basename $1 .tex`
ocaml pp.ml $file.new > $file.tex
