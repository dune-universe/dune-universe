# ocaml-sgf

This is a simple library for parsing the SGF FF[4] file format used to
store game records of board games for two players, and especially the
game of Go. It uses ulex and menhir to do the lexing and
parsing. UTF-8 in SGF files is handled correctly, thanks to sedlex.

## Dependencies

* sedlex
* menhir
* rresult

You can install these dependencies with OPAM.

## Building & installing

`# make install`

## Building & installing in OPAM

`# opam pin add .`

## Using and testing

The program `sgftrip` is a test program that parse and print an SGF
file. It just takes an SGF file as an input, parse it, and print the
result in the standard output. The result should be equivalent to the
input, modulo line breaks / spaces.

The library `Sgf` contain functions to parse and pretty-print SGF
files.