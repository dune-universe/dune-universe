# OCaml bindings to radamsa

OCaml radamsa is a project to provide OCaml binding for [radamsa](https://gitlab.com/akihe/radamsa).

## Installation

Use [opam](https://opam.ocaml.org/) to install `radamsa` package. This will automatically compile & install `libradamsa` in sandboxed environment.

```
opam install radamsa
```

## Example

`utop` example

```
utop # #require "radamsa";;
utop # Radamsa.radamsa "(1+2)*3";;
- : string = "(1󠁈+1)*3"
utop # Radamsa.radamsa "(1+2)*3";;
- : string = "(1+18446744073709551616)*3"
utop # Radamsa.radamsa ~seed:8 "(1+2)*3";;
- : string = "(2147483649+2)*2"
utop # Radamsa.radamsa ~seed:10 "(1+2)*3";;
- : string = "(0+6135)*-1"
```
