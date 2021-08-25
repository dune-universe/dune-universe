# ppx_make
[![OCaml](https://github.com/erebuxy/ppx_make/actions/workflows/build.yml/badge.svg?branch=master)](https://github.com/erebuxy/ppx_make/actions/workflows/build.yml)

ppxlib based make deriver

## Installation
`ppx_make` can be installed via [OCaml Package Manager](https://opam.ocaml.org/packages/ppx_make/).
```sh
$ opam install ppx_make
```

## Usage
To use this library, add `(preprocess (pps ppx_make))` to the library/executable configuration in `dune` file.

## Syntax
```ocaml
type my_type = {
  my_field : int;
  my_option : int option;
  my_list : int list;
  my_string : string;
  my_default : int; [@default 1024]
}
[@@deriving make]

(* the deriver will automatically generate the function below *)
val make_my_type :
  my_field:int ->
  ?my_option:int ->
  ?my_list:int list ->
  ?my_string:string ->
  ?my_default:int ->
  unit ->
  my_type
```
Please check the [test](test) for more examples.
