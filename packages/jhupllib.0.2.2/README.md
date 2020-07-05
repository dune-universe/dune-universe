# jhupllib

This repository contains a standard library of sorts for JHU PL Lab projects (although it is by no means limited to those projects).  The library contains the sort of general-purpose miscellany that we wish to use in multiple projects but which do not have sufficient mass to warrant maintenance as independent libraries.

## Installing

`jhupllib` can be installed via [OPAM](http://opam.ocaml.org).

## Building

This project uses [Dune](https://github.com/ocaml/dune) as a build tool.  After cloning this repository, it should be sufficient to perform the following steps:

  1. Install dependencies.

    `opam install jbuilder batteries monadlib ocaml-monadic ppx_deriving yojson ppx_deriving_yojson`

  2. Build the project.

    `make && make test`
