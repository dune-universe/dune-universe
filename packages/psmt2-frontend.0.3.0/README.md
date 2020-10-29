# psmt2-frontend

A library to parse and type-check a conservative extension of the SMT-LIB 2
standard with prenex polymorphism.

## Dependencies

`psmt2-frontend` requieres `4.0.4.0` or higher `menhir` and `ocamlfind`
You can use `make opam-deps` to install dependencies in the current switch

## Build and Install Instructions

The easiest way to install psmt2-frontend is to use OPAM:

    $ opam install psmt2-frontend

If you want to install psmt2-frontend from sources, use the following
instructions:

    $ drom build

to compile and install `psmt2-frontend` on your system.

You can uninstall the library with `make uninstall`.


## Minimal Examples

 See the file `test/example.ml` for a small example of the usage of the library.


## Contributing

Don't hesitate to report encountered bugs on this Git repo's issues
tracker.


## TODO

- Dev is in early stage. This is a first prototype that needs reimplementation

- Needs some documentation.

- Some features of SMT-LIB are not yet supported (Floating point, Bit-vectors, etc)


## Licensing

The library is distributed under the terms of the Apache License version 2.0 (see LICENSE file).


