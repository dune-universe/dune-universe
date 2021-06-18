[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
![CI](https://github.com/bitwuzla/ocaml-bitwuzla/workflows/CI/badge.svg)

# ocaml-bitwuzla

[Bitwuzla](https://bitwuzla.github.io) is a Satisfiability Modulo Theories
(SMT) solvers for the theories of fixed-size bit-vectors, floating-point
arithmetic, arrays, uninterpreted functions and their combinations.

This library contains basic bindings for using Bitwuzla in OCaml code.
Bitwuzla sources and dependencies are repackaged for convenient use
with opam.

## From Opam

```bash
opam install bitwuzla
```

## From source

The latest version of `ocaml-bitwuzla` is available on GitHub:
https://github.com/bitwuzla/ocaml-bitwuzla

### Required Dependencies

- [CMake >= 3.7](https://cmake.org)
- [GMP v6.1 (GNU Multi-Precision arithmetic library)](https://gmplib.org)
- [OCaml >= 4.08](https://github.com/ocaml/ocaml)
- [Dune >= 2.7](https://github.com/ocaml/dune)

### Optional Dependencies

- [Zarith](https://github.com/ocaml/Zarith)

### Build

```bash
dune build
```

#### Building the API documentation

To build the API documentation, it is required to install
- [odoc](https://github.com/ocaml/odoc)

```bash
dune build @doc
```

#### Running tests

To run the tests, it is required to install
- [ppx_expect](https://github.com/janestreet/ppx_expect)

```bash
dune runtest
```