# unidecode

Convert unicode strings into its ASCII representation.

The purpose of this library is the same as python's
[unidecode](https://pypi.org/project/Unidecode/) library (version
1.1.1).

Code of the initial release of this library has been extracted from
[GeneWeb](https://github.com/geneweb/geneweb) and adapted to be
released in an independent library.

## Installation

```bash
opam install unidecode
```

## License

Released under the terms of the [GNU GENERAL PUBLIC LICENSE](LICENSE).

## Instructions for developpers

```bash
dune build            # build the library
dune install          # install the built library
dune clean            # clean compilation artifacts
dune runtest          # run unit tests
dune build @runbench  # compare with other libs
```
