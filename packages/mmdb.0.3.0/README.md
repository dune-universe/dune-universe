# ocaml-mmdb

[![Build Status](https://travis-ci.org/issuu/ocaml-mmdb.svg?branch=master)](https://travis-ci.org/issuu/ocaml-mmdb)

OCaml bindings to the MaxMind Geo IP database (also known as GeoIP2).

## Development setup

1. Set up a separate environment with all the dependencies:

   ```sh
   opam switch create . --deps-only ocaml-base-compiler.4.07.1
   ```

1. Build the code:

   ```sh
   make
   ```

1. Run the tests:

   ```sh
   make test
   ```
