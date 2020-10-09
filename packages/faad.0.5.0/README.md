# ocaml-faad

This package contains an O'Caml interface for the AAC audio decoder, otherwise known as libfaad.

Please read the COPYING file before using this software.

## Prerequisites:

- ocaml >= 3.0.6 (haven't tried earlier versions)

- libfaad >= 2.5 (haven't tried earlier versions)

- findlib >= 0.8.1 (haven't tried earlier versions)

- dune >= 2.0

## Compilation:

```sh
$ dune build
```

This should build both the native and the byte-code version of the
extension library.

## Installation:

Via `opam`:

```sh
$ opam install faad
```

Via `dune` (for developers):

```sh
$ dune install
```

This should install the library file (using ocamlfind) in the
appropriate place.

