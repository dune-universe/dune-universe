# ocaml-duppy

ocaml-duppy is an advanced scheduler for Ocaml programmers.

Please read the COPYING file before using this software.

## Prerequisites:

- ocaml >= 4.03.0

- findlib >= 1.8.0

- ocaml-pcre >= 7.3.4

- dune >= 2.0

The code may work with earlier versions but these are the one currently
supported.

## Compilation:

```sh
$ dune build
```

This should build both the native and the byte-code version of the
extension library.

## Installation:

Via `opam`:

```sh
$ opam install duppy
```

Via `dune` (for developers):
```sh
$ dune install
```

This should install the library file (using ocamlfind) in the
appropriate place.

## Author:

This author of this software may be contacted by electronic mail
at the following address: savonet-users@lists.sourceforge.net.
