ocaml-opus
==========

This package contains an OCaml interface for the `opus` library

Please read the COPYING file before using this software.

Prerequisites:
==============

- ocaml
- libopus
- findlib
- ocaml-ogg >= 0.7.0
- dune >= 2.0

Compilation:
============

```
$ dune build
```

This should build both the native and the byte-code version of the
extension library.

Installation:
=============

Via `opam`:

```
$ opam install opus
```

Via `dune` (for developers):
```
$ dune install
```

This should install the library file (using ocamlfind) in the
appropriate place.
