ocaml-ogg
=========

This package contains an OCaml interface for the `ogg` library

Please read the COPYING file before using this software.

Prerequisites:
==============

- ocaml
- libogg
- findlib
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
$ opam install samplerate
```

Via `dune` (for developers):
```
$ dune install
```

This should install the library file (using ocamlfind) in the
appropriate place.

