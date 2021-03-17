ocaml-frei0r
============

This package contains an OCaml interface for the `frei0r` library

Please read the COPYING file before using this software.

Prerequisites:
==============

- ocaml
- frei0r
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
$ opam install frei0r
```

Via `dune` (for developers):
```
$ dune install
```

This should install the library file in the appropriate place.
