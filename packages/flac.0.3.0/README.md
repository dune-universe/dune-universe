ocaml-flac
============

This package contains an OCaml interface for the `flac` library

Please read the COPYING file before using this software.

Prerequisites:
==============

- ocaml
- libflac
- findlib
- ocaml-ogg >= 0.7.0 (optional)
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
$ opam install flac
```

Via `dune` (for developers):
```
$ dune install
```

This should install the library file in the appropriate place.
