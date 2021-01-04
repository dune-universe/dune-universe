ocaml-lastfm


This package contains an OCaml interface for Lastfm API interface. 

Please read the COPYING file before using this software.

Prerequisites:
==============

- ocaml

- ocamlnet >= 2.2.8 (optional)

- ocaml-xmlplaylist >= 0.1.0

- ocaml-pcre >= 5.12.2

- dune >= 2.0

Compilation:
============

```sh
dune build
```

This should build both the native and the byte-code version of the
extension library.

Installation:
=============

Using [opam](http://opam.ocaml.org/):
```sh
opam install lastfm
```

Using `dune` and a local copy (dev only):
```sh
dune install
```

This should install the library file (using ocamlfind) in the
appropriate place.

Author:
=======

This author of this software may be contacted by electronic mail
at the following address: savonet-users@lists.sourceforge.net.
