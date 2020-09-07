ocaml-lame

This package contains an OCaml interface for the lame library.

Please read the COPYING file before using this software.

Prerequisites:
==============

- ocaml >= 4.00.1 (haven't tried earlier versions)

- lame

- findlib >= 0.8.1 (haven't tried earlier versions)

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
$ opam install lame
```

Via `dune` (for developers):
```
$ dune install
```

Author:
=======

This author of this software may be contacted by electronic mail
at the following address: savonet-users@lists.sourceforge.net.
