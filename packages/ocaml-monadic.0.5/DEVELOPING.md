Developing
----------

This file will contain notes relevant to the development of this library.


Manual Experimentation
======================

To view the output of the `ocaml-monadic` PPX rewriter, a command like the
following is sufficient:

```
ocamlfind ppx_tools/rewriter -ppx '_build/default/.ppx/ocaml-monadic/ppx.exe --as-ppx' src_test/test.ml
```

This requires the OPAM package `ppx_tools_versioned` to be installed.


Packaging
=========

Using OPAM 2, it should be sufficient to test the package by pinning it locally:

```
$ opam install .
```

Once everything is working, update the version number in the `.opam` file.  Determine the hash of the commit to publish (here, `HASH`).  Make sure it's pushed to GitHub.  Then, it's enough to run

```
$ opam publish http://github.com/zepalmer/ocaml-monadic/archive/HASH.zip
```

The command `opam publish` is supposed to sort this out on its own, but as of 2019-05-15, it generates a warning about a virtual package and the resulting PR to OPAM's repositories contains no URL.
