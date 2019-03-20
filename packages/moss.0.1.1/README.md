OCaml MOSS
==========

The module [Moss](src/moss.mli) is an [OCaml][] client for
the [MOSS][] (Measure Of Software Similarity) plagiarism detection
service.  It is based on the original [submission script][submission].
The MOSS system only runs on Stanford's servers — you cannot run your
own instance — so you need to [obtain an account][MOSS] first.

Install
-------

The easiest way to install the library is to use [OPAM][]:

    opam install moss

To compile by hand, install [dune][] and do `dune build @install`.


Documentation
-------------

See the [interface of Moss](src/moss.mli).



[OCaml]: http://ocaml.org/
[MOSS]: http://theory.stanford.edu/~aiken/moss/
[submission]: http://moss.stanford.edu/general/scripts/mossnet
[OPAM]: https://opam.ocaml.org/
[dune]: https://github.com/ocaml/dune
