[![Build Status](https://travis-ci.org/Chris00/ocaml-interval.svg?branch=master)](https://travis-ci.org/Chris00/ocaml-interval)
[![Build status](https://ci.appveyor.com/api/projects/status/s144ehk5tls6imiu?svg=true)](https://ci.appveyor.com/project/Chris00/ocaml-interval)

Interval
========

This is an [interval arithmetic][] library for OCaml.  Here is a small
example in the REPL:

```ocaml
# #require "interval_intel";;
~/.opam/4.06.1/lib/interval_base: added to search path
~/.opam/4.06.1/lib/interval_base/interval.cma: loaded
~/.opam/4.06.1/lib/interval_intel: added to search path
~/.opam/4.06.1/lib/interval_intel/interval_intel.cma: loaded
# open Interval_intel;;
# let v = I.v 1. 1.;;
val v : Interval.t = {Interval.low = 1.; high = 1.}
# I.sin v;;
- : Interval_intel.t =
{low = 0.841470984807896505; high = 0.841470984807896616}
```

Several [OPAM][] packages are provided by this repository:

- `interval_base`: basic interval library that defines the datatype
  `Interval.t` and uses Intel assembly if possible or C99 instructions
  to perform arithmetic operations.

- `interval_crlibm`: relies on [crlibm][] to implement the interval
  operations.  CRlibm provides *proved* correctly-rounded operations,
  so this is the library of choice for computer assisted proofs.

- `interval_intel`: use Intel FPU instructions with directed rounding
  to implement interval operations.  However, the Intel FPU operations
  may not always be correctly rounded for, say, trigonometric
  functions.

  `interval_intel` uses assembly code to compute all operations with
  proper rounding, and currently **ONLY** works on Intel processors.
  The package has been developed for Linux systems but works on
  MacOSX and Windows when compiled with GCC.

- `interval` is a meta-package installing all the packages above.

Note that `ocamlopt` does float constant propagation in *round to the
nearest* mode which may invalidate interval computations.  Use the
compiler flag `-no-float-const-prop` to deactivate it.

Happy interval programming...

Installation
------------

The easier way to install this library is to use [OPAM][]:

    opam install interval

`interval` is a meta-package that will install all packages mentioned above.

If you cloned this repository, first install [dune][] and
type `make` in the main directory.  This will compile the libraries,
the examples and run basic tests.  You can compile the examples with The
programs of the examples will be in `_build/default/examples/`.

Documentation
-------------

To documentation is build using `dune build @doc` and will be in
`_build/default/_doc/` in HTML format.  You can also consult the
interfaces of

- [Interval](src-base/interval.mli),
- [Interval_intel](src-intel/interval_intel.mli) and
  [Fpu](src-intel/fpu.mli),
- [Interval_crlibm](src-crlibm/interval_crlibm.mli),

or [online](https://chris00.github.io/ocaml-interval/doc/).
It is extremely wise to read the whole documentation, even if you
intend to only use the `Interval_intel` module.

Some examples are available in the `examples/` directory.  There is a
`B_AND_B` sub-directory with an example of a branch-and-bound
algorithm that uses interval arithmetics for function optimization
(the example is for the Griewank function, but you can substitute any
function you like).

Tests
-----

Tests are available in the `tests/` directory.  They are mainly for
debugging purpose and quite complicated.  You may run them (`make
tests`) to check that everything is working properly for your machine.
The `test` program runs also a speed test for your particular
architecture.

Bug reports should be open at
https://github.com/Chris00/ocaml-interval/issues



Remark: This library was originally published on Jean-Marc Alliot
[website](http://www.alliot.fr/fbbdet.html.fr) but was moved to Github
with the permission of the authors.


[interval arithmetic]: https://en.wikipedia.org/wiki/Interval_arithmetic
[OPAM]: https://opam.ocaml.org/
[crlibm]: https://github.com/Chris00/ocaml-crlibm
[dune]: https://github.com/ocaml/dune
