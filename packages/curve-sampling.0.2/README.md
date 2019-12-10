[![Build Status](https://travis-ci.org/Chris00/ocaml-curve-sampling.svg?branch=master)](https://travis-ci.org/Chris00/ocaml-curve-sampling)
[![Build status](https://ci.appveyor.com/api/projects/status/rajos1g6j17e6plo?svg=true)](https://ci.appveyor.com/project/Chris00/ocaml-curve-sampling)

Curve Sampling
==============

This module provide a collection of routines to perform adaptive
sampling of parametric and implicit curves as well as manipulating
those samplings.

Install
-------

The easier way to install this library is to use [opam][]:

    opam install curve-sampling

If you prefer to compile by hand, install the dependencies listed in
[curve-sampling.opam](curve-sampling.opam) and issue `dune build
@install`.

[opam]: https://opam.ocaml.org/


Documentation
-------------

The documentation is available in
[curve_sampling.mli](src/curve_sampling.mli) or
[online](https://chris00.github.io/ocaml-curve-sampling/doc/curve-sampling/Curve_sampling/).

Example
-------

Here is a graph of the function x ↦ x sin(1/x) produced with only 227
evaluations of the function.
![x sin(1/x)](https://user-images.githubusercontent.com/1255665/70428344-b302f500-1a76-11ea-9b9e-150ad4794ed6.png)
