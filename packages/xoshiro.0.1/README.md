Xoshiro
=======

This library includes OCaml implementation of some pseudorandom number
generators (PRNGs) designed by David Blackman and Sebastiano Vigna behind an
interface that mimmics that of the `Random` module of the standard library.

The Xoshiro generators (for XOR/shift/rotate) are all-purpose generators (**not
cryptographically secure**). Compared to the standard library, they:

- **have a bigger state space**: `xoshiro256++`/`xoshiro256**` generators have a
  period of 2²⁵⁶-1.

- **pass more tests**: `xoshiro256++`/`xoshiro256**` pass the whole
  [BigCrush](http://simul.iro.umontreal.ca/testu01/tu01.html) test suite while
  the `Random` module of the standard library systematically fails some of the
  tests.

Usage
-----

### Drop-in Replacement

The modules in this library are drop-in replacements of the `Random` module of
the standard library. This means you can use `Xoshiro` everywhere where you
would use `Random`. For instance:

- use `Xoshiro.bits` instead of `Random.bits`
- (same for `int`, `bool`, etc. and also for the `State` submodule)
- use `open Xoshiro` instead of `open Random`
- or even write `module Random = Xoshiro` at the beginning of every file.

### Bindings vs. Pure

The library comes in two version: one using C bindings and the other written in
pure OCaml. The C bindings are here for performances. They are usually around
twice faster as their pure counterpart. The pure implementations come from
applications which require it, eg. for programs that compile to JavaScript. The
interface is the same, the only thing that changes is whether you depend on the
library `xoshiro.bindings` or `xoshiro.pure`. By default, `xoshiro` depends on
`xoshiro.bindings`.

### Example

For instance, say you have an executable `crazyrandom` which uses the standard
`Random` module as source of randomness. You can easily switch to using
`Xoshiro` instead by replacing all occurrences of `Random` in `crazyrandom.ml`
by `Xoshiro` (a simple search and replace should do the trick). You can then
compile your executable with a Dune file similar to:

```
(executable
 (name crazyrandom)
 (libraries xoshiro))
```

Now you realise that you also need to compile for JavaScript. Sadly, C bindings
will not work there. You can however keep the same file for `crazyrandom.ml` and
change your Dune file to:

```
(executable
 (name crazyrandom)
 (libraries xoshiro.pure)
 (modes js))
```

You're done!

Installation
------------

We recommend installing via OPAM with `opam install xoshiro`. Otherwise, `make`
followed by `make install` should do the trick, provided you have the required
dependencies.

Documentation
-------------

[Documentation is available online](http://lesboloss-es.github.io/xoshiro/). It
can also be built locally with `make doc` and by pointing a browser to
`doc/index.html`.

Tests & Benchmarks
------------------

The library comes with a set of tests and benchmarks. Tests are ran at every
push to the repository. The tests include:

- checking that bindings and pure implementation yield the same bits;

- checking that implementations comply with our claims with respect to the
  [*Crush](http://simul.iro.umontreal.ca/testu01/tu01.html) test batteries (note
  that, because of time considerations, only SmallCrush is ran in continuous
  integration).

- checking that the `MakeRandom` functors build the same interface as the
  standard library.

It is easy to run other test batteries on generators. For instance, one can run
BigCrush on `xoshiro256++` by calling:

    dune exec xoshiro256plusplus/test/crusher/crusher.exe -- --bigcrush --verbose

The benchmarks allow to compare various PRNGs from this library against each
other and, more importantly, against the standard library. They can be ran using
`dune exec bench/run.exe`. For `xoshiro256++`, we observe that the bindings are
slightly slower than the standard library, and almost twice faster than the pure
implementations.

Related Word
------------

- [Xavier Leroy's PRINGO](https://github.com/xavierleroy/pringo), “pseudo-random
  number generators that support splitting and two interfaces: one stateful, one
  purely functional”. Xavier Leroy's work is very similar to ours: it provides
  new PRNGs for OCaml (including Split mix which we can also find in the package
  `xoshiro`) in an interface similar to that of the standard library (although
  not exactly the same, contrary to what can be obtained with `make-random`).

- [Mirage's Crypto Library](https://github.com/mirage/mirage-crypto), including
  in particular the strong, cryptographically secure, PRNG Fortuna. This is
  further from our work as usage is really not the same.
