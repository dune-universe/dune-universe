[![Build Status](https://travis-ci.com/thvnx/mlfenv.svg?branch=master)](https://travis-ci.com/thvnx/mlfenv)

# mlfenv
OCaml bindings to fenv(3)

The `Fenv` module aims to bind fenv(3) functions for floating-point rounding and exception handling. At this point, only bindings for `fegetround` and `fesetround` are implemented. Please, create an issue if you need more support to fenv functions.

## install

mlfenv depends on `dune` build system (see `opam` to install `dune`), then:

```bash
dune build @all @runtest
```

## usage

See the `tests` for some examples.
