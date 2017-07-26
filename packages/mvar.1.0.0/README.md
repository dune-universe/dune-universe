ocaml-mvar
==========

[![Build status](https://travis-ci.org/johnelse/ocaml-mvar.png?branch=master)](https://travis-ci.org/johnelse/ocaml-mvar)

An OCaml port of Haskell's [Control.Concurrent.MVar](https://hackage.haskell.org/package/base/docs/Control-Concurrent-MVar.html).

A `'a Mvar.t` is a mutable location which can either be empty, or contain a
value of type `'a`. The location can be written to and read from safely from
multiple concurrent Unix threads.

Depends only on OCaml's native `threads` library, as well as
[oUnit](http://ounit.forge.ocamlcore.org/) for testing.
