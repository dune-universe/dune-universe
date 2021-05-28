# Sek

This OCaml library offers an efficient implementation of sequences.

This data structure supports all of the standard operations on stacks, queues,
deques (e.g. `push`, `pop` at either end), catenable sequences (`concat`,
`split`), and random access sequences (`get`, `set`).

Data is stored internally in *chunks* (fixed-capacity arrays),
which is why this data structure is known as a *chunk sequence*.

This data structure comes in two flavors, namely an ephemeral (mutable) flavor
and a persistent (immutable) flavor, and offers constant-time conversions
between these flavors.

It is intended to achieve excellent time complexity and memory usage.

## Installation

To install the latest released version, type:
```
  opam update
  opam install sek
```

To install the latest development version, type:
```
  git clone git@gitlab.inria.fr:fpottier/sek
  cd sek
  opam pin add sek .
```

To install the latest development version *with debugging assertions enabled*,
type:
```
  git clone git@gitlab.inria.fr:fpottier/sek
  cd sek
  make setup
  make install
```

## Documentation

The [documentation of the latest released
version](http://cambium.inria.fr/~fpottier/sek/doc/sek/Sek/index.html)
is available online.
Here are shortcuts to the
[ephemeral sequence API](http://cambium.inria.fr/~fpottier/sek/doc/sek/Sek/Ephemeral/index.html),
the
[persistent sequence API](http://cambium.inria.fr/~fpottier/sek/doc/sek/Sek/Persistent/index.html),
and the
[conversion functions](http://cambium.inria.fr/~fpottier/sek/doc/sek/Sek/index.html#conversion-functions).

The documentation is built locally by `make doc` and can be viewed via `make
view`.

The file [`play.ml`](play.ml) allows playing with the library in the OCaml
toplevel loop.
