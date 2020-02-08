An OCaml syntax extension (technically, a ppx_deriving plugin) which generates
object-oriented visitors for traversing and transforming data structures.

Here is the [documentation of the latest released version](http://gallium.inria.fr/~fpottier/visitors/manual.pdf).

The easiest way of installing the latest released version of this package is
via `opam`, the OCaml package manager.
```bash
opam update
opam install visitors
```

To install the latest development version, also via `opam`, please proceed as follows:
```bash
  git clone https://gitlab.inria.fr/fpottier/visitors.git
  cd visitors
  make pin
```

To install the latest development version, outside of `opam`, please proceed as follows:
```bash
  git clone https://gitlab.inria.fr/fpottier/visitors.git
  cd visitors
  opam install . --deps-only
  make install
```
