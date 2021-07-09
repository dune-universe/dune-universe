# Memprof-limits — Memory limits, allocation limits, and thread cancellation for OCaml

Memprof-limits is an implementation of (per-thread) global memory
limits, (per-thread) allocation limits, and cancellation of CPU-bound
threads, for OCaml. Memprof-limits interrupts a computation by raising
an exception asynchronously and offers features to recover from them
such as interrupt-safe resources.

The implementation uses OCaml's Memprof engine with a low sampling
rate that does not affect performance.

An explanation of what must be done to ensure one recovers from a
memprof-limits interrupt is provided. It summarises the experience
acquired in OCaml in the Coq proof assistant, as well as in other
situations in many other programming languages. To my knowledge, this
has never been told in textbooks, so I thought it might be of general
interest to the community.

- Home page: https://gitlab.com/gadmm/memprof-limits/
- Documentation: https://guillaume.munch.name/software/ocaml/memprof-limits/
- Changes: https://gitlab.com/gadmm/memprof-limits/-/blob/master/CHANGES.md

Memprof-limits is distributed under the LGPL license version 3 with
linking exception, see LICENSE.

## Installation

Memprof-limits can be installed with Opam: `opam install
memprof-limits`. It requires OCaml 4.12 or newer.

Compilation from sources is done with `dune build`.

## Documentation

- [Guide with use cases and
  examples](https://guillaume.munch.name/software/ocaml/memprof-limits/).
- [Reference
  manual](https://guillaume.munch.name/software/ocaml/memprof-limits/Memprof_limits/).
- A [statistical analysis of the different limits](doc/statistical.md).
- A [guide to recover from
  interrupts](https://guillaume.munch.name/software/ocaml/memprof-limits/recovering.html).
- [FAQ](https://gitlab.com/gadmm/memprof-limits/-/blob/master/FAQ.md).
