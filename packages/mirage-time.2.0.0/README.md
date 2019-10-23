## mirage-time -- Time operations for MirageOS

The mirage-time repository defines:
- [Mirage_time.S][1] the signature for time-related operations for MirageOS;
- [Time][2] an implementation of the previous signature for the Unix backend.

[1]: https://mirage.github.io/mirage-time/mirage-time/Mirage_time
[2]: https://mirage.github.io/mirage-time/mirage-time-unix/Time

### Installation

`mirage-time` can be installed with `opam`:

    opam install mirage-time

`mirage-time-unix` can be installed with `opam`:

    opam install mirage-time-unix

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.
