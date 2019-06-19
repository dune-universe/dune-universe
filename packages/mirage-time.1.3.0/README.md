## mirage-time -- Time operations for MirageOS

mirage-time defines:
- [Mirage_time.S][1] the signature for time-related operations for MirageOS;
- [Mirage_time_lwt.S][2] the previous signature specialized Lwt; and
- [Time][3] an implementation of the previous signature for the Unix backend.

[1]: https://mirage.github.io/mirage-time/mirage-time/Mirage_time
[2]: https://mirage.github.io/mirage-time/mirage-time-lwt/Mirage_time_lwt
[3]: https://mirage.github.io/mirage-time/mirage-time-unix/Time

### Installation

`mirage-time` can be installed with `opam`:

    opam install mirage-time

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.
