ocaml-unix-errno
================

[ocaml-unix-errno](https://github.com/dsheets/ocaml-unix-errno) provides
an errno variant similar to `Unix.error` but including POSIX 2008,
Linux, OS X, and FreeBSD constructors. A macro definition type, `defns`
is also provided in order to transport a specific errno-integer map as
is the case with FUSE or 9p2000.u. The types and their functions reside
in `Errno` and are independent of any Unix bindings. This makes the
library's types usable from MirageOS on top of Xen. `Errno_unix`
provides maps to and from `Unix.error`, the present host's errno map, an
errno exception `Error`, and higher-order errno checking functions.

The executable `errno-map` will output a tab-delimited map from errno
macro name to integer. This map can then be read back using the function
`Errno.defns_of_string`.
