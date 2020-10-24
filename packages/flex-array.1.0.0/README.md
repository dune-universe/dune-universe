# flex-array
OCaml implementation of flexible arrays

Flexible arrays are arrays whose size can be changed by adding or
removing elements at either end (one at a time).

This is an implementation of flexible arrays using Braun trees,
following

    Rob Hoogerwoord
    A logarithmic implementation of flexible arrays
    http://alexandria.tue.nl/repository/notdare/772185.pdf

All operations (get, set, cons, tail, snoc, liat) have logarithmic
complexity.
