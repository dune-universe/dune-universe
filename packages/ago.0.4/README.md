
# README

Ago is a small command line utility to compute the number of days between
two calendar dates. If just one date is given, the current date is assumed
for the second one.

    $ ago 2014-12-10 
    -123

    $ ago 2014-10-11 2014-12-04
    -54
    
# Installation

I recommend to compile and install ago via Opam, the OCaml packaging
manager:

    opam install ago

This installs a binary and a manual page which you can read as usual with
`man ago`.  Please note that the version on Opam might be slightly behind
this repository.

# Building

Ago is implemented in OCaml. It does not rely on libraries outside
of the standard library and was developed with OCaml 4.02.1.

    jbuilder build

# Documentation

The ago utility comes with a Unix manual part which is installed by the
install target. It is built from ago.pod in the repository.

# License

BSD License. See [LICENSE.md](LICENSE.md).

# Author

Christian Lindig <lindig@gmail.com>

# Opam Description

ago(1) - compute the number of days between two calendar dates

Ago computes the difference in days between two calendar dates provided
as arguments. If just one date is given, the current date is taken as
the second one.
