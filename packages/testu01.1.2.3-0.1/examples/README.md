Examples
========

This directory contains the examples from TestU01, as well as their OCaml
version. The C files are here for comparison and we do not provide a way to
compile them. The OCaml files can be run *via* Dune. For instance, to run the
“bat2” example, one would do, from the `bat2` directory:

    dune exec ./bat2.exe

Note that, in the case of this example, it uses the file `vax.bin` and must thus
be run from this directory.

This directory also contains the `*.res` files of expected result, taken
directly from TestU01.

**WIP**: Some examples are missing as some would require bindings that are not
our priority.
