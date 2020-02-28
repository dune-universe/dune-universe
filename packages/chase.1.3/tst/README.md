# Test Suite

While `dune` is used to compile and install `chase`, it is developed
using `ocamlbuild`.  After building with `make` in the parent of this
directory, test the current build by typing `make` in this directory.

The output of each test is compared with a file containing the
expected output.  If differences are found, there could be a problem.
Otherwise, the expected outputs can be updated by running the script
`./updatetst` in this directory.

\[[Up](../README.md)\]
