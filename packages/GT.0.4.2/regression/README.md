This is a test suite for regression testing of a library. The compiled executables
produce some output which is compared to the prepared output from the `orig/`
directory. If the output is the same test is passed.

Most of the tests are related to camlp5 syntax extension. The tests related to
`ppx` extension engine are starting from `test7*`.
