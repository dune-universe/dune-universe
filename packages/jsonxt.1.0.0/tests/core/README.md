# Jsonxt test suite

The Jsonxt test suite covers standards compliance, extension support and
read-write-read tests.

The tests can be run using the dune command

```
dune build @runtests
```

## Compliance tests
Jsonxt supports multiple levels of compliance with the RFC 8259
standard. The compliance tests validate that each compliance level
(Strict, Basic, Extended, Yojson.Safe etc) correctly parses a number
of test files.  The validation process checks that each module
correctly parse or returns an error for each file.

Run compliance only checks with

```
dune build @compliance_tests
```

## Validation tests
Validation checks ensure that the parser are correctly processing
various JSON expressions.  The tests perform standard parses
and comparing with expected result using `sexp`.  In addition,
read-write-read test are performed to ensure writers work
correctly.  Again, `sexp` is used to validate the result.

Run validation only checks with

```
dune build @validation_tests
```

## JSONTestSuite tests
The suite set of tests validates the parses against the freely available
[JSONTestSuite](https://github.com/nst/JSONTestSuite). 
Run the tests with:

```
dune build @run_test_suite
```
Which will run the tests for standard, streaming and monad parsers.
The results are reported as follows:
- pass: The JSON was handled correctly as defined by the test suite
- fail: The parser either succeeded when it should have failed or vice versa
- OKpass: The JSON was parsed but the suite defines this case as indeterminate
- OKfail: The parser failed to parse the JSON but the suite defines this case as indeterminate

There should be no fails, OKpass and OKfail are both fine as the result is implementation
dependent.

## Building the jxtester executable
To build the jxtester.exe without running any tests run

```
dune build @jxtester
```
