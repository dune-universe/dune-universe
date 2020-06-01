## Tests

`ppx_factory` uses two layers of tests. Adding tests along with your contribution is definitely
encouraged and appreciated!

### Unit tests

You can find unit tests for simple library functions in `test/lib`. They are written using OUnit.
There is one test file per tested library module and the entire test suite is defined and run in
`test_ppx_factory_lib.ml`.

We should try testing stuff there as much as possible, especially small helper functions that don't
produce chunks of AST.

Each function has its own test suite under `test_<function_name>` and they are all grouped together
under a `suite` value at the test module's top level.

We try to mimic the module structure of the project in the tests. That means that if you want to add
tests for a function from a submodule B from a module A, it should sit under a `Test_a.B` module
which should have its own `suite`.

### Deriver tests

These are the main tests for `ppx_factory`. We use them to ensure the generated code is what we
expect it to be.

They are run by comparing the output of `ppx_factory` as a standalone binary against `.expected`
files, both for successful and unsuccessful deriving.

The successful cases are splitted between `test/deriver/test_default.ml` and
`test/deriver/test_factory.ml` depending on which part of the plugin they correspond.  You can update
those with new test cases and then run `dune runtest`. It will show the diff with the generated code
for your new case, if you're happy with the result you can update the expected results by running
`dune promote`.

We also test the PPX errors to make sure they are properly triggered and located. There's one file
per error case tested in `test/deriver/errors`. If you want to add a test case there you should:
1. run `touch test/deriver/errors/factory_<test_case_name>.{ml,expected}` to add the new empty test
   cases. Use `factory_` or `default_` as a prefix depending on which of the two derivers they
   correspond to.
2. run `dune runtest --auto-promote` to update the dune file with the rules for your new test case.
3. update the `.ml` file with the erronous use of `ppx_factory`, you can use one of the other test
   cases as an example. If your error is already produced correctly, merlin should highlight it in
   your code.
4. run `dune runtest` to see what the error is.
5. if you're happy with the result, run `dune promote` to update the `.expected` file.
