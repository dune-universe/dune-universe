April 2020

`Expect_test_helpers_async` is a library intended for use with expect
tests, i.e. the `let%expect_test` syntax.  It has functionality that
cannot go in `Expect_test_helpers_base` and
`Expect_test_helpers_kernel` due to its use of `Async`, in particular
file i/o, and running external processes.

`Expect_test_helpers_async` uses both `Unix` and `Async`.  It is not
suitable for use in JavaScript.
