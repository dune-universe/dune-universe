ocaml-webtest
=============

[![Build status](https://travis-ci.org/johnelse/ocaml-webtest.png?branch=master)](https://travis-ci.org/johnelse/ocaml-webtest)
[![API reference](https://img.shields.io/badge/docs-API_reference-blue.svg)](https://johnelse.github.io/ocaml-webtest)

A unit test framework, fully compatible with
[js_of_ocaml](https://github.com/ocsigen/js_of_ocaml), and written with
in-browser testing of JavaScript code in mind.

Heavily influenced by [oUnit](http://ounit.forge.ocamlcore.org/).

You may find this helpful if you want to

* test OCaml bindings to a JavaScript library
* write tests for a JavaScript library compiled from OCaml

You could even use this library to test normal OCaml code, but in that case
you're probably better off just using oUnit for the extra features it provides.

## Library contents

`ocaml-webtest` consists of two libraries:

* `webtest`

This has no dependencies, and contains code for creating tests and suites.

* `webtest-js`

This depends on `js_of_ocaml`, and contains code used for running tests in a
browser.

## Creating test suites

`ocaml-webtest` supports two kinds of test cases - synchronous and asynchronous.

Both kinds of test cases can use the assertion functions `assert_true`,
`assert_equal` and `assert_raises` to check for expected behaviour.

Synchronous test cases are functions of type `unit -> unit`, and in order to
pass should return cleanly without throwing an exception.

Some examples of simple synchronous test cases:

```
let sync_test1 () = Webtest.Suite.assert_equal (get_five ()) 5

let sync_test2 () = Webtest.Suite.assert_true (get_value ())

let sync_test3 () = Webtest.Suite.assert_raises MyExn (exception_thrower ())
```

Asynchronous test cases are functions of type
`((unit -> unit) -> unit) -> unit`. When run they are passed a wrapper function
which must be used to wrap any asynchronous code which should be triggered as
part of the test. In order to pass, an asynchronous test case should not only
return cleanly, it should also make sure that the wrapped code runs
successfully. Asynchronous test cases can be used to check that an event handler
associated with a JavaScript object has been called.

An example of an asynchronous test case:

```
let async_test wrapper =
  let js_object = create_object () in

  js_object##onclose :=
    Dom_html.handler (fun _  ->
      wrapper (fun () ->
        assert_true "Object has been closed" (is_closed js_object));
      Js._false);

  js_object##close
```

If you don't need to perform any assertions in the asynchronous code but just
need to check that a handler fired, you can call the wrapper function with
`Async.noop`, which is just an alias for `fun () -> ()`.

Synchronous and asynchronous test cases can be combined into suites using the
functions `>::`, `>:~` and `>:::` - for example:

```
open Webtest.Suite

let suite =
  "suite" >::: [
    "sync_test1" >:: sync_test1;
    "sync_test2" >:: sync_test2;
    "sync_test3" >:: sync_test3;
    "async_test" >:~ async_test;
  ]
```

## In-browser testing

Once you've created a suite, you can integrate it into an HTML document using
`Webtest_js.Runner.setup`:

```
let () = Webtest_js.Runner.setup suite
```

This will create the global JavaScript object `webtest` which exposes a simple
API for running the test suite.

* `webtest.run` is a function with no arguments - calling it will run the test
  suite.
* `webtest.finished` is a boolean indicating whether the suite run has finished.
* `webtest.passed` is a boolean indicating whether all the tests passed.
* `webtest.log` contains the log produced by running the tests.

This API can be used by browser automation tools such as
[Selenium WebDriver](http://www.seleniumhq.org/projects/webdriver/). For an
example implementation in Python, see [test_driver.py](test/test_driver.py).

A "known good" setup for automating browser tests (and what this repository
uses for CI) is:

* Firefox 50
* Python 2.7 with version 3.3.0 of the Selenium bindings
* geckodriver 0.15.0
* Xvfb (optional, unless running on a headless machine)

## Examples

`ocaml-webtest` tests itself in both OCaml
([run_tests_ocaml.ml](test/run_tests_ocaml.ml)), in JavaScript
([run_tests_browser.ml](test/run_tests_browser.ml)) and in Node.js
([run_tests_nodejs.ml](test/run_tests_nodejs.ml)).

For a more real-world example, see
[ocaml-webaudio](https://github.com/johnelse/ocaml-webaudio).
