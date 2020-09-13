# Tests

## Preparation

[Run a Tezos sandboxed node and activate its alpha protocol.](https://tezos.gitlab.io/user/sandbox.html)

## Test an ml file

Ex:

```
$ ./test.sh add.ml
```

This compiles `add.ml` in verbose mode under `_build/` directory.  Intermediate codes are saved as `_build/add*.ml`:

* `_build/add.ml`: The original code
* `_build/add_iml_0xxx.ml`: Intemediate code in each transformation step at separate compilation
* `_build/add_iml.ml`: The final IML of the separate compilation
* `_build/add_link_iml_0xxx.ml`: Intermediate code in each transformation step at linking
* `_build/add_link_iml.ml`: The final IML to compile to Michelson
* `_build/add.tz`: The final output Michelson

Then, if `tezos-node` command is available (you should have it if you have prepared a sandboxed node), `test.sh` runs the final output Michelson with

* Parameter: `Unit`
* Storage: `Unit`

by default.  If the execution does not fail, the test is considered successful.

### Custom parameter and storage

You can modify the parameter and storage by adding a special comment at the head of each test `*.ml`.  Here is an example to run the code with parameter `(Int 3, Int 1)` and storage `Int 0`:

```
(* INPUT= (Int 3, Int 1)
   STORAGE= Int 0
*)
open SCaml
...
```

### `MUST_FAIL`

If you declare `MUST_FAIL` in the top comment of your `*.ml`, this test must fail.  If it executes successfully, the test is considered a failure:

```
(* MUST_FAIL *)
open SCaml
let [@entry] main x y = if true then failwith (Int 12) else ([], ())
```

Even if you specify `MUST_FAIL`, the source code must be compiled successfully by SCaml.

## Test all

```
$ ./test_all.sh
```

It runs all the test or aborts at the first test failure.
