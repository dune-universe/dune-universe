# Dryunit

Dryunit is a tool that allows you to test OCaml code using *Convention over Configuration*.

Your tests are put first, so TDD can get out of your way. We wanted to get the project right and be *dry*. That is why the first implementations do not implement a test framework. You are invited to use [Alcotest][] or [OUnit][] for that.

The big advantage of traditional testing over alternatives is that ***you get to use pure OCaml***. Free of enhancements. Using the exact same syntax you do everything else. Your tests are independent but can use anything in their context. *It's just OCaml, do whatever way you want*.


## Conventions

Conventions are minimal, but necessary. They allow for a good visual distinction when you are interacting with non-test code. They also make configuration simpler.

- All files containing tests should be either called `tests.ml` or `something_tests.ml`.
- All test function names must start with `test`.
- By default, test executables are created per directory and are called `main`. But you do not need to ever see this file.

## Quickstart

Install the command line in your system:

```
opam install dryunit
```

Dryunit works with jbuilder out of the box:

```
mkdir tests
dryunit init > tests/jbuild
```

No other configuration is required. The generated rules will define the executable `tests/main.exe` ready for the default framework. You can also make the framework explicit by using `dryunit init alcotest`.

## Configuration

This is the output of the command `dryunit init`:

```
(executables
 ((names (main))
  (libraries (alcotest))))

(rule
 ((targets (main.ml))
  (deps ( (glob_files tests.ml) (glob_files *tests.ml) (glob_files *Tests.ml) ))
  (action  (with-stdout-to ${@} (run dryunit gen
    --framework alcotest
    ;; --filter "space separated list"
    ;; --ignore "space separated list"
    ;; --ignore-path "space separated list"
  )))))
```

As you see, this is the place to customize your test executable. The definitions in the comments provide a template for common filters, but you can find more information about customizations using `dryunit help` or `dryunit COMMAND - - help`.


## About the extension

This project was originated as a PPX. It turns out this setup introduces the unnecessary preprocess of every test file, needs to be randomly modified at every build to bypass jbuilder cache, and overall, that's not even what most users need.

It is still available as the optional package `ppx_dryunit`. Currently the extension provides roughly the same functionality as the command line, plus the possibility to detect tests only in the current file, which is its recommended setup.

The simplest way to use it is adding this line at the end of your file `main.ml`:

```
let () = [%dryunit]
```

That will generate a default configuration that only sees the current file. You can override any default behavior passing arguments through a record, as shown below. All fields are optionals and might be in any order.

```ocaml
let () =
  [%dryunit
    { cache_dir   = ".dryunit"
    ; cache       = true
    ; framework   = "alcotest"
    ; ignore      = ""
    ; filter      = ""
    ; detection   = "file"
    ; ignore_path = "self"
    }
  ]
```



## Implementation details

- At build time, dryunit will check anything that looks like a test file in the build context and check its internal cache mechanism for preprocessed suites.
- If none is found, an instance of OCaml parser will be created to extract a structured representation of the test file.
- Cache is done in one file for the whole directory. Updated according to timestamps and compiler version. Default directory is (`_build/.dryunit`).
- The extension does nothing if outside a build directory.



[alcotest]: https://github.com/mirage/alcotest
[ounit]: http://ounit.forge.ocamlcore.org/documentation.html
