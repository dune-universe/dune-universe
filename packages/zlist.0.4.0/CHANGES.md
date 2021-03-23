v0.4.0
-------
2021-03-21

- Remove all dependencies (including in tests)
- Generate the opam file from Dune
- Switch the development environment to use Nix flakes
- Use a Fedora Docker container for CI

v0.3.3
-------
2020-09-23

- Restructure tests so that the package itself has no dependencies other than at build-time

v0.3.2
-------
2020-09-22

- Add a `description` field to the opam file
- Correct the way dependencies are specified in the opam file
- Remove version information from `dune-project`

v0.3.1
-------
2020-09-17

- Correct a reference to a nonexistent function in the documentation

v0.3.0
-------
2020-09-16

- Flatten the structure of the package so that there is a single module `Zlist` with the type `Zlist.t`
- Add to and correct the package's documentation
- Correct the argument order of the fold functions to be consistent with `Stdlib`
- Rename `elems` to `items`
- Move package documentation to a top-level landing page
- Switch to expectation-style tests

v0.2.0
-------
2018-08-12

- Correct the implementation and documentation of `Lazy_list.length`
- Add equality checking
- Rename list conversion functions
- Remove code documentation and examples from `README.md`: these belong in the .mli files
- Switch to an "even" representation instead of an "odd" one
- Switch to the `dune` build system
- Use a dedicated Docker image for CI tests
- Require opam version 2
- Replace `alcotest` with inline unit tests

v0.1.2
-------
2017-01-03

- Corrections to the `opam` file and to the README.

v0.1.1
-------
2017-01-01

- Use `topkg` watermarks for tracking version information.

v0.1.0
-------
2016-12-23

Initial release.
