# ocaml-semver

Library for handling semantic version numbers ([Semver 2.0.0](http://semver.org/spec/v2.0.0.html)) in OCaml.

## Purpose

This library provides basic functions for working with semantic version numbers:

- Parsing and printing version strings
- Comparing version numbers for precedence
- Checking validity of version numbers according to the Semver spec

Please refer to the module documentation for its API.

## Local development

An OCaml and Opam environment is required. Library dependencies can be installed with:

    $ make install-deps

Then you can run the test suite with:

    $ make test


## Contributing

Contributions are welcome in the form of issues or pull requests. Please remember to:

1. Explain the intended purpose of your change and provide enough context for others to follow your reasoning.
2. Add documentation for new (or existing!) functionality.
3. Include tests to help ensure correctness of added code.

## Copyright and Licensing

(C) 2019 Dividat AG.

Published under MIT license.

