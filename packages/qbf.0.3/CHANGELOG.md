# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog][Keep a Changelog] and this project adheres to [Semantic Versioning][Semantic Versioning].

## [Unreleased]

## [0.3][] - 2021-01-11

### Added

- Moved to [Dune](https://github.com/ocaml/dune) which has become the
  de-facto OCaml build tool. We were previously using Oasis, which was
  using Ocamlbuild under the hood and made the builds slightly slower.

### Removed

- `depqbf` support has been temporarily removed due to the move to Dune. A
  PR to re-introduce `depqbf` in Dune would be welcome!

### Changed

- OCaml 4.08.x is now the minimum version required to run this library.

### Fixed

- Fixed the `x86_64` build on Windows + Cygwin64 + the mingw64 toolchain.
- Fixed building ocaml-qbf using opam 2. Opam 2 is sandboxing builds and
  do not accept messing with /tmp.

## [0.2][] - 2016-06-09

### Added

- ocaml-qbf is now [available on
  opam](https://opam.ocaml.org/packages/qbf)! You can install it with:

  ```sh
  opam install qbf
  ```

### Changed

- Updated picosat to version [960](http://fmv.jku.at/picosat/)
- Updated quantor to version [3.2](http://fmv.jku.at/quantor/)

### Fixed

- The library can now be cross-compiled on Cygwin64 and Cygwin32 using the
  `x86_64-w64-mingw32` toolchain.
- The macOS build was also fixed.

[Keep a Changelog]: https://keepachangelog.com/
[Semantic Versioning]: https://semver.org/
[Unreleased]: https://github.com/c-cube/ocaml-qbf/compare/0.3...HEAD
[0.3]: https://github.com/c-cube/ocaml-qbf/compare/0.2..0.3
[0.2]: https://github.com/c-cube/ocaml-qbf/releases/0.2
