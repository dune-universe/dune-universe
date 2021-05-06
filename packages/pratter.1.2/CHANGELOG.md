# Changelog
This project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [1.2] -- 2021-05-05
### Added
- Walkthrough in README.md.
### Fixed
- Parsing a binary operator without left context fails.
- Binding power can be negative.

## [1.1] -- 2021-01-23
### Added
- Non associative operators
- Error handling on partially applied operators (which raises a
  `Stream.Failure`)
### Changed
- One function `get` for operators in API
- `make_appl` does not use the table of operators

## [1.0.1] -- 2021-01-16
### Fixed
- Correct OCaml dependency
- Tests comply with OCaml 4.02
### Added
- Gitlab continuous integration

## [1.0] -- 2021-01-14
### Changed
- API: parser uses a data structure passed as argument
- renamed CHANGELOG to CHANGELOG.md

## [0.1.1] -- 2021-01-06
### Added
- Initial version
