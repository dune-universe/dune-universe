## 2.0.3 (2020-05-08)

### Fixed

- Fixed test build when using Core v0.13

## 2.0.2 (2020-05-07)

### Fixed

- Add missing odoc dependency
- Skip tests if environment variables are not set

## 2.0.1 (2020-05-06)

### Fixed

- Don't build the test folder in release mode (only when running tests)

## 2.0 (2020-05-06)

### Added

- Streaming `execute_` helpers: `execute_map`, `execute_iter`, `execute_fold`, and `execute_pipe`.
- `Param.Array` now supports lists, which is useful for `IN ($1)` clauses.

### Changed

- Make `connect`'s `port` argument optional
- Support Core v0.13
- Result sets that don't contain row data aren't returned. For example, `INSERT ...; SELECT ...` now returns one
  result set instead of two.

### Fixed

- Correctly use `port` when provided
- Various [upstream fixes in `ocaml-freetds`](https://github.com/kennknowles/ocaml-freetds/releases/tag/0.7)
  - Exceptions shouldn't break the connection handle
  - Runtime lock released during queries
- Logging always occurs in an Async context
- Logging occurs in the same Async context as the caller and not a random one
- We depend on Async_extra

### Removed

- `Mssql.Test`. This module was for testing and shouldn't have been part of the public API. We recommend adding a
  module like this to your own code if you want it.
- Semi-broken connection pool (`Mssql.Pool`) removed. Doing this safely requires setting the
  [`RESETCONNECTION` bit](https://docs.microsoft.com/en-us/openspecs/windows_protocols/ms-tds/ce398f9a-7d47-4ede-8f36-9dd6fc21ca43),
  which doesn't seem to be possible in FreeTDS.

## 1.1 (2019-01-29)

- Switch build command from jbuilder to dune

## 1.0 (2019-01-28)

Initial release
