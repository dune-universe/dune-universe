## v0.2.0

*2019-07-23*

### Fixed

- Fix a bug in `generate_key` where it would never actually work when used with
  a proper `rng` function (#44, @NathanReb)
- Fix benchmark executable. It is now built (but not executed) as part of tests
  (#45, @emillon)

### Changed

- Use `alcotest` instead of `ppx_expect` for tests (#43, @emillon)

## v0.1.0

*2019-06-28*

- Initial release
