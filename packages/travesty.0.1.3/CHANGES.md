# v0.1.3 (2018-12-13)

- Fix incorrect module name (was `Lib`, not `Travesty`).
- Restrict to OCaml v4.06+ (this was the case in the final v0.1.2
  OPAM release, but not upstream).

# v0.1.2 (2018-12-12)

- Improve API documentation.
- Move functors and concrete modules out of `Intf` files.
- Generally rationalise the interface ready for a public release.
- Add various container modules from `act`: `Singleton`, `T_list`, and
  `T_option`.

# v0.1.1 (2018-12-10)

- Move API documentation, in an attempt to get `dune-release` to work.

# v0.1 (2018-12-10)

- Initial release.
- Existing functionality migrated from `act`'s utils directory.
