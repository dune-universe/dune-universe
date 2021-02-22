# Version 0.4.0, 2021-02-19

- Port to ppxlib 0.18 and OCaml 4.12 (by kit-ty-kate, #2)

# Version 0.3.0, 2020-09-22

- Port to ppxlib 0.16 / ocaml-migrate-parsetree 2.0.0

- `Metapp.ExtensibleS.destruct_extension` returns a pair `Some (e, attrs)` where
  `e` is the extension and `attrs` is the list of optional attributes.

- Fix bug with nested calls in meta-quotes

- Add `?optional` argument to `Metapp.apply`

- Add `[%%metaverbose]` for logging compilation commands.

# Version 0.2.0, 2020-05-11

- Compatibility with OCaml 4.11.0

- More utility functions in `Metapp` module to construct and destruct OCaml's
  parsetree in a version-independent way

- Functions for invoking OCaml compiler and loading modules dynamically are
  exported in the package `metapp.dyncompile`.

# Version 0.1.0, 2020-02-27

- First release
