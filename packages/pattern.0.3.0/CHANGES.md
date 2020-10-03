# v0.3.0, 2020-09-22

- Port to `ppxlib` 0.16 / `ocaml-migrate-parsetree` 2.0.0

# v0.2.0, 2020-05-15

- moved to GitHub

- do not rely on `ppxlib` anymore (use `metapp`, `metaquot` and `refl`)

- pattern ppx rewriter package renamed as `pattern.ppx`

- pattern runtime package renamed as `pattern`

# v0.1.1, 2019-09-27

- compatible with OCaml 4.09.

- use `ppxlib` parsetree to be more independent from OCaml version.

- added `Pattern_runtime.check` function for a friendlier type inference
  in patterns

# v0.1.0, 2019-05-15

- PPX extension that generates functions from patterns that explain
  match failures by returning the common context and the list of
  differences between a pattern and a value.
