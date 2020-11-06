# Changes

## 2020/11/04

* In the solver's high-level API, introduce a new combinator `range`, which
  allows annotating a constraint with a range of source code positions.
  **Incompatible change:** the exceptions `Unbound`, `Unify`, and `Cycle`
  now carry a range.

* In the solver's high-level API, define the type `deep_ty` of deep types,
  and introduce a new function `build`, which converts a deep type into a
  type variable, allowing it to appear in a constraint.

* In the solver's high-level API, introduce a new function `instance_`. This
  is a variant of `instance`. This function is more convenient (and more
  efficient) than `instance` when one does not need to know how a type scheme
  was instantiated.

## 2020/10/01

* Change the signature `SolverSig.OUTPUT` so as to make `tyvar` an abstract
  type. An injection function `solver_tyvar : int -> tyvar` is introduced.

* Add n-ary products to the System F demo.
  (Contributed by Gabriel Scherer and Olivier Martinot.)

* Some cleanup in the directory structure.

## 2019/09/24

* Use `dune` instead of `ocamlbuild`. All necessary library files
  should now be properly installed (which was not the case in the
  previous version).

## 2018/04/05

* First release of Inferno as an `opam` package.
