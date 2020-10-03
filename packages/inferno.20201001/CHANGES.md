# Changes

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
