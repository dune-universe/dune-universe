[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
![CI](https://github.com/bitwuzla/ocaml-bitwuzla/workflows/CI/badge.svg)

# ocaml-bitwuzla

[Bitwuzla](https://bitwuzla.github.io) is a Satisfiability Modulo Theories
(SMT) solvers for the theories of fixed-size bit-vectors, floating-point
arithmetic, arrays, uninterpreted functions and their combinations.

This library provides an API for using Bitwuzla in OCaml code.
Online documentation is available at
https://bitwuzla.github.io/docs/ocaml.

### Quickstart

You will want to create some expressions and assert formulas.
For example, consider the following SMT-LIB input:

```smt2
(set-logic QF_BV)
(set-option :produce-models true)
(declare-const x (_ BitVec 8))
(declare-const y (_ BitVec 8))
(assert
    (distinct
        ((_ extract 3 0) (bvsdiv x (_ bv2 8)))
        ((_ extract 3 0) (bvashr y (_ bv1 8)))))
(check-sat)
(get-model)
(exit)
```

This input is created and asserted as follows:

```ocaml
  (* First, create a Bitwuzla instance. *)
  let open Bitwuzla.Once () in

  (* Create a bit-vector sort of size 8. *)
  let bv8 = Sort.bv 8 in

  (* Create two bit-vector variables of that sort. *)
  let x = Term.const bv8 "x" and y = Term.const bv8 "y" in

  (* Create bit-vector values one and two of the same sort. *)
  let one = Term.Bv.one bv8 and two = Term.Bv.of_int bv8 2 in

  (* (bvsdiv x (_ bv2 8)) *)
  let sdiv = Term.Bv.sdiv x two in
  (* (bvashr y (_ bv1 8)) *)
  let ashr = Term.Bv.shift_right y one in
  (* ((_ extract 3 0) (bvsdiv x (_ bv2 8))) *)
  let sdive = Term.Bv.extract ~hi:3 ~lo:0 sdiv in
  (* ((_ extract 3 0) (bvashr x (_ sortbv1 8))) *)
  let ashre = Term.Bv.extract ~hi:3 ~lo:0 ashr in

  (*
     (assert
       (distinct
         ((_ extract 3 0) (bvsdiv x (_ sortbv2 8)))
         ((_ extract 3 0) (bvashr y (_ sortbv1 8)))))
  *)
  assert' @@ Term.distinct sdive ashre;
```

After asserting formulas, satisfiability can be determined via
`check_sat`.

```ocaml
  (* (check-sat) *)
  let result = check_sat () in
```

If the formula is satifiable, it is possible to query the value
of expressions via `get_value` as well as its concrete value
via `assignment`.

```ocaml
  assert (result = Sat);

  (* (get-model) *)
  let xval = get_value x and yval = get_value y in
  Format.printf "assignment of x: %s@\n"
  @@ Z.format "%08b" @@ Term.Bv.assignment xval;
  Format.printf "assignment of y: %s@\n"
  @@ Z.format "%08b" @@ Term.Bv.assignment yval;
```

It is also possible to query the model value of expressions that do not
occur in the input formula.

```ocaml
  let x2 = Term.Bv.mul x x in
  let x2val = get_value x2 in
  Format.printf "assignment of x * x: %s@\n"
  @@ Z.format "%08b" @@ Term.Bv.assignment x2val;
```

We can then let the garbage collector delete the Bitwuzla instance,
but if we want to release the resources earlier, it is possible to
call the funcion `unsafe_close` as the last action of the session.

```ocaml
  (* Finally, delete the Bitwuzla instance. *)
  unsafe_close ()
```

### Examples

Other examples together with their SMT-LIB input can be found in directory
[examples](https://github.com/bitwuzla/ocaml-bitwuzla/tree/master/examples):
- an incremental example with `push` and `pop`
([pushpop](https://github.com/bitwuzla/ocaml-bitwuzla/tree/master/examples/pushpop.ml));
- an incremental example with `check-sat-assuming`
([checksatassuming](https://github.com/bitwuzla/ocaml-bitwuzla/tree/master/examples/checksatassuming.ml));
- an unsatisfiable example with unsat core generation
([unsatcore](https://github.com/bitwuzla/ocaml-bitwuzla/tree/master/examples/unsatcore.ml));
- an unsatisfiable example with unsat assumption query
([unsatassumption](https://github.com/bitwuzla/ocaml-bitwuzla/tree/master/examples/unsatassumption.ml)).

## Installation

Bitwuzla sources and dependencies are repackaged for convenient use
with [*opam*](https://opam.ocaml.org/).

### From Opam

```bash
opam depext -i bitwuzla
```

### From source

The latest version of `ocaml-bitwuzla` is available on GitHub:
https://github.com/bitwuzla/ocaml-bitwuzla.

---
:information_source: **Dealing with submodules**

In order to checkout the complete source tree,
run the following at `clone` time:
```bash
git clone --recurse-submodules https://github.com/bitwuzla/ocaml-bitwuzla.git
```
or at any time:
```bash
git submodule init # first time
git submodule update
```

:warning: **Do not** download the source archive (`.zip`, `.tar.gz`).
Download instead the
[tarball](https://github.com/bitwuzla/ocaml-bitwuzla/releases/download/1.0.0/bitwuzla-1.0.0.tbz) from release panel.
```bash
tar -xjf bitwuzla-1.0.0.tbz
```

#### Dependencies

- [GMP v6.1 (GNU Multi-Precision arithmetic library)](https://gmplib.org)
  (**required**)
- [OCaml >= 4.08](https://github.com/ocaml/ocaml) (**required**)
- [dune >= 2.7](https://github.com/ocaml/dune) (**required**)
- [zarith](https://github.com/ocaml/Zarith) (**required**)
- [odoc](https://github.com/ocaml/odoc) (*documentation*)
- [ppx_expect](https://github.com/janestreet/ppx_expect) (*tests*)

---
:information_source: **Handling dependencies with opam**

Dependencies can be automatically installed via
[*opam*](https://opam.ocaml.org/doc/Install.html).

```bash
opam pin add -n .                             # read the package definition
opam depext bitwuzla                          # install system dependencies
opam install --deps-only bitwuzla             # install OCaml dependencies
opam install --deps-only --with-doc bitwuzla  # optional, for documentation
opam install --deps-only --with-test bitwuzla # optional, for tests
```

#### Build

```bash
dune build @install
```

#### Running examples

```bash
dune exec -- examples/quickstart.exe # replace quickstart by other examples
```

#### Building the API documentation

```bash
dune build @doc
```
*An index page containing examples and links to modules can be found in
`_build/default/_doc/_html/index.html`.*

#### Running tests

```bash
dune build @runtest
```

:memo: No output is the expected behavior.
