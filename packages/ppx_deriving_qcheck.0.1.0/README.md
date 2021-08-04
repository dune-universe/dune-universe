# ppx_deriving_qcheck

## Documentation
The documentation can be found [here](https://vch9.github.io/ppx_deriving_qcheck/).

## Arbitrary
Derive `QCheck.arbitrary` on a type declaration

```ocaml
type tree = Leaf of int | Node of tree * tree
[@@deriving arb]
```

### Overwrite arbitrary
If you wan't to specify your own `arbitrary` for any type you can
add an attribute to the type:

```ocaml
type t = (int : [@arb QCheck.(0 -- 10)])
[@@deriving arb]

(* produces *)

let arb : t QCheck.arbitrary = QCheck.(0 -- 10)
```

This attribute has 2 advantages:
* Use your own arbitrary for a specific type (see above)
* Arbitrary is not available for a type
  ```ocaml
  type my_foo =
  | Foo of my_other_type
  | Bar of bool
  [@@deriving arb]
  ^^^^^^^^^^^^^^^^
  Error: Unbound value arb_my_other_type
  
  (* Possible fix *)
  let arb_my_other_type = (* add your implementation here *)
  
  type my_foo =
  | Foo of my_other_type [@arb arb_my_other_type]
  | Bar of bool
  [@@deriving arb]
  ```

## How to use
Install ppx_deriving_qcheck (dev version)
```
$ opam pin add ppx_deriving_qcheck.dev git+https://github.com/vch9/ppx_deriving_qcheck.git#dev
```

Add to your OCaml libraries with dune
```ocaml
...
(libraries ppx_deriving_qcheck)
(preprocess (pps ppx_deriving_qcheck)))
...
```
