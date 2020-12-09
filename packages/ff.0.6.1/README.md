## OCaml FF

Play with Finite Field in OCaml

This library provides functors to instantiate finite field of arbitrary orders (in the limit of Zarith, the dependency to handle arbitrary integers).

```ocaml
module F13 = Ff.MakeFp (struct let prime_order = Z.of_string "13" end)
module BLSFr = Ff.MakeFp (
  struct
    let prime_order = Z.of_string "52435875175126190479447740508185965837690552500527637822603658699938581184513"
  end
)
```

## JavaScript compatibility

This library can be transpiled in JavaScript using js_of_ocaml.
An example is provided in `js/test_js.ml`, with the corresponding `dune` file.
It instantiates Fp with p = 53. `dune` will compile this into a `FiniteField.js`
file, exporting methods like `add`, `toString`, `random`, etc. `FiniteField` can
be used as a Node module. See `js/test/test_js.js` for an example.

```
# Generate FiniteField.js
dune build js
cp _build/default/js/FiniteField.js ./
node
```

```js
var FF = require("./FiniteField.js");
let x = FF.random();
let y = FF.random();
let x_plus_y = FF.add(x, y);
```

## Install

```
opam install ff
```

For a specific version (from 0.2.1), use
```shell
opam install ff.0.3.0
```
Replace 0.3.0 with the version you want, see [tags](https://gitlab.com/dannywillems/ocaml-ff/tags).


## Documentation

See [here](https://dannywillems.gitlab.io/ocaml-ff/)


## PBT testing

A package `ff-pbt` is also included and published providing Property Based
Testing of finite fields based on the generic finite fields interface given in
`ff`.
If you have a library implementing finite fields, but not using the functors
provided by `ff`, you can use:

```ocaml
(* You module is MyField *)

module MyFieldProperties = Ff_pbt.MakeFieldProperties (MyField)

let () =
  let open Alcotest in
  run "MyField" [MyFieldProperties.get_tests()]
```

It is stronly relying on the `random` function implemented by the finite field module.

## Benchmark

`ff-bench` is a benchmark library (using `Core_bench`) for finite fields, respecting the signature `Ff_sig.BASE`.
Here how to use:

```ocaml
module F337 = Ff.Make (struct let prime_order = Z.of_string "337" end)
module Bench = Ff_bench.MakeBench (F337)
let () =
  let commands = Bench.get_benches "F337" in
  Core.Command.run (Core.Bench.make_command commands)
```
