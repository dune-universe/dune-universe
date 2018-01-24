open Ocaml_conv.Default

type 'a t = Foo of 'a
[@@deriving conv{ocaml}]

type u = Bar
[@@deriving conv{ocaml}]

let f = [%derive.ocaml_of: int]
let g = [%derive.ocaml_of: u t]
let h = [%of_ocaml: u t t]
