open Ocaml_conv.Default

type 'a t = Foo | Bar of 'a | Zee of int * float [@@deriving conv{ocaml}]

type t2 = int option [@@deriving conv{ocaml}]

type 'a t3 = 'a option [@@deriving conv{ocaml}]

