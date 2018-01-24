open Ocaml_conv.Default

module M : sig
  type ('a, 'b) record = { a : 'a; b : 'b } [@@deriving conv{ocaml}]
  type t = (int, float) record [@@deriving conv{ocaml}]
end = struct
  type ('a, 'b) record = { a : 'a; b : 'b } [@@deriving conv{ocaml}]
  type t = (int, float) record [@@deriving conv{ocaml}]
end


