open Ocaml_conv.Default

module M : sig
  type 'a t3 = 'a option [@@deriving conv{ocaml}]
end = struct
  type 'a t3 = 'a option [@@deriving conv{ocaml}]
end
