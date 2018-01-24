open Ocaml_conv.Default

module M : sig
  type 'a t = Foo of { x : int; y : int } [@@deriving conv{ocaml}]
end = struct
  type 'a t = Foo of { x : int; y : int } [@@deriving conv{ocaml}]
end


