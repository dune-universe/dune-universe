open Ocaml_conv.Default

module M : sig
  type 'a t = { x : int; y : 'a } [@@deriving conv{ocaml_of}]
end = struct
  type 'a t = { x : int; y : 'a } [@@deriving conv{ocaml_of}]
end


