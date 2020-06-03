open Ocaml_conv.Default

module M : sig
  type 'a t = int * float * 'a [@@deriving conv{ocaml}]
end = struct
  type 'a t = int * float * 'a [@@deriving conv{ocaml}]
end
