open Ocaml_conv.Default

module M : sig
  type 'a t = Foo | Bar of 'a | Zee of int * float [@@deriving conv{ocaml}]
end = struct
  type 'a t = Foo | Bar of 'a | Zee of int * float [@@deriving conv{ocaml}]
end


