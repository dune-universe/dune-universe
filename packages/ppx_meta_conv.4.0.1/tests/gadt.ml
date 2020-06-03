open Ocaml_conv.Default

let ocaml_of_unit () = assert false

module M : sig
  type 'a t =
    | Foo : int -> unit t
    | Bar : unit t -> bool t
    | Zee : (int * float) -> bool t
  [@@deriving conv{ocaml_of}]
end = struct
  type 'a t =
    | Foo : int -> unit t
    | Bar : unit t -> bool t
    | Zee : (int * float) -> bool t
  [@@deriving conv{ocaml_of}]
end
