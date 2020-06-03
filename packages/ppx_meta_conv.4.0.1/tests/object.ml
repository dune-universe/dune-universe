open Ocaml_conv.Default

module M : sig 
  type t = <
    x : int;
    y : int;
  > [@@deriving conv{ocaml}]
end = struct
  type t = <
    x : int;
    y : int;
  > [@@deriving conv{ocaml}]
end
