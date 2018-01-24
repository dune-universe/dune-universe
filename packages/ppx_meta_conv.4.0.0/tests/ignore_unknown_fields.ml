open Ocaml_conv.Default

module M : sig
  type t [@@deriving conv {ocaml}]
end = struct
  type t = { x : int; y : float } 
    [@@conv.ignore_unknown_fields]
    [@@deriving conv {ocaml}]
end
