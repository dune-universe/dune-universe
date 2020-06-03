open Ocaml_conv.Default

module M : sig
  type t [@@deriving conv{ocaml}]

end = struct
  type t = { x : int [@conv.as X] ;
             y : float [@conv.as {ocaml=Y}] }
      [@@deriving conv{ocaml}]
end
