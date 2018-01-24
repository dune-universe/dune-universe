open Ocaml_conv.Default

type 'a mc_option = 'a option
module M : sig
  type 'a t = { x : int; 
                y : 'a mc_option } [@@deriving conv{ocaml}]
end = struct
  type 'a t = { x : int; 
                y : 'a mc_option } [@@deriving conv{ocaml}]
end


