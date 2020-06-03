open Ocaml_conv.Default

type 'a mc_option_embeded = 'a option

module M : sig
  type t = < x : int; y : int >
  and t2 = < coords : t mc_option_embeded;
             color : string >
  [@@deriving conv{ocaml}]
end = struct
  type t = < x : int; y : int >
  and t2 = < coords : t mc_option_embeded;
              color : string >
  [@@deriving conv{ocaml}]

end

