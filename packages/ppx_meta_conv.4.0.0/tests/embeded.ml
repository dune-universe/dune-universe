open Ocaml_conv.Default

type 'a mc_embeded = 'a

module M : sig
  type t = < x : int; y : int >
  and t2 = < coords : t mc_embeded;
             color : string >
  [@@deriving conv{ocaml}]
end = struct
  type t = < x : int; y : int >
  and t2 = < coords : t mc_embeded;
              color : string >
  [@@deriving conv{ocaml}]

end

let () =
  Helper.test M.t2_of_ocaml M.ocaml_of_t2
    {|object method x = 1 method y = 2 method color = "red" end|}
