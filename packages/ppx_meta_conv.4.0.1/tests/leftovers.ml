open Camlon
open Ocaml_conv.Default

type 'a mc_leftovers = (string * 'a) list (* should be defined in Meta_conv *)

module M : sig
  type 'a t = { x : int; 
                y : 'a;
                rest : Ocaml.t mc_leftovers;
              } [@@deriving conv{ocaml}]
end = struct
  type 'a t = { x : int; 
                y : 'a;
                rest : Ocaml.t mc_leftovers;
              } [@@deriving conv{ocaml}]
end

let () = Helper.test (M.t_of_ocaml Ocaml_conv.int_of_ocaml) (M.ocaml_of_t Ocaml_conv.ocaml_of_int) {|{x=1; y=2; z=3}|}


