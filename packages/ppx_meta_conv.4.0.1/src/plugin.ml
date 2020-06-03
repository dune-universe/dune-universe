open Ppx_meta_conv

let () = big_one ()

(* Unlike [@@deriving conv{xxx}], [@@deriving xxx] and [%derive.xxx]
   require explicit declaration. Sad. *)
let () = List.iter splitted ["ocaml"; "sexp"; "json"]
