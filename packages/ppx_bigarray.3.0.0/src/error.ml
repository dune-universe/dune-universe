(* ppx_bigarray --- An OCaml PPX extension for providing big array literals

   Copyright (C) 2015 Akinori ABE
   This software is distributed under MIT License
   See LICENSE.txt for details. *)

(** [kasprintf] is [Format.asprintf] with continuation. *)
let kasprintf k fmt =
  let buf = Buffer.create 64 in
  let aux ppf = Format.pp_print_flush ppf () ; k (Buffer.contents buf) in
  Format.kfprintf aux (Format.formatter_of_buffer buf) fmt

let exnf ?(loc = !Ast_helper.default_loc) ?(sub=[]) ?(if_highlight="") fmt =
  let k msg () = raise Location.(Error { loc; msg; sub; if_highlight; }) in
  kasprintf k fmt
