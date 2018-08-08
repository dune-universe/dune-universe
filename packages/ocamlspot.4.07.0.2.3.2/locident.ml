(***********************************************************************)
(*                                                                     *)
(*                            OCamlSpotter                             *)
(*                                                                     *)
(*                             Jun FURUSE                              *)
(*                                                                     *)
(*   Copyright 2008-2014 Jun Furuse. All rights reserved.              *)
(*   This file is distributed under the terms of the GNU Library       *)
(*   General Public License, with the special exception on linking     *)
(*   described in file LICENSE.                                        *)
(*                                                                     *)
(***********************************************************************)

(* Long identifiers with locations *)

type desc =
    LLident of string
  | LLdot of t * string
  | LLapply of t * t

and t = {
  lident_desc : desc;
  lident_loc : Location.t
}

let rec format ppf t = match t.lident_desc with
  | LLident s -> Format.pp_print_string ppf s
  | LLdot (t, s) -> Format.fprintf ppf "%a.%s" format t s
  | LLapply (t1, t2) -> Format.fprintf ppf "%a(%a)" format t1 format t2
