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

val format : Format.formatter -> t -> unit
