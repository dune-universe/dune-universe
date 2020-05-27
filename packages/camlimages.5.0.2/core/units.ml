(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            François Pessaux, projet Cristal, INRIA Rocquencourt     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*            Jun Furuse, projet Cristal, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1999-2004,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: units.ml,v 1.1 2006/11/28 15:43:28 rousse Exp $ *)

let inch_cm = 2.54
let inch_pt = 72.0

let units = [
   "mm", 0.1 /. inch_cm *. inch_pt;
   "cm", 1.0 /. inch_cm *. inch_pt;
   "pt", 1.0;
   "in", inch_pt;
 ]

let parse_length s = (* return in pt *)
  let v =
    let l = String.length s in
    let digit,unit =
      if l > 2 then String.sub s 0 2, String.sub s (l-2) 2 else "", "" in
    try
      (List.assoc (String.lowercase unit) units) *. float_of_string digit
    with
    | Not_found -> (* think it is in "pt" *)
      float_of_string s in
  prerr_endline (Printf.sprintf "%s -> %fpt" s v);
  v
