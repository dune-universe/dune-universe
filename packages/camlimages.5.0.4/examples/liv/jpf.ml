(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Jun Furuse, projet Cristal, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1999-2004,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: jpf.ml,v 1.4 2004/09/24 10:55:07 weis Exp $ *)

type quality = int
type enhance = bool

type flags = quality * enhance

let string_of_flags (q, e, c) =
  let qs =
    match q with
    | 2 -> "XX"
    | 1 -> "X"
    | 0 -> ""
    | _ when q >= 3  -> "XXX"
    | _ -> "_" in

  let es = if e then "e" else "" in
  let cs = if c then "c" else "" in
  qs ^ es ^ cs

let parse_flags str =
  let enhanced = String.contains str 'e' in
  let checked = String.contains str 'c' in
  let prefix s = try String.sub str 0 (String.length s) = s with _ -> false in
  let q =
    if prefix "XXX" then 3 else
    if prefix "XX" then 2 else
    if prefix "X" then 1 else
    if prefix "_" then -1 else
    0 in
  q, enhanced, checked

let get_flags file =
  let body, _ext = Images.get_extension file in
  try
    let pos = String.rindex body '~' in
    let flag_str = String.sub body (pos+1) (String.length body - (pos+1)) in
    parse_flags flag_str
  with
  | _ -> 0, false, false

let set_flags file flags =
  let body, ext = Images.get_extension file in
  let realbody =
    try
      let pos = String.rindex body '~' in
      String.sub body 0 pos
    with
    | _ -> body in
  let flags = string_of_flags flags in
  realbody ^ (if flags = "" then "" else "~" ^ flags) ^ "." ^ ext
