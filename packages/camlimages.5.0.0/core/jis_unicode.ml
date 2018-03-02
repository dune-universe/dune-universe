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

(* $Id: jis_unicode.ml,v 1.1 2007/01/18 10:29:57 rousse Exp $ *)

open Jis_table

let encode s = (* they must be pure EUC string! *)
  Array.init (String.length s / 2) @@ fun i ->
    let h = (Char.code s.[i * 2] - 0x80)
    and l = (Char.code s.[i * 2 + 1] - 0x80) in
    let pos = ((h - 0x21) * 94 + (l - 0x21)) * 2 in
    int_of_char table.[pos] * 256 + int_of_char table.[pos+1]
