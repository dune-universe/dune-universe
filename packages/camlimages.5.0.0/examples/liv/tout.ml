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

(* $Id: tout.ml,v 1.14 2004/09/21 18:15:46 weis Exp $ *)

open Livmisc
(* open Gdk *)
(* open GDraw *)
open GMain

let wait = ref 0.0

let hook_next = ref (fun _ -> () : [ `FORCE | `DIR ] option -> unit)
let nexttimeout = ref None

let set_timeout () =
  (* prerr_endline "set timeout"; *)
  if !wait <> 0.0 then
    nexttimeout :=
      Some
        (Timeout.add
           ~ms: (truncate (!wait *. 1000.0))
           ~callback: (fun () -> !hook_next None; false))

let remove_timeout () =
  (* prerr_endline "remove timeout"; *)
  may Timeout.remove !nexttimeout
