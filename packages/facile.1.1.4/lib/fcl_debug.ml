(***********************************************************************)
(*                                                                     *)
(*                           FaCiLe                                    *)
(*                 A Functional Constraint Library                     *)
(*                                                                     *)
(*            Nicolas Barnier, Pascal Brisset, LOG, CENA               *)
(*                                                                     *)
(* Copyright 2004 CENA. All rights reserved. This file is distributed  *)
(* under the terms of the GNU Lesser General Public License.           *)
(***********************************************************************)
let level = ref (try Sys.getenv "FACILEDEBUG" with Not_found -> "");;
let log = ref stdout;;
let call lev f =
  assert(if !level = "*" || String.contains !level lev then begin f !log ;flush !log end; true);;

let internal_error mesg =
  failwith (Printf.sprintf "Internal error (%s). Please send a bug report to facile@recherche.enac.fr" mesg)

let fatal_error mesg =
  failwith (Printf.sprintf "Fatal error: %s" mesg)

let print_in_assert pred mesg =
  pred || (Printf.fprintf stderr "Fatal error: %s" mesg; flush stderr; false)
