(***********************************************************************)
(*                                                                     *)
(*                             Active-DVI                              *)
(*                                                                     *)
(*                   Projet Cristal, INRIA Rocquencourt                *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Lesser General Public License.          *)
(*                                                                     *)
(*  Jun Furuse, Didier Rémy and Pierre Weis.                           *)
(*  Contributions by Roberto Di Cosmo, Didier Le Botlan,               *)
(*  Xavier Leroy, and Alan Schmitt.                                    *)
(*                                                                     *)
(*  Based on Mldvi by Alexandre Miquel.                                *)
(***********************************************************************)

(* $Id$ *)

(* To add options to the command line in different modules way *)
let options = ref [];;

let all () = !options;;

let add option_name action man =
  options := (option_name, action, man) :: !options;;

(* A special case: flag options *)
let flag init option_name message =
  let r = ref init in
  add option_name
    (if init then Arg.Clear r else Arg.Set r)
    message;
  r;;

let toggle flag x = flag := !flag

(* Another special case: debug options (init value is false). *)
let debug option_name message =
  let r = ref false in
  add option_name (Arg.Set r) message;
  (fun s -> if !r then prerr_endline s);;

