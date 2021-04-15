(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2003 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License.         *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

let apg_out = ref stdout;;
let apg_in = ref stdin;;

let open_apg_out fname = apg_out := open_out_bin fname;;
let open_apg_in fname = apg_in := open_in_bin fname;;

let save_apg_oc, load_apg_ic =
  (fun oc (prog : Apg.program) ->
    output_value oc prog),
  (fun ic ->
    (input_value ic : Apg.program));;

let save_apg fname prog =
  open_apg_out fname;
  let oc = !apg_out in
  save_apg_oc oc prog;
  flush oc;
  if oc != stdout then close_out oc;;

let load_apg fname =
  open_apg_in fname;
  let ic = !apg_in in
  let prog = load_apg_ic ic in
  if ic != stdin then close_in ic;
  prog;;

let run_apg fname =
  Apg_run.run (load_apg fname);;
     
    
