(***********************************************************************)
(*                                                                     *)
(*                             Active-DVI                              *)
(*                                                                     *)
(*                   Projet Cristal, INRIA Rocquencourt                *)
(*                                                                     *)
(*  Copyright 2003 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Lesser General Public License.          *)
(*                                                                     *)
(*  Pierre Weis.                                                       *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Global Active-DVI options. *)

(* To print debugging messages. *)
let debug_endline = Options.debug "--debug" " General debug";;

(* Setting the forward in Misc. *)
let loaded = Misc.set_forward_debug_endline debug_endline;;

(* Some global options for Active-DVI. *)

(* let try_epsbygs = true;; *)

let pson =
  if Config.have_gs then
    Options.flag true
      "-nogs"
      " Turn off the display of inlined Postscript.\
      \n\t (the default is to display inlined Postscript)."
  else ref false;;

let get_global_display_mode, set_global_display_mode =
 let global_display_mode = ref false in
 (fun () -> !global_display_mode),
 (fun b ->
    GraphicsY11.set_enable_display_mode b;
    global_display_mode := b);;

Options.add "-fg"
 (Arg.Unit (fun () -> set_global_display_mode true))
 " Set the drawing policy to ``screen only'',\
 \n\t (the default is to draw both on the screen and in the memory).";;

Options.add "-w"
 (Arg.String
    (function
     | "a" -> Misc.set_warnings false
     | "A" -> Misc.set_warnings true
     | s -> raise (Arg.Bad (Printf.sprintf "-w %s is unknown" s))))
 "<flags>  Enable/disable warnings according to <flags>,\
 \n\t A/a enable/disable all warnings\
 \n\t (the default is \"A\", to enable all warnings).";;

let loaded = ()
