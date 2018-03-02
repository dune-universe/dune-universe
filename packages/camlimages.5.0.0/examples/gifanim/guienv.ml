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

(* $Id: guienv.ml,v 1.6.2.1 2010/05/13 13:36:09 furuse Exp $ *)

open Gdk
open GMain

let () = ignore @@ GtkMain.Main.init ()

let window = GWindow.window ~title: "gifanim"
(*    ~auto_shrink: true  *)
    ~allow_shrink: true 
    ~allow_grow: true ()

let () = ignore @@ window#connect#destroy ~callback:Main.quit

let visual = window#misc#visual
let quick_color_create = Truecolor.color_creator visual
let quick_color_parser = Truecolor.color_parser visual

let colormap = Gdk.Color.get_system_colormap ()

