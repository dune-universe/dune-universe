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

(* $Id: viewer.ml,v 1.5 2004/09/23 07:20:20 weis Exp $ *)

open Gdk
open Gtk

(* image viewer widget *)

let sync () = while Glib.Main.iteration false do () done

(* screen *)

let screen_width = Gdk.Screen.width ()
let screen_height = Gdk.Screen.height ()

(* the viewer *)

class viewer ?border_width ?width ?height ?packing ?show () =
  let fixed = GPack.fixed ?border_width ?width ?height ?packing ?show () in
  (* let prog =
       new_progress_bar ~packing: (fixed#put ~x:0 ~y:0) ~text: "" () in *)
  let prog = GRange.progress_bar ~packing: (fixed#put ~x:0 ~y:0) () in
  let visual = prog#misc#visual in

  object
    inherit GPack.fixed (Obj.magic fixed#as_widget : Gtk.fixed obj)

    val colormap = Gdk.Color.get_system_colormap ()
    val color_create = Truecolor.color_creator visual
    val color_parser = Truecolor.color_parser visual

    val mutable previous_size = (-1,-1)

    method progress = prog

    method display (ximage : OXimage.ximage) =
      let pixmap =
        let win = fixed#misc#window in
        let pix =
          Gdk.Pixmap.create ~window: win
            ~depth: (Gdk.Visual.depth visual)
            ~width: ximage#width
            ~height: ximage#height () in
        let pixmap = new GDraw.drawable pix in
        pixmap#put_image ~x:0 ~y:0
          ~width: ximage#width ~height: ximage#height
          ~xsrc:0 ~ysrc:0
          ximage#data;
        pix in
      previous_size <- (ximage#width,ximage#height);
      fixed#misc#set_size_request
        ~width: ximage#width ~height: ximage#height ();
      Gdk.Window.set_back_pixmap fixed#misc#window (`PIXMAP pixmap);
      sync ()
end

let viewer ?border_width ?width ?height ?packing ?show () =
  new viewer ?border_width ?width ?height ?packing ?show ()
