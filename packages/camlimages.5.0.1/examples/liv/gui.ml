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

(* $Id: gui.ml,v 1.27 2008/02/19 12:44:04 furuse Exp $ *)

open Gdk
open GDraw
open GMain

let () = ignore @@ GMain.Main.init ()

let active = ref true

let sync () = while Glib.Main.iteration false do () done

(* let window = GWindow.window ~title: "liv" ~allow_shrink: true ~allow_grow: true () *)
let window = GWindow.window ~title: "liv" ()
(* We should not set allow_shrink and allow_grow here. *)

let () =
  ignore @@ window#connect#destroy ~callback:Main.quit;
  window#misc#set_size_request ~width: 1 ~height: 1 ();
  window#resize ~width: 1 ~height: 1;
  window#misc#set_app_paintable true

let drawing = window

let fixed = GPack.fixed ~packing: window#add ~show: true ()

(*
let drawing = 
  GMisc.drawing_area 
    ~width:150 ~height:150
    ~packing: window#add ~show:true ()
*)

(* let fixed = GPack.fixed ~packing: box#add () *)

(*
window#event#connect#configure (fun ev ->
  prerr_endline (Printf.sprintf "Configure %dx%d+%d+%d"
                   (GdkEvent.Configure.width ev)
                   (GdkEvent.Configure.height ev)
                   (GdkEvent.Configure.x ev)
                   (GdkEvent.Configure.y ev));
  false (* continue configure event handling *))
*)

class new_progress_bar obj = object
  inherit GRange.progress_bar obj as super
  val mutable previous = 0.0
  method! set_fraction x =
    let x = floor (x *. 10.0) /. 10.0 in
    if x <> previous then begin 
      super#set_fraction x; sync (); previous <- x 
    end
end

let new_progress_bar =
  GtkRange.ProgressBar.make_params []
    ~cont:(fun pl ?packing ?show () ->
             GObj.pack_return
               (new new_progress_bar (GtkRange.ProgressBar.create pl))
               ~packing ~show)

let prog_on_image = true
 
class prog_nop = object
  method map () = ()
  method unmap () = ()
  method set_text (_s : string) = ()
  method set_fraction (_s : float) = ()
end

class prog (p : GRange.progress_bar) = object
  method map () = fixed#misc#map ()
  method unmap () = fixed#misc#unmap () 
  method set_text = p#set_text
  method set_fraction = p#set_fraction
end

let prog1 = 
  if prog_on_image then 
    let p = 
      new_progress_bar ~packing: (fixed#put ~x:0 ~y:0) ()
    in
    new prog p
  else (new prog_nop :> prog)


let visual = window#misc#visual
let screen_width = Screen.width ()
let screen_height = Screen.height ()
let colormap = Gdk.Color.get_system_colormap ()

let quick_color_create = Truecolor.color_creator visual
let quick_color_parser = Truecolor.color_parser visual

let root_win = Window.root_parent ()

let root_size = Drawable.get_size root_win

let drawing_root = new drawable root_win

let infowindow = GWindow.window ~title:"liv info" ~width:300 ~height:150 ()

let () = 
  infowindow#misc#set_size_request ~width: 300 ~height: 150 (); 
  infowindow#resize ~width: 300 ~height: 150;
  ignore @@ infowindow#connect#destroy ~callback:Main.quit;
  ()

let imglbox0 = GPack.vbox ~packing:infowindow#add ()

let imglbox = GPack.hbox ~packing:imglbox0#add ()

let sb = GRange.scrollbar `VERTICAL
    ~packing:(imglbox#pack ~from:`END ~expand:false) ()

let imglist =
  ((GList.clist ~shadow_type:`OUT
     ~columns: 1 ~packing: imglbox#add ~vadjustment:sb#adjustment ())
   : string GList.clist)

let () = imglist#misc#set_size_request ~width:300 ~height: 150 ()

let prog2 = GRange.progress_bar ~packing: (imglbox0#pack ~expand: false) ()

class progs = object
  method map = prog1#map
  method unmap = prog1#unmap
  method set_format_string s =
    prog1#set_text s;
    prog2#set_text s
  method set_fraction s =
    prog1#set_fraction s;
    prog2#set_fraction s
end

let prog = new progs

let () = sync()
