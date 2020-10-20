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

(* $Id: gifanim.ml,v 1.12.2.1 2010/05/13 13:36:09 furuse Exp $ *)

open Images
open Ximage

open Guienv

open Gdk
open GMain

open Gif

let () =
  let file = ref "" in
  
  Arg.parse [] (fun s -> file := s) "gifanim file";
  
  let gifseq = Gif.load !file [] in
  
  let darea =
  (* new GMisc.drawing_area :width :height show: true packing: window#add *)
    GMisc.drawing_area ~show: true ~packing: window#add
      ~width: gifseq.screen_width ~height: gifseq.screen_height () in
  
  window#show ();
  let drawing = darea#misc#window in
  prerr_endline "GUI done";
  
  let frames =
    Array.of_list (List.map (fun frame ->
      frame, Ximage.of_image visual None (Index8 frame.frame_bitmap),
             Ximage.mask_of_image window#misc#window
                             (Index8 frame.frame_bitmap) ) gifseq.frames) in
  prerr_endline "Image load done";
  
  let len = Array.length frames in
  
  let gc = GC.create drawing in
  let rect = Rectangle.create ~x:0 ~y:0 ~width: gifseq.screen_width
      ~height: gifseq.screen_height in
  
  let pos = ref 0 in
  
  let rec disp_frame () =
    let frame, image, mask = frames.(!pos) in
    begin match mask with
    | Some bmp ->
        Gdk.GC.set_clip_origin gc ~x: frame.frame_left ~y: frame.frame_top;
        Gdk.GC.set_clip_mask gc bmp
    | None ->
        Gdk.GC.set_clip_origin gc ~x: 0 ~y: 0;
        Gdk.GC.set_clip_rectangle gc rect
    end;
    Gdk.Draw.image drawing gc image.data ~xsrc: 0 ~ysrc:0
      ~xdest: frame.frame_left ~ydest: frame.frame_top
      ~width: frame.frame_bitmap.Index8.width
      ~height: frame.frame_bitmap.Index8.height;
    Gdk.X.flush ();
    if len = 1 then false
    else begin
      incr pos;
      if !pos = len then pos := 0;
      ignore (Timeout.add ~ms:(frame.frame_delay*10) ~callback: disp_frame);
      false
    end in
  
  prerr_endline "first call";
  
  let id = ref None in
  id := Some (window#event#connect#configure ~callback: (fun _ev ->
      begin match !id with
      | Some id -> window#misc#disconnect id
      | None -> ()
      end;
      disp_frame ()));
  
  prerr_endline "Entered main loop";
  
  Main.main ()

