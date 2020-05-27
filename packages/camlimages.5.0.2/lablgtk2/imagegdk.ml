
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

(* $Id: imagegdk.ml,v 1.2 2009/03/01 09:49:53 furuse Exp $*)

let initialized = ref None


let try_initialize () =
  if !initialized = None then begin
    Gdk.Rgb.init ();
    initialized := Some (Gdk.Rgb.get_visual (), Gdk.Rgb.get_cmap ())
  end


open OImages

let draw (obj : #GDraw.drawable) ?x ?y ?dither image =
  try_initialize ();
  match tag image, image#blocks with
  | Rgb24 image, (1,1) ->
      let string = fst (image#unsafe_access 0 0) in
      let buf = Gpointer.region_of_bytes string in
      obj#put_rgb_data ~width: image#width ~height: image#height
	?x ?y ?dither ~row_stride:(image#width * 3) buf
  | _ -> failwith "Gdkrgb.draw"


let to_pixbuf image =
  match tag image, image#blocks with
  | Rgb24 image, (1,1) ->
      let string = fst (image#unsafe_access 0 0) in
      let buf = Gpointer.region_of_bytes string in
      (* string may be GC'ed here? *)
      let pixbuf = 
	GdkPixbuf.from_data ~width: image#width ~height: image#height
	  ~bits: 8 ~rowstride:(image#width * 3) ~has_alpha: false buf
      in
      pixbuf
  | Rgb24 image, (w,h) -> 
      let dest = GdkPixbuf.create 
	~width:image#width ~height:image#height
	~bits: 8 ~has_alpha: false () 
      in
      for x = 0 to w - 1 do
	for y = 0 to h - 1 do
	  let blk = image#dump_block x y in
	  let width = blk.Bitmap.Block.width in
	  let height = blk.Bitmap.Block.height in
	  let buf = Gpointer.region_of_bytes blk.Bitmap.Block.dump in
	  let pixbuf =
	    GdkPixbuf.from_data ~width ~height ~bits: 8
	      ~rowstride:(width * 3) ~has_alpha: false buf 
	  in
	  GdkPixbuf.copy_area ~dest
	    ~dest_x: blk.Bitmap.Block.x
	    ~dest_y: blk.Bitmap.Block.y
	    ~width ~height
	    pixbuf
	done
      done;
      dest
  | Cmyk32 _, _ -> failwith "Gdkrgb.draw cmyk32"
  | _ -> failwith "Gdkrgb.draw"

