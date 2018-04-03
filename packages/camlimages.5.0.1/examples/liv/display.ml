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

(* $Id: display.ml,v 1.36.2.1 2010/05/13 13:36:09 furuse Exp $ *)

open Livmisc
open Gui
open Tout

open Gdk

type filter = [ `SIZE of int * int * [ `ATMOST | `ATLEAST | `NOASPECT ] ]

let current_filters = (ref [] : filter list ref)

let add_size w h =
  List.fold_right
    (fun x st ->
       match x with
       | `SIZE _ -> st
       (* | _ -> x :: st *))
    !current_filters [`SIZE (w,h,`NOASPECT)]

let forward_redisplay = ref (fun _ -> ())

module WINDOW = struct

  let prev = ref (-1,-1)

  let pixmap = ref None

  let set_pixmap pix =
    begin match !pixmap with
    | None -> ()
    | Some pix -> Gdk.Pixmap.destroy pix
    end;
    pixmap := Some pix

  let display pixbuf =
    let width = GdkPixbuf.get_width pixbuf in
    let height = GdkPixbuf.get_height pixbuf in
    let pixmap = Gdk.Pixmap.create ~window:window#misc#window ~width ~height () in
    GdkPixbuf.render_to_drawable pixmap pixbuf;
    prev := (width, height);
    prog#set_format_string "done";
    sync ();
    prog#unmap ();
    (* Resizing window *)
    (* Changes from gtk1.2 to 2.0:
       It is no longer permissable to draw directly on an arbitrary
       widget, or to set an arbitrary widget's background pixmap. If
       you need to do that, use a GtkDrawingArea or (for a toplevel) a
       GtkWindow where gtk_widget_set_app_paintable() has been called. *)
    (* Format.eprintf "resizing %d %d@." ximage#width ximage#height; *)
    drawing#resize ~width ~height;
    Gdk.Window.set_back_pixmap drawing#misc#window (`PIXMAP pixmap);
    set_pixmap pixmap;
    Gdk.Window.clear drawing#misc#window;
    set_pixmap pixmap;
    sync ();
    set_timeout ()

  let init () = 
    ignore (drawing#event#connect#configure ~callback:
	      (fun ev ->
		let w = GdkEvent.Configure.width ev
		and h = GdkEvent.Configure.height ev in
		let rec f w h =
		  if (w, h) <> !prev then begin
		    !forward_redisplay (add_size w h);
		  end;
		  let w', h' = Drawable.get_size drawing#misc#window in
		  if (w', h') <> (w, h) then f w' h' 
		in
		f w h;
		false))
end

module ROOT = struct

  type root_geom = {
      width : int;
      height : int;
      xdest : int;
      xsrc : int;
      ydest : int;
      ysrc : int;
      put_width : int;
      put_height : int;
    }

  let root_geom width height x y =
    let width0 = width in
    let height0 = height in
    let xdest0 = if x < 0 then 0 else x in
    let xsrc0 = if x < 0 then -x else 0 in
    let put_width0 =
      if x + width0 > screen_width then screen_width - xdest0
      else x + width0 - xdest0 in
    let ydest0 = if y < 0 then 0 else y in
    let ysrc0 = if y < 0 then -y else 0 in
    let put_height0 =
      if y + height0 > screen_height then screen_height - ydest0
      else y + height0 - ydest0 in
    { width= width0;
      height= height0;
      xdest= xdest0;
      ydest= ydest0;
      xsrc= xsrc0;
      ysrc= ysrc0;
      put_width= put_width0;
      put_height= put_height0;
    }

  let root_pixmap = lazy (
    let pix =
      GDraw.pixmap ~window
        ~width: screen_width
        ~height: screen_height () 
    in
    pix#set_foreground `BLACK;
    pix#rectangle
      ~x:0 ~y:0 ~width: screen_width ~height: screen_height ~filled: true ();
    pix#pixmap )

  let display_at pixbuf x y =
    let geom = root_geom (GdkPixbuf.get_width pixbuf) (GdkPixbuf.get_height pixbuf) x y in
    GdkPixbuf.render_to_drawable !!root_pixmap ~dest_x: geom.xdest ~dest_y: geom.ydest pixbuf;
    Window.set_back_pixmap root_win (`PIXMAP(!!root_pixmap));
    Window.clear root_win;
    set_timeout ()

end

let working = ref (None : (int * OImages.rgb24 * filter list) option)
let waiting = ref None
let check_waiting () = if !waiting <> None then raise Exit

type root_mode = [ `NONE | `CENTER | `RANDOM ]
type transition = [ `NONE | `MYST | `TRANSPARENT ]
let root_mode = ref (`NONE : root_mode)
let transition = ref (`NONE : transition)

let root_prev_pos = ref None

let display_pixbuf pixbuf =
  match !root_mode with
  | `CENTER | `RANDOM ->
      let width = GdkPixbuf.get_width pixbuf 
      and height = GdkPixbuf.get_height pixbuf in
      let x, y =
        let w = screen_width - width
        and h = screen_height - height in
        match !root_mode with
        | `RANDOM ->
          let w = screen_width - width
          and h = screen_height - height in

          let overwrap x y =
            match !root_prev_pos with
            | None -> 0
            | Some (pw, ph, px, py) ->
              let w = min (x + w - 1) (px + pw - 1) - max x px in
              let h = min (y + h - 1) (py + ph - 1) - max y py in
              if w < 0 || h < 0 then 0 else w * h in

          let random_x_y () =
            let x = if w <= 0 then w / 2 else Random.int w
            and y = if h <= 0 then h / 2 else Random.int h in
            (x, y), overwrap x y in

          let min = ref (random_x_y ()) in
          for _i = 0 to 5 do
            let (x, y), over = random_x_y () in
              if snd !min > over then min := (x, y), over
          done;
          let x, y = fst !min in
          root_prev_pos := Some (w, h, x, y);
          x, y
        | _ -> w/2, h/2 in
      ROOT.display_at pixbuf x y
  | _ -> WINDOW.display pixbuf

let sort_filters (filters : filter list) =
  let rec get_size = function
    | [] -> []
    | `SIZE x :: _ -> [`SIZE x]
    | _ :: xs -> get_size xs in
(*
  let rec get_normalize = function
    | [] -> []
    | `NORMALIZE :: _ -> [`NORMALIZE]
    | _ :: xs -> get_normalize xs in
  let rec get_enhance = function
    | [] -> []
    | `ENHANCE :: _ -> [`ENHANCE]
    | _ :: xs -> get_enhance xs in
*)
  List.flatten [(*get_enhance filters; get_normalize filters; *)get_size filters]

let resize w h cond old =
  let old_width = GdkPixbuf.get_width old in
  let old_height = GdkPixbuf.get_height old in
  let xmag,ymag =
    let xmag = float w /. float old_width
    and ymag = float h /. float old_height in

    let xmag,ymag =
      match cond with
      | `ATMOST ->
        let mag = if xmag > ymag then ymag else xmag in
        if mag > 1.0 then 1.0, 1.0 else mag, mag
      | `ATLEAST ->
        let mag = if xmag > ymag then xmag else ymag in
        if mag < 1.0 then 1.0, 1.0 else mag, mag
      | `NOASPECT -> xmag, ymag in

    let nw = truncate (float old_width *. xmag)
    and nh = truncate (float old_height *. ymag) in

    if nw > fst root_size || nh > snd root_size then
      let xmag = float (fst root_size) /. float old_width
      and ymag = float (snd root_size) /. float old_height in
      match cond with
      | `NOASPECT -> xmag, ymag
      | _ -> if xmag > ymag then ymag, ymag else xmag, xmag
    else xmag, ymag in

  if xmag = 1.0 && ymag = 1.0 then old else

  let nw = truncate (float old_width *. xmag)
  and nh = truncate (float old_height *. ymag) in
  prog#map ();

  let fmts =
    if xmag > 1.0 && ymag > 1.0 then
      Printf.sprintf "enlarging to %dx%d" nw nh else
    if xmag < 1.0 && ymag < 1.0 then
      Printf.sprintf "reducing to %dx%d" nw nh
    else
      Printf.sprintf "resizing to %dx%d" nw nh in

  prog#set_format_string fmts;

  if nw <> old_width || nh <> old_height then begin
    (* original pixbuf will be GC'ed automatically *)
    let pixbuf' = GdkPixbuf.create ~width: nw ~height: nh () in
    GdkPixbuf.scale 
      ~dest: pixbuf' ~dest_x: 0 ~dest_y: 0 ~width: nw ~height: nh
      ~ofs_x: 0. ~ofs_y: 0. ~scale_x: (float nw /. float old_width) ~scale_y: (float nh /. float old_height) 
      ~interp: `HYPER old;
    pixbuf'
  end else old

let create_pixbuf (_id : int) image filters =
  let pixbuf = Imagegdk.to_pixbuf image in
  let filters = sort_filters filters in
  let filter_pixbuf pixbuf = function
    | [] -> pixbuf
    | `SIZE (w, h, cond) :: _fs -> 
	(* original pixbuf will be GC'ed automatically *)
	resize w h cond pixbuf
  in
  filter_pixbuf pixbuf filters


let current = ref None

let rec display id image filters =
  let start_waiting () =
    match !waiting with
    | Some (id, image, filters) ->
	prerr_endline "aborted!";
	waiting := None;
	display id image filters
    | None -> () 
  in

  if !working <> None then begin
    (* we store it at waiting *)
    (* hoping working process may find it and move to it *)
    prerr_endline "try to abort";
    waiting := Some (id, image, filters);
  end else begin
    try
      working := Some (id, image, filters);
      (* XIMAGE let ximage = ximage_of_image id image filters in *)
      let pixbuf = create_pixbuf id image#coerce filters in
      current := Some (id, image);
      current_filters := filters;
      display_pixbuf pixbuf;
      working := None;
      start_waiting ()
    with
    | Exit -> (* abort! *)
        working := None;
        start_waiting ()
  end

let redisplay new_filters =
  match !current with
  | Some (id, image) -> display id image new_filters
  | None -> ()

let () = forward_redisplay := redisplay
