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

let pauses =
  Options.flag true "-nopauses"
  "  switch pauses off,\
  \n\t (the default is to wait for specified pauses).";;

let events =
  Options.add
  "-pushkeys"
    (Arg.String (String.iter Misc.push_char_event))
  "<string>: push string chars as events\n\t";;


let postsyncing =
  Options.flag true "--nopostsyncing"
  "  do not automatically redraw when syncing if needed \n\t";;

let center_on_cursor =
  Options.flag false "--center-on-cursor"
  "  redraw so that cursor is displayed\n\t";;
  

let scroll_fast =
  Options.flag false "--scrollfast"
  "  scroll to next page instead of page_down \n\t";;

let without_pauses f x =
  let p = !pauses in
  try pauses := false; let v = f x in pauses := p; v
  with x -> pauses := p; raise x;;

let fullwidth =
  Options.flag false "-fullwidth"
  "  adjust size to full width,\
   \n\t (the default is not to adjust to full width).";;

let bounding_box =
  Options.flag false "-bbox"
  "  show the bounding box,\
  \n\t (the default is to hide the bounding box).";;

let autoswitch =
  Options.flag false "-autoswitch"
  "  switch back to master when refreshed";;
let toggle_autoswitch () = autoswitch := not !autoswitch;;

let start_page = ref 0;;
Options.add
  "-page"
  (Arg.Int (fun i -> start_page := i))
  "<num>: start preview to page number <num>,\
  \n\t (the default starting page is page number 0).";;

let starting_page npages =
 if !start_page > 0 then min !start_page npages - 1 else 0;;

let start_html = ref None;;
Options.add
  "-html"
  (Arg.String (fun s -> start_html := Some s))
  "<anchor>: ask Active-DVI to start at HTML reference named <anchor>.";;
let debug_pages =
  Options.debug
    "--debug-pages"
    "  debug page motion.";;

let browser = ref "firefox";;
Options.add
  "-browser"
  (Arg.String (fun s -> browser := s))
  (Printf.sprintf
     "<com>: set the HTML files viewer command to <com>,\
      \n\t (the default is %S)."
     !browser);;

let pager = ref "xterm -e less";;
Options.add
  "-pager"
  (Arg.String (fun s -> pager := s))
  (Printf.sprintf
    "<com>: set the text files viewer command to <com>,\
     \n\t (the default is %S)."
    !pager);;

let pdf_viewer = ref "xpdf";;
Options.add
  "-pdf-viewer"
  (Arg.String (fun s -> pdf_viewer := s))
  (Printf.sprintf
    "<com>: set the PDF files viewer command to <com>,\
     \n\t (the default is %S)."
    !pdf_viewer);;

let ps_viewer = ref "gv";;
Options.add
  "-ps-viewer"
  (Arg.String (fun s -> ps_viewer := s))
  (Printf.sprintf
    "<com>: set the PostScript files viewer command to <com>,\
    \n\t (the default is %S)."
    !ps_viewer);;

let image_viewer = ref "xv";;
Options.add
  "-image-viewer"
  (Arg.String (fun s -> image_viewer := s))
  (Printf.sprintf
    "<com>: set the image files viewer command to <com>,\
    \n\t (the default is %S)."
    !image_viewer);;

let film_viewer = ref "mplayer";;
Options.add
  "-film-viewer"
  (Arg.String (fun s -> film_viewer := s))
  (Printf.sprintf
    "<com>: set the movie files player command to <com>,\
    \n\t (the default is %S)."
    !film_viewer);;

let click_turn_page =
  Options.flag false
    "-click-turn"
    "  turn pages with mouse clicks (see the doc).";;

let page_stack_to_string page stack =
  let stack = String.concat " " (List.map string_of_int stack) in
  Printf.sprintf "Page no: %d Page_stack: %s" page stack;;

let scale_step = ref (sqrt (sqrt (sqrt 2.)));;

let set_scale x =
  if x > 1.0 && x <= 2. then scale_step := x else
  Misc.warning
    (Printf.sprintf "out of bounds scale_step %f ignored" x);;
Options.add
  "-scalestep"
  (Arg.Float set_scale)
  "<float>: set the step used by '<' and '>' for scaling the page,\
  \n\t (the default is \"sqrt (sqrt (sqrt 2.0))\").";;

let autoresize = ref true;;
Options.add
  "-noautoresize"
  (Arg.Clear autoresize)
  "  prevents scaling the page from resizing the window,\
  \n\t (automatically set when the geometry is specified).";;

let autoscale = ref true;;
Options.add
  "-noautoscale"
  (Arg.Clear autoscale)
  "  prevents resizing the window from scaling the page,\
  \n\t (automatically set when the geometry is specified).";;

let dpi_resolution = ref 72.27;;
let set_dpi_resolution r = dpi_resolution := max r 72.27;;

Options.add
  "-resolution"
  (Arg.Float set_dpi_resolution)
  "<float>: set the dpi resolution of the screen,\
  \n\t (the default (and minimum value) is 72.27))).";;

module Symbol = Grdev.Symbol;;

open Dimension;;

exception Error of string;;

(*** View attributes ***)
(* Things we can set before initialization *)

type attr = {
    mutable geom : Ageometry.t;
    mutable crop : bool;
    mutable hmargin : dimen;
    mutable vmargin : dimen
  };;

(*** The view state ***)
type mode =
   | Selection
   | Control;;
type toc =
   | Page of int
   | Thumbnails of int * (int * Graphics.image) array;;
type command = Position | Other;;
type duplex = Alone | Master of state | Client of state
and state = {
    (* DVI attributes *)
    filename : string;
    mutable duplex : duplex;
    mutable dvi : Cdvi.t;
    mutable cdvi : Driver.cooked_dvi;
    mutable num_pages : int;
    (* Page layout *)
    mutable base_dpi : float;
    mutable dvi_width : int;   (* in pixels *)
    mutable dvi_height : int;  (* in pixels *)
    (* Window size *)
    mutable size_x : int;
    mutable size_y : int;
    (* Current parameters *)
    mutable orig_x : int;
    mutable orig_y : int;
    mutable ratio : float;
    (* the current page number *)
    mutable page_number : int;
    (* the history of pages used by forward and backward.
       negative numbers are used to mark pages used by pop *)
    mutable page_stack : int list;
    mutable page_marks : int list;
    mutable exchange_page : int;
    mutable last_command : command;
    mutable last_modified : int * int * float;
    mutable button : (int * int) option;
    mutable full_screen : (int * int * int * int * (int * int)) option;
    mutable in_full_screen : bool;

    mutable pause_number : int;
    mutable last_pause_number : int;

    (* Attributes for Embedded postscript *)

    (* True when page was not completed: may need to redraw *)
    mutable aborted : bool;
    (* True when hrefs have not been processed *)
    mutable frozen : bool;
    (* Numeric value for keyboard interaction *)
    mutable num : int;
    (* Next numeric value *)
    mutable next_num : int;
    (* Control the action of the mouse *)
    mutable mode : mode;
    (* Some of f when on a pause *)
    mutable cont : (unit -> bool) option;
    mutable toc : toc array option;
    synchronize : bool;
};;

exception Duplex of (state -> unit) * state;;

let set_page_number st n =
 Userfile.save_page_number n;
 Userfile.save_page_timing n;
 Thumbnails.save n;
 st.page_number <- n;;

(*** Setting the geometry ***)

(*****************************************************************************

  The `st' record contains the geometry for advi drawing, not the real
  geometry, which is stored in the attr record.
  Fields st.size_x st.size_y orig_x orig_y of the window are in
  advi coordinates, i.e.

       ----->
      |
      |
      \/

  Fields st.orig_x et st.orig_y indicates the advi "margins".
  Fields st.size_x et st.size_y the advi dimensions according to the
  resolution:

   - these are not the dimension of the window !Graphics.size_x() and
  Graphics.size_y() unless the window is self-adjusted to the advi dimensions;

   - they can be lower in one dimension when the window is forced a
  different shape;

   - they can be both higher when the window size is fixed and when drawing at
  a higher scale.

  We always have st.orig_x + st.width + st.orig_x = st.size_x
  and the same for y coordinates

*****************************************************************************)

(*** Setting other parameters ***)

let attr =
  { geom = {
      Ageometry.width = 0;
      Ageometry.height = 0;
      Ageometry.xoffset = Ageometry.No_offset;
      Ageometry.yoffset = Ageometry.No_offset;
    };
    crop = false;
    hmargin = Px 0;
    vmargin = Px 0
  };;

let set_autoresize b = autoresize := b
let set_autoscale b = autoscale := b
let set_geometry g = attr.geom <- Ageometry.parse g;;

let set_crop b = attr.crop <- b;;

let set_hmargin d = attr.hmargin <- normalize d;;

let set_vmargin d = attr.vmargin <- normalize d;;

(*** Initialization ***)
let init_geometry all st =
  let dvi = st.dvi in
  let dvi_res = !dpi_resolution
  and mag = float dvi.Cdvi.preamble.Dvicommands.pre_mag /. 1000.0 in
  let w_sp = dvi.Cdvi.postamble.Dvicommands.post_width
  and h_sp = dvi.Cdvi.postamble.Dvicommands.post_height in
  let w_in = mag *. ldexp (float w_sp /. dvi_res) (-16)
  and h_in = mag *. ldexp (float h_sp /. dvi_res) (-16) in

  let wdpi =
    match attr.hmargin with
    | Px n -> float (attr.geom.Ageometry.width - 2 * n) /. w_in
    | In f -> float attr.geom.Ageometry.width /. (w_in +. 2.0 *. f)
    | _ -> assert false
  and hdpi =
    match attr.vmargin with
    | Px n -> float (attr.geom.Ageometry.height - 2 * n) /. h_in
    | In f -> float attr.geom.Ageometry.height /. (h_in +. 2.0 *. f)
    | _ -> assert false in
  let base_dpi = min wdpi hdpi in
  let real_width = Misc.round (base_dpi *. w_in *. st.ratio)
  and real_height = Misc.round (base_dpi *. h_in *. st.ratio) in
  let fwidth = base_dpi *. w_in
  and fheight = base_dpi *. h_in in
  let (size_x, size_y) =
    if attr.crop then begin
      let sx = match attr.hmargin with
      | Px n -> Misc.round (fwidth +. 2.0 *. float_of_int n)
      | In f -> Misc.round (fwidth +. 2.0 *. base_dpi *. f)
      | _ -> assert false
      and sy = match attr.vmargin with
      | Px n -> Misc.round (fheight +. 2.0 *. float_of_int n)
      | In f -> Misc.round (fheight +. 2.0 *. base_dpi *. f)
      | _ -> assert false in
      (min attr.geom.Ageometry.width sx, min attr.geom.Ageometry.height sy)
    end else
      (attr.geom.Ageometry.width, attr.geom.Ageometry.height) in
  if all then
    begin
      let orig_x = Misc.round ((float size_x -. fwidth) *. 0.5)
      and orig_y = Misc.round ((float size_y -. fheight) *. 0.5) in
      st.base_dpi <- base_dpi;
      st.size_x <- size_x;
      st.size_y <- size_y;
      st.orig_x <- orig_x;
      st.orig_y <- orig_y;
    end;
  st.dvi_width <- real_width;
  st.dvi_height <- real_height;
  st.toc <- None;
;;

let init master filename =
  let dvi =
    try Cdvi.load filename
    with
    | Sys_error _ -> raise (Error (Printf.sprintf "cannot open `%s'" filename))
    | Dvi.Error s -> raise (Error (Printf.sprintf "%s: (Dvi) %s" filename s))
    | e ->
        raise (Error
                 (Printf.sprintf "error while loading `%s': %s"
                    filename (Printexc.to_string e))) in
  let cdvi = Driver.cook_dvi dvi in
  let int = 0 in
  let float = 0. in
  let last_modified =
    try 
      let stat =  Unix.stat filename in
      stat.Unix.st_dev, stat.Unix.st_ino, stat.Unix.st_mtime
    with _ -> 0, 0, 0.0 in
  Gs.init_do_ps ();
  let st =
    let npages = Array.length dvi.Cdvi.pages in
    { filename = filename;
      duplex = Alone;
      dvi = dvi;
      cdvi = cdvi;
      num_pages =  npages;
      base_dpi = float;
      dvi_width = int;
      dvi_height = int;
      size_x = int;
      size_y = int;
      orig_x = int;
      orig_y = int;
      ratio = 1.0;
      page_stack = [];
      page_marks = [];
      exchange_page = 0;
      page_number = starting_page npages;
      last_modified = last_modified;
      last_command = Other;
      button = None;
      full_screen = None;
      in_full_screen = false;

      pause_number = 0;
      last_pause_number = 0;

      frozen = true;
      aborted = false;
      cont = None;
      mode = Control;
      num = 0;
      next_num = 0;
      toc = None;
      synchronize = true;
    } in
  init_geometry true st;
  if master then begin
    attr.geom.Ageometry.width <- st.size_x;
    attr.geom.Ageometry.height <- st.size_y;
  end;
  st;;

let compatible st st' =
  (*
  Printf.eprintf
    "x=%d/%d, y=%d/%d, width=%d/%d height=%d/%d dpi=%f/%f\n%!"
    st.size_x  st'.size_x
    st.size_y  st'.size_y
    st.dvi_width  st'.dvi_width
    st.dvi_height  st'.dvi_height
    st.base_dpi  st'.base_dpi;
  st.base_dpi = st'.base_dpi &&
  st.dvi_width = st'.dvi_width &&
  st.dvi_height = st'.dvi_height && *)
(*
  st.size_x = st'.size_x &&
  st.size_y = st'.size_y
*)
 true;;

let set_bbox st =
  Grdev.set_bbox (Some (st.orig_x, st.orig_y, st.dvi_width, st.dvi_height));;

let update_dvi_size all ?dx ?dy st =
  init_geometry all st;
  begin match dx with None -> () | Some z -> st.orig_x <- z end;
  begin match dy with None -> () | Some z -> st.orig_y <- z end;
  set_bbox st;;
  (*
  match st.duplex with
  | Master st' | Client st' ->
      st'.orig_x <- st.orig_x;
      st'.orig_y <- st.orig_y;
      set_bbox st';
  | Alone -> () *)

(* Reloading *)

let reload_time st =
  try 
    let stat =  Unix.stat st.filename in
    stat.Unix.st_dev, stat.Unix.st_ino, stat.Unix.st_mtime
  with _ -> st.last_modified;;

let changed st =
  reload_time st <>  st.last_modified;;

let rec clear_page_stack max stack =
  let pages = Array.make max false in
  let rec clear = function
    | p :: stack ->
        let s = clear stack in
        if p = -1 then s else
        let pa = if p < 0 then -2 - p else p in
        if pa < max && not pages.(pa) then begin
          pages.(pa) <- true;
          p :: s end
        else s
    | _ -> [] in
  clear stack;;

(* Incremental drawing *)
let synchronize st =
  if st.synchronize then Grdev.synchronize ();;

let goto_next_pause n st =
  let rec aux n st =
    if n > 0 then
      begin match st.cont with
      | None -> ()
      | Some f ->
          st.cont <- None;
          try
            begin try
              while f () do () done;
              st.pause_number <- st.pause_number + 1;
              st.last_pause_number <- st.last_pause_number + 1;
            with
            | Driver.Wait sec ->
                ignore (Grdev.sleep sec);
                st.cont <- Some f;
                aux n st
            | Driver.Pause ->
                st.pause_number <- st.pause_number + 1;
                st.last_pause_number <- st.last_pause_number + 1;
                st.cont <- Some f;
                aux (pred n) st
            end;
          with Grdev.Stop -> st.aborted <- true;
      end in
  aux n st;
  synchronize st;
  Busy.set (if st.cont = None then Busy.Free else Busy.Pause);;

let draw_bounding_box st =
  Grdev.set_color 0xcccccc;
  Grdev.fill_rect st.orig_x st.orig_y st.dvi_width 1;
  Grdev.fill_rect st.orig_x st.orig_y 1 st.dvi_height;
  Grdev.fill_rect st.orig_x (st.orig_y + st.dvi_height) st.dvi_width 1;
  Grdev.fill_rect (st.orig_x + st.dvi_width) st.orig_y 1 st.dvi_height;;

(* Input : a point in window coordinates, relative to the lower-left corner.
   Output : a point in document coordinates, relative to the upper-right corner.
   The output depends on the ratio st.ratio. *)
let document_xy st x y =
  (* x and y are relative to the lower-left corner. *)
  let y = st.size_y - y in
  x, y;;

let position st x y =
  match Symbol.lines x y with
  | Some (s, line, bound, left, before, after, right, file) ->
      let line = max 0 line in
      let bound = max 0 bound in
      let file = match file with Some f ->  f | _ -> "" in
      Printf.printf "#line %d, %d <<%s<<%s>><<%s>>%s>> %s\n"
        line bound left before after right file;
      flush stdout
  | None -> ();;

(* User has selected a region with the mouse.
   We dump the corresponding characters. *)
let selection s = Grdev.cut s;;

let get_size_in_pix st = function
  | Px n -> n
  | In f -> Misc.round (st.base_dpi *. f)
  | _ -> assert false;;

let vmargin_size st = get_size_in_pix st attr.vmargin;;
let hmargin_size st = get_size_in_pix st attr.hmargin;;

(* The next four functions returns the position that correspond to the top,
   the bottom, the left, and right of the page. *)
let top_of_page = vmargin_size;;

let bottom_of_page st =
  attr.geom.Ageometry.height - st.dvi_height - vmargin_size st;;

let left_of_page = hmargin_size;;

let right_of_page st =
  attr.geom.Ageometry.width - st.dvi_width - hmargin_size st;;

(* The two following functions move the displayed part of the page while
   staying inside the margins. *)
let move_within_margins_y st movey =
  let tmp_orig_y = st.orig_y + movey in
  let new_orig_y =
    let vmargin_size = vmargin_size st in
    if movey < 0 then begin
      if tmp_orig_y + st.dvi_height + vmargin_size < attr.geom.Ageometry.height
      then attr.geom.Ageometry.height - st.dvi_height - vmargin_size
      else tmp_orig_y
    end else begin
      if tmp_orig_y - vmargin_size > 0
      then vmargin_size
      else tmp_orig_y
    end in
  if st.orig_y <> new_orig_y then Some new_orig_y else None;;

let move_within_margins_x st movex =
  let tmp_orig_x = st.orig_x + movex in
  let new_orig_x =
    let hmargin_size = hmargin_size st in
    if movex < 0 then begin
      if tmp_orig_x + st.dvi_width + hmargin_size < attr.geom.Ageometry.width
      then attr.geom.Ageometry.width - st.dvi_width - hmargin_size
      else tmp_orig_x
    end else begin
      if tmp_orig_x - hmargin_size > 0
      then hmargin_size
      else tmp_orig_x
    end in
  if st.orig_x <> new_orig_x then Some new_orig_x else None;;

let make_visible st x y =
  let hskip = 15 in
  let vskip = 15 in
  let movex =
    let move_within_margins_x st dx = move_within_margins_x st dx in
    let posx = x + st.orig_x in
    if posx < 0 then
      (* move far left to make x visible *)
      (* move_within_margins_x st (attr.geom.Ageometry.width - 10) *)
      move_within_margins_x st (attr.geom.Ageometry.width - hskip - posx)
    else if posx > attr.geom.Ageometry.width then
      (* move far right to make x visible *)
      (* move_within_margins_x st (10 - attr.geom.Ageometry.width) *)
      move_within_margins_x st (hskip - posx)
    else None in
  let movey = 
    let posy = st.orig_y + y in
    if posy < 0 then
      move_within_margins_y st (attr.geom.Ageometry.height - vskip - posy)
    else if posy > attr.geom.Ageometry.height then
      move_within_margins_y st (vskip - posy)
    else None  in
  let r = false in
  let r = match movex with | None -> r | Some dx -> st.orig_x <- dx; true in
  let r = match movey with | None -> r | Some dy -> st.orig_y <- dy; true in
  if r then set_bbox st;
  r

let rec redraw ?trans ?chst st =
  (* Draws until the current pause_number or page end. *)
  (* The pauses and waits that appear before are ignored. *)
  Busy.set Busy.Busy;
  st.cont <- None;
  st.aborted <- false;
  begin
    try
      Grdev.continue ();
      Driver.clear_symbols ();
      if !bounding_box then draw_bounding_box st;
      let f =
        Driver.render_step st.cdvi st.page_number ?trans ?chst
          (st.base_dpi *. st.ratio) st.orig_x st.orig_y in
      if !pauses then begin
        let current_pause = ref 0 in
        try
          while
            try f () with
            | Driver.Wait sec ->
                if !current_pause = st.pause_number then
                  ignore (Grdev.sleep sec);
                true
            | Driver.Pause ->
                if !current_pause = st.pause_number
                then raise Driver.Pause
                else begin
                  incr current_pause; 
                  st.last_pause_number <- !current_pause;
                  true end
          do () done;
          if !current_pause < st.pause_number
          then st.pause_number <- !current_pause
        with
        | Driver.Pause -> st.cont <- Some f
      end else begin
        Misc.debug_endline ("Pauses are disabled: overriding transitions!");
        Transimpl.sleep := (fun _ -> true); (* always breaks *)
        while
          try f () with
          | Driver.Wait _ | Driver.Pause -> true
        do () done
      end
    with
    | Grdev.Stop -> st.aborted <- true
  end;
  synchronize st;
  Busy.set (if st.cont = None then Busy.Free else Busy.Pause);
  if not st.aborted &&  !postsyncing && not (Grdev.syncing()) then
    begin
      match st.dvi.Cdvi.pages.(st.page_number).Cdvi.page_status with
      | Cdvi.Known { Cdvi.hasps = true } -> 
          Grdev.with_syncing (redraw ?trans ?chst) st
      | _ -> ()
    end;
  Misc.debug_stop "Page has been drawn\n";;

let goto_previous_pause n st =
  if n > 0 then begin
    st.pause_number <- max 0 (st.pause_number - n);
    redraw st
  end;;

let thumbnail_limit = ref 5;;

Options.add
  "-thumbnail-scale"
  (Arg.Int (fun i -> thumbnail_limit := i))
  (Printf.sprintf
     "<int>: set the number of thumbnails per line\
     \n\t and column to <int>,\
     \n\t (the default number is %d)." !thumbnail_limit);;

let xrefs st =
  if st.frozen then
    begin
      Driver.scan_special_pages st.cdvi max_int;
      st.frozen <- false;
    end;
  st.dvi.Cdvi.xrefs;;

let make_thumbnails st =
  let xnails =
    Hashtbl.fold
      (fun x p all -> if Misc.has_prefix "/page." x then p :: all else all)
      (xrefs st)
      [] in
  let page_nails =
    if xnails = [] then Array.init st.num_pages (fun p -> p) else
    let ucons x l =
      match l with
      | y :: _ when x = y -> l
      | _ -> x :: l in
    let rec unique = function
      | [] -> []
      | x :: l -> ucons x (unique l) in
    Array.of_list (unique (List.sort compare xnails)) in
  let num_nails = Array.length page_nails in
  let r_fit = int_of_float (ceil (sqrt (float_of_int num_nails))) in
  let r = min r_fit !thumbnail_limit in
  let ist =
    { st with
      size_x = st.size_x / r;
      size_y = st.size_y / r;
      synchronize = false;
      orig_x = st.orig_x / r;
      orig_y = st.orig_y / r;
      dvi_width = st.dvi_width / r;
      base_dpi = st.base_dpi /. float r;
      dvi_height = st.dvi_height / r;
    } in
  let size_x = Graphics.size_x () in
  let size_y = Graphics.size_y () in
  let dx = size_x / r
  and dy = size_y / r in
  let all =
    Driver.with_active false
      (Array.map
         (fun p ->
           let chgvp s =
             {s with
              Cdvi.bkgd_prefs =
               {s.Cdvi.bkgd_prefs with
                Grdev.bgviewport =
                  Some {
                   Grdev.vx = 0;
                   Grdev.vy = size_y - dy;
                   Grdev.vw = dx;
                   Grdev.vh = dy;
                  }
               }
             } in
           without_pauses
             (redraw ?chst:(Some chgvp)) {ist with page_number = p};
           (* Interrupt thumbnail computation in case of user interaction. *)
           begin try Grdev.continue() with
           | Grdev.Stop ->
               let gray = Graphics.rgb 200 200 200 in
               Grdev.with_color gray
                 (Graphics.fill_rect 0 (size_y - dy) (dx - 1)) (dy - 1)
           end;
           p, Graphics.get_image 0 (size_y - dy) dx dy;
         ))
         page_nails in
  let rolls = (Array.length all + r * r - 1) / r / r in
  let split =
    Array.init rolls
      (fun roll ->
        let first = roll * r * r in
        Thumbnails
          (r, Array.sub all first (min (r * r) (Array.length all - first)))) in
  st.toc <- Some split;;

let make_toc st =
  try
    let refs = xrefs st in
    let first = Hashtbl.find refs "advitoc.first" in
    let last = Hashtbl.find refs "advitoc.last" in
    st.toc <- Some (Array.init (last - first + 1) (fun p -> Page (p + first)))
  with
  | Not_found -> ();;

let find_xref_here tag st =
  try
    let p = int_of_string (Misc.get_suffix "/page." tag) in
    if p > 0 && p <= st.num_pages then p - 1 else raise Not_found
  with Misc.Match ->
    Hashtbl.find (xrefs st) tag;;

let scan_find_anchor_position st page html =
  try
    let  (x, y, _z, _w, _h) =
      Driver.scan_find_anchor_position st.cdvi (st.base_dpi *. st.ratio)
        page html in
    Some (x, y)
  with Not_found -> None 

let page_start default st =
  match !start_html with
  | None -> default, None
  | Some html ->
      Driver.scan_special_pages st.cdvi max_int;
      try
        let page = find_xref_here html st in
        let pos = scan_find_anchor_position st page html in
        page, pos
      with Not_found -> default, None;;


(* foreground if drawing is needed after reloading *)
let rec reload foreground st =
  try
    Grdev.clear_usr1 ();
    st.last_modified <- reload_time st;
    let dvi = Cdvi.load st.filename in
    let cdvi = Driver.cook_dvi dvi in
    let npages =  Array.length dvi.Cdvi.pages in
    st.dvi <- dvi;
    st.cdvi <- cdvi;
    st.num_pages <- npages;
    st.toc <- None;
    st.page_stack <- clear_page_stack npages st.page_stack;
    let npage, pos = page_start (min st.page_number (st.num_pages - 1)) st in
    if npage <> st.page_number then
      begin
        st.pause_number <- 0;
        st.exchange_page <- st.page_number;
      end;
    set_page_number st npage;
    begin if !center_on_cursor then
      match pos with
      | Some (x, y) -> ignore (make_visible st x y)
      | None -> ()
    end;
    st.frozen <- true;
    st.aborted <- true;
    begin match st.duplex with
    | Client st' when not (compatible st st') ->
        Misc.warning
          (Printf.sprintf
             "Dropping master %s (incompatible with client %s)"
             st.filename st'.filename);
        st'.duplex <- Alone;
        raise (if foreground then Duplex (redraw, st') else Not_found)
(*
    | Master st' when st'.last_command = Position ->
        update_dvi_size false st;
        Gs.init_do_ps ();
        raise (Duplex (redraw, st'))
*)
    | _ ->
        update_dvi_size false st;
        Gs.init_do_ps ();
        if foreground then redraw ?trans:(Some Transitions.DirTop) st
    end;
    ()
  with x ->
    Misc.warning
      (Printf.sprintf "exception while reloading %s" (Printexc.to_string x));
    st.cont <- None;;

let find_xref_master tag default st =
  try
    match st.duplex with
    | Client _ | Alone -> default
    | Master st' ->
        if changed st' then reload false st';
        st'.page_number <- Hashtbl.find (xrefs st') tag;
        (* so as to pop to current view automatically *)
        st'.page_stack <- (-1) :: st'.page_stack;
        raise (Duplex (redraw, st'))
  with Not_found -> default;;

let find_xref tag default st =
  try find_xref_here tag st
  with Not_found -> find_xref_master tag default st;;

let redisplay st =
  st.pause_number <- 0;
  Grdev.toggle_syncing ();
  redraw st;;

let resyncing st =
  st.pause_number <- 0;
  Grdev.with_syncing redraw st;;

let show_thumbnails st r page =
  let size_x = Graphics.size_x () in
  let size_y = Graphics.size_y () in
  let dx = size_x / r
  and dy = size_y / r in
  Array.iteri
    (fun p' (p, img) ->
       let x = size_x * (p' mod r) / r in
       let y = size_y * (p' / r) / r in
       Graphics.draw_image img x (size_y - y -dy);
       Grdev.H.area
         (Grdev.H.Href ("#/page." ^ string_of_int (p + 1))) x
         (size_y - y - dy) dx dy)
    page;
  (* To force the page under thumbnails to be redrawn *)
  st.aborted <- true;;

let rec show_toc st =
  if st.toc = None then make_toc st;
  Grdev.clear_dev();
  Driver.clear_symbols();
  let show st rolls st' = 
    (* we are in st, but the rolls are in st', useful in duplex mode *)
    let n = Array.length rolls in
    if st.num = n then redraw st
    else
      let roll = st.num mod n in
      st.next_num <- succ st.num;
      begin match rolls.(roll) with
      | Page p -> redraw
            { st' with
              page_number = p;
              orig_x = 0;
              orig_y = top_of_page st;
            }
      | Thumbnails (r, page) -> show_thumbnails st' r page
      end;
      synchronize st;
      st.aborted <- true in
  match st.toc with
  | None ->
      begin match st.duplex with
      | Master ({ duplex = Client _ (* to prevent loop *) } as st') ->
          (* normally, there is only one master, but just in case something
             is wrong, we certainly prefer not to do anything than to
             go in an infinite loop *)
          if changed st' then reload false st';
          if st'.toc = None then make_toc st';
          begin match st'.toc with
          | None -> ()
          | Some rolls -> show st rolls st'
          end
      | _ -> ()
      end
  | Some rolls ->
      show st rolls st;;

exception Link;;

(* View a link: call the appropriate viewer helping command. *)
let exec_xref link =
  let call command arg =
    Grdev.wait_button_up ();
    ignore (Launch.fork_process (Printf.sprintf "%s %s" command arg)) in
  let find_protocole link =
    try Misc.string_prefix ':' link with
    | Not_found -> "" in
  match find_protocole link with
  | "file:" ->
      if Misc.has_prefix "file://" link then call !browser link else
      begin try
        let fname, arguments =
          match Misc.split_string (Misc.get_suffix "file:" link)
                  (function '#' -> true | _ -> false) 0 with
          | [ fname; tag ] -> fname, ["-html"; tag ]
          | [ fname ] -> fname, []
          | _ -> Misc.warning ("Invalid link " ^ link); raise Link in
        if not (Sys.file_exists fname) then
          Misc.warning
            (Printf.sprintf
               "File %s is non-existent or not readable" fname) else
        match Misc.filename_extension fname with
        | ".dvi" ->
            let command = String.concat " " (Sys.argv.(0) :: arguments) in
            call command fname
        | ".html" | ".shtml" | ".htm" | ".shtm" ->
            call !browser link
        | ".pdf" -> call !pdf_viewer fname
        | ".ps" | ".eps" -> call !ps_viewer fname
        | ".bmp" | ".gif" | ".png" | ".jpg" | ".jpeg"
        | ".pbm" | ".pgm" | ".pnm" | ".tiff" | ".xpm" ->
            call !image_viewer fname
        | ".mpg" -> call !film_viewer fname
        (* In any other cases we call the pager program. *)
        | ".txt" | ".tex" | ".ftex" | ".sty" | ".pic"
        | ".ml" | ".scm" | ".c" | ".el" ->
            call !pager fname
        | _ ->
            Misc.warning
              (Printf.sprintf "Don't know what to do with link %s" link)
      with
      | Misc.Match -> assert false
      | Link -> () end
  | "http:" | "https:" | "ftp:" | "telnet:" ->
      call !browser link
  | _ ->
      Misc.warning (Printf.sprintf "Don't know what to do with link %s" link);;

let pop_duplex st =
  match st.duplex with
  | Client st' -> raise (Duplex (redraw, st'))
  | Alone | Master _ -> ();;

(* Go to the begining of the page. *)
let set_page_pause_num n pause_num st =
  (* now controlled by pop_page *)
  (*   if n < 0 (* then n = -1 *) then pop_duplex st else *)
  let new_page_number = max 0 (min n (st.num_pages - 1)) in
  let trans =
    if new_page_number = succ st.page_number then
      Some Transitions.DirRight else
    if new_page_number = pred st.page_number then
      Some Transitions.DirLeft else
    if new_page_number = st.page_number then Some Transitions.DirTop
    else None in
  let page_is_new = st.page_number <> new_page_number in
  (* We have to redraw if we have to go to another page. *)
  let have_to_redraw = page_is_new || st.aborted in
  if page_is_new then st.exchange_page <- st.page_number;
  if have_to_redraw then set_page_number st new_page_number;
  (* We also have to redraw if we have to go to another pause. *)
  let have_to_redraw = st.pause_number <> pause_num || have_to_redraw in
  st.pause_number <- pause_num;
  if have_to_redraw then redraw ?trans st;;

let goto_page n st = set_page_pause_num n 0 st;;

(* Find the coocked DVI page that corresponds to a position in a source file. *)
let find_page_before_position (pos, filename as location) st =
  (* To find all matching pages *)
  let rec find last all p =
    if p < 0 then all else
    match st.dvi.Cdvi.pages.(p).Cdvi.line with
    | Some (pos', f) -> 
        if (pos' <= pos) && (pos < last || last < pos') &&  filename = f 
        then find pos' (min (p+1) max_int :: all) (pred p)
        else find pos' all (pred p)
    | _ -> find last all (pred p) in
  let matches = find max_int [] (st.num_pages - 1) in
  (* We should then desambiguate, but we do not. Return the first
     page that match always would never jump to appendices.  *)
  let refined =
    List.map
      (fun p -> p, Driver.scan_find_location st.cdvi p location)
      matches in
  match
    List.sort (fun (p1,l1) (p2, l2) -> compare (pos - l1) (pos - l2)) refined
  with
  | [] -> raise Not_found
  | (p, _)::_ -> p

let start_anchor = "Start-Document"

let duplex_switch sync st =
  let find_sync_page master client =
    match
      (* master.dvi.Cdvi.pages.(master.page_number).Cdvi.line *)
      Driver.scan_find_anchor_location master.cdvi 
        (Hashtbl.find (xrefs master) start_anchor) start_anchor 
    with
    | Some (pos, _) -> pos
    | None -> raise Not_found in

  match st.duplex with
  | Alone -> ()
  | (Client st' | Master st') when not sync -> raise (Duplex (redraw, st'))
  | Client master ->
      (try
        let page  = find_sync_page master st in
        begin
          (* should only be done if not all window shown *)
          match scan_find_anchor_position st page start_anchor 
          with
          | Some (x, y) -> ignore (make_visible st x y); 
          | None -> ()
        end;
      with Not_found -> ())
  | Master client ->
      try
        if changed client then reload false client;
        (* should rather find the page with the linenumber *)
        begin try
          let q = find_sync_page st client in
          client.page_number <- q;
          client.page_stack <- (-1) :: client.page_stack;
        with Not_found -> ()
        end;
        raise (Duplex (redraw, client))
      with Not_found -> ();;

let push_stack b n st =
  match st.page_stack with
  | p :: rest when p = n -> if b then st.page_stack <- ( -2 - n ) :: rest
  | p :: rest when p = -2 - n -> ()
  | all -> st.page_stack <- (if b then -2 - n else n) :: all;;

let push_page b n st =
  let new_page_number = max 0 (min n (st.num_pages - 1)) in
  if st.page_number <> new_page_number || st.aborted then
    begin
      push_stack b st.page_number st;
      goto_page n st
    end;;

let pop_page b n st =
  assert
    (debug_pages
       (Printf.sprintf "%s\n => popping %s page %d "
          (page_stack_to_string st.page_number st.page_stack)
          (string_of_bool b)
          n);
     true);
  let rec pop n return_page return_stack stack =
    match n, stack with
    | n, _ when n <= 0 ->
        return_page, return_stack
    | _, [] ->
        return_page, return_stack
    | n, h :: t ->
        if b && h >= 0
        then pop n return_page return_stack t
        else pop (pred n) h t t in
  let npage, stack = pop n st.page_number st.page_stack st.page_stack in
  st.page_stack <- stack;
  let new_page = if npage > 0 then npage else -2 - npage in
  if new_page = st.page_number then redraw st else
  if new_page < 0 (* necessarily -1 *) then pop_duplex st
  else goto_page new_page st;;

let mark_page st =
  let marks =
    if List.length st.page_marks > 9
    then List.rev (List.tl (List.rev  st.page_marks))
    else st.page_marks in
  st.page_marks <- st.page_number :: marks;;

let goto_mark n st =
  try goto_page (List.nth st.page_marks n) st with
  | Failure _
  | Invalid_argument _ -> ();;

let previous_slice st =
  print_string "#line 0, 0 <<<<>><<>>Next-Slice>> ";
  print_newline ();;

let next_slice st =
  print_string "#line 0, 0 <<Previous-Slice<<>><<>>>> ";
  print_newline ();;

(* goto hyperref link *)
let goto_href link st =
  (*prerr_endline ("Goto reference " ^ link);*)
  let p =
    if Misc.has_prefix "#" link then
      let tag = Misc.get_suffix "#" link in
      (*prerr_endline ("Find xref " ^ tag);*)
      find_xref tag st.page_number st
    else begin
      exec_xref link;
      st.page_number
    end in
  push_page true p st;
  Grdev.H.flashlight (Grdev.H.Name link);;

(* Go to hyperpage n if possible or page n otherwise *)
let goto_pageref n st =
  let tag = Printf.sprintf "#page.%d" n in
  let alt = if st.num > 0 then st.num - 1 else st.num_pages in
  let p = find_xref tag alt st in
  push_page true p st;;

let goto_next_page st =
  if st.page_number <> st.num_pages - 1 then goto_page (st.page_number + 1) st;;

let resize st ?dx ?dy x y =
  attr.geom <-
    { Ageometry.width = x;
      Ageometry.height = y;
      Ageometry.xoffset = Ageometry.No_offset;
      Ageometry.yoffset = Ageometry.No_offset;
    };
  if !autoscale then update_dvi_size true ?dx ?dy st;
  redraw st;;

let scale n st =
  let factor =
    if n > 0 then !scale_step ** float n
    else  (1. /. !scale_step) ** float (0 - n) in
  if !autoresize then begin
    let scale x = Misc.round (float x *. factor) in
    attr.geom.Ageometry.width <- scale st.size_x;
    attr.geom.Ageometry.height <- scale st.size_y;
    Grdev.resize_dev attr.geom.Ageometry.width attr.geom.Ageometry.height;
  end else begin
    let new_ratio = factor *. st.ratio in
    if new_ratio >= 0.02 && new_ratio < 50.0 then begin
      st.ratio <- new_ratio;
      let (cx, cy) = (st.size_x / 2, st.size_y / 2) in
      st.orig_x <- Misc.round (float (st.orig_x - cx) *. factor) + cx;
      st.orig_y <- Misc.round (float (st.orig_y - cy) *. factor) + cy;
    end;
  end;
  update_dvi_size true st;
  redraw st;;

(* Keymap kinds for Active-DVI:
   - Default_keymap is for normal key bindings.
   - Control_x_keymap is for ^X prefixed key bindings. *)
type keymap =
   | Control_x_keymap
   | Default_keymap;;

let set_keymap, get_keymap =
  let map = ref Default_keymap in
  let get_keymap () = !map in
  let set_keymap km =
     match km with
     | Default_keymap ->
         Busy.restore_cursor ();
         map := km
     | Control_x_keymap ->
         Busy.temp_set Busy.Change_Keymap;
         map := km in
  (set_keymap, get_keymap);;

module B =
  struct
    let nop st = ()
    let unbound c st =
      Misc.warning (Printf.sprintf "Unbound key %C" c)
    let control_x_unbound c st =
      set_keymap Default_keymap;
      Misc.warning (Printf.sprintf "Unbound Control-x key %C" c)
    let set_control_x_keymap st =
      set_keymap Control_x_keymap
    let push_next_page st =
      push_page false (st.page_number + max 1 st.num) st
    let next_pause st =
      if st.cont = None then push_next_page st
      else goto_next_pause (max 1 st.num) st
    let digit k st =
      st.next_num <- st.num * 10 + k
    let next_page st =
      goto_page (st.page_number + max 1 st.num) st
    let goto st =
      push_page true (if st.num > 0 then st.num - 1 else st.num_pages) st
    let goto_pageref st =
      goto_pageref st.num st
    let push_page st =
      push_stack true st.page_number st
    let previous_page st =
      goto_page (st.page_number - max 1 st.num) st
    let pop_previous_page st =
      pop_page false (max 1 st.num) st
    let previous_pause st =
      if st.pause_number > 0
      then goto_previous_pause (max 1 st.num) st
      else
        set_page_pause_num
          (st.page_number - max 1 st.num) (st.last_pause_number) st
    let pop_page st =
      pop_page true 1 st

    let first_page st =
      goto_page 0 st
    let last_page st =
      goto_page max_int st
    let exchange_page st =
      goto_page st.exchange_page st
    let unfreeze_fonts st =
      Driver.unfreeze_fonts st.cdvi
    let unfreeze_glyphs st =
      Driver.unfreeze_glyphs st.cdvi (st.base_dpi *. st.ratio)
    let center st =
      st.ratio <- 1.0;
      update_dvi_size false st;
      st.orig_x <- (st.size_x - st.dvi_width) / 2;
      st.orig_y <- (st.size_y - st.dvi_height) / 2;
      redraw st

    let scale_up st = scale (max 1 st.num) st
    let scale_down st = scale (0 - max 1 st.num) st

    let nomargin st =
      attr.hmargin <- Px 0; attr.vmargin <- Px 0;
      update_dvi_size true st;
      redraw st

    let page_left st =
      match move_within_margins_x st (attr.geom.Ageometry.width - 10) with
      | Some n ->
          if n > st.orig_x  then begin
            st.orig_x <- n;
            set_bbox st;
            redraw st
          end
      | None -> ()

    let page_right st =
      match move_within_margins_x st (10 - attr.geom.Ageometry.width) with
      | Some n ->
          if n < st.orig_x then begin
            st.orig_x <- n;
            set_bbox st;
            redraw st
          end
      | None -> ()

    let page_down st =
      let none () =
        if st.page_number < st.num_pages - 1 then begin
          (* the following test is necessary because of some
           * floating point rounding problem *)
          if attr.geom.Ageometry.height <
            st.dvi_height + 2 * vmargin_size st then begin
              st.orig_y <- top_of_page st;
              set_bbox st;
            end;
          goto_page (st.page_number + 1) st
        end in
      match move_within_margins_y st (10 - attr.geom.Ageometry.height) with
      | Some n ->
          (* this test is necessary because of rounding errors *)
          if n > st.orig_y then none () else begin
            st.orig_y <- n;
            set_bbox st;
            redraw st
          end
      | None -> none ()

    let page_up st =
      let none () =
        if st.page_number > 0 then begin
          if attr.geom.Ageometry.height <
            st.dvi_height + 2 * vmargin_size st then begin
              st.orig_y <- bottom_of_page st;
              set_bbox st;
            end;
          goto_page (st.page_number - 1) st
        end in
      match move_within_margins_y st (attr.geom.Ageometry.height - 10) with
      | Some n ->
          if n < st.orig_y then none () else begin
            st.orig_y <- n;
            set_bbox st;
            redraw st
          end
      | None -> none ()

    let scroll_down st =
      if !scroll_fast then next_page st else page_down st
    let scroll_up st =
      if !scroll_fast then previous_page st else page_up st
    let toggle_center_on_cursor = Options.toggle center_on_cursor

    let scroll_switch st = scroll_fast := not !scroll_fast

    let redraw = redraw ?trans:(Some Transitions.DirNone) ?chst:None

    let toggle_active st =
      Driver.toggle_active (); redraw st

    let reload = reload true
    let redisplay = redisplay
    let resyncing = resyncing
    let toggle_postsyncing st = postsyncing := not !postsyncing

    let toggle_full_screen st =
      let b = (st.full_screen = None) in
      let (size_x, size_y), (dx, dy) =
        match st.full_screen with
        | None ->
            let x = GraphicsY11.origin_x () in
            let y = GraphicsY11.origin_y () in
            st.full_screen <-
              Some (x, y, st.size_x, st.size_y, (st.orig_x, st.orig_y));
            (* negative width and height mean full_screen,
               last parameter is screen number *)
            st.in_full_screen <- true;
            Grdev.reposition ~x:0 ~y:0 ~w:(-1) ~h:(-1) ~screen:st.num,
            (0, 0)
        | Some (x, y, w, h, dxy) ->
            st.full_screen <- None;
            st.in_full_screen <- false;
            Grdev.reposition ~x ~y ~w ~h ~screen:0,
            dxy in
      resize st ~dx ~dy size_x size_y;
      if b then center st

    let full_screen st =
      set_keymap Default_keymap;
      if not st.in_full_screen then begin
        toggle_full_screen st;
        st.in_full_screen <- true;
      end

    let exit st = raise Exit
    let switch_edit_mode st =
      Grdev.E.switch_edit_mode ();
      redraw st
    let clear_image_cache st = (* clear image cache *)
      Grdev.clean_ps_cache ()
    let help st =
      ignore (
      Launch.fork_me
        (Printf.sprintf "-g %dx%d"
           attr.geom.Ageometry.width
           attr.geom.Ageometry.height)
        Config.splash_screen)

    let toggle_antialiasing st =
      Gs.toggle_antialiasing ()

    let scratch_draw st =
      Scratch.draw ()
        
    let scratch_write st =
      Scratch.write ()

    let previous_slice = previous_slice
    let next_slice = next_slice
    let mark_page = mark_page
    let goto_mark st = goto_mark st.num st

    let make_thumbnails st =
      Launch.without_launching
        (Busy.busy_exec
           (fun () ->
             make_thumbnails st;
             if not st.aborted then show_toc st))
        ()

    let show_toc st =
      Launch.without_launching show_toc st

    let ask_to_search =
      let prefill = ref "" in
      (fun message ->
        let minibuff =
          let nlines = 1 in
          let xc, yc = 2, 2 in
          let sx, sy = Graphics.text_size "X" in
          let wt, ht = Graphics.size_x () - 2 * xc, sy * nlines in
          let ncol = wt / sx in
          let bw = 0 in
          Gterm.make_term_gen
            Graphics.black (Graphics.rgb 180 220 220) (* Grounds colors *)
            bw Graphics.black (* Border *)
            (Graphics.rgb 220 150 120) (* Title color *)
            (Graphics.black) (* Cursor color *)
            xc yc (* Where on the screen *)
            ncol nlines (* Size in columns and lines *) in
        Gterm.draw_term minibuff;
        let re = Gterm.ask_prefill minibuff message !prefill in
        (* Forces future redisplay. *)
        Misc.push_key_event '' GraphicsY11.control;
        prefill := re;
        re)

    let search_forward st =
      let re_string = ask_to_search "Search Forward (re): " in
      Misc.warning (Printf.sprintf "Search forward %s" re_string);
      let _re = Str.regexp re_string in
      ()

    let search_backward st =
      let re_string = ask_to_search "Search Backward (re): " in
      Misc.warning (Printf.sprintf "Search backward %s" re_string);
      let _re = Str.regexp re_string in
      ()

    let duplex = duplex_switch false
    let duplex_sync = duplex_switch true
    let toggle_autoswitch st = toggle_autoswitch ()

    let revert_to_default_keymap f st =
      Busy.busy_exec f ();
      set_keymap Default_keymap

    let save_page_image = revert_to_default_keymap Shot.save_page_image

    let laser_beam st =
      set_keymap Default_keymap;
      Laser_pointer.laser_beam ()

    let abort_key st =
      set_keymap Default_keymap

  end;;

let bind_key tbl (key, action) = tbl.(int_of_char key) <- action;;

let default_keymap = Array.init 256 (fun i -> B.unbound (char_of_int i));;
let bind_default_key = bind_key default_keymap;;

let bind_default_keys () =
  for i = 0 to 9 do
    bind_default_key (char_of_int (int_of_char '0' + i), B.digit i)
  done;
  List.iter bind_default_key [
   (* Default key bindings. *)

   (* General purpose keys. *)
   'A', B.toggle_antialiasing;
   'a', B.toggle_active;
   'q', B.exit;
   '?', B.help;

   (* hjkl to move the page around *)
   'h', B.page_left;
   'j', B.page_down;
   'k', B.page_up;
   'l', B.page_right;
   '', B.toggle_center_on_cursor; 
   (* toggle scroll fast *)
   '\010', B.scroll_switch;

   (* m, i are reserved for scrolling
     'm', B.scroll_one_line_down;
     'i', B.scroll_one_line_up;
    *)

   (* return, tab, backspace, escape, and x
      to handle the page marks stack. *)
   (* '\t' (* tab *), B.push_page; *)
   '\t' (* tab *), B.show_toc;
   '' (* Escape *), (*B.nop *) B.pop_page; 

   '\b' (* backspace *), B.pop_previous_page;
   '\r' (* return *), B.push_next_page;

   'x', B.exchange_page;
   'M', B.mark_page;
   'm', B.goto_mark;

   (* space, n, p, P, N to go on, or go back to next pause or page. *)
   ' ', B.next_pause;
   'n', B.next_page;
   '', B.next_page;
   'p', B.previous_page;
   '\246' (* o'' *), B.previous_page;
   'P', B.previous_pause;
   'N', B.next_pause;

   (* ^P, ^N in edit mode means previous -or next- slice *)
   '', B.next_slice;
   '', B.previous_slice;

   (* comma, ., g, to go to a page. *)
   ',', B.first_page;
   '.', B.last_page;
   'g', B.goto;
   'G', B.goto_pageref;

   (* r, Control-l, R, to redraw, redisplay, or reload. *)
   'r', B.redraw;
   'R', B.reload;
   '', B.redisplay;
   '/', B.resyncing;
   '|', B.toggle_postsyncing;

   (* Control-f, c, to handle the advi window. *)
   '', B.toggle_full_screen;
   'c', B.center;

   (* Searching: Control-s search forward, Control-r search backward. *)
   '', B.search_forward;
   '', B.search_backward;

   (* Scaling the page. *)
   '<', B.scale_down;
   '-', B.scale_down;
   '_', B.scale_down;
   '>', B.scale_up;
   '+', B.scale_up;
   '=', B.scale_up;
   '#', B.nomargin;

   (* Efficiency related keys. *)
   'f', B.unfreeze_fonts;
   'F', B.unfreeze_glyphs;
   'C', B.clear_image_cache;

   (* Edit mode. *)
   'e', B.switch_edit_mode;

   (* Scratching. *)
   's', B.scratch_write;
   'S', B.scratch_draw;

   (* Thumbnails. *)
   'T', B.make_thumbnails;
   't', B.show_toc;

    (* Duplex *)
   '', B.toggle_autoswitch;
   'w', B.duplex;
   'W', B.duplex_sync;

    (* First step to a true Emacs-like input policy... *)
   '', B.set_control_x_keymap;
   '', B.abort_key;

  ];;

let control_x_keymap =
  Array.init 256 (fun i -> B.control_x_unbound (char_of_int i));;
let bind_control_x_key = bind_key control_x_keymap;;

(* Bindings for ^X-prefixed keys. *)
let bind_control_x_keys () =
  List.iter bind_control_x_key [
    'l', B.laser_beam;
    '', B.exit;
    '', B.full_screen;
    '', B.abort_key;
    '', B.save_page_image;
  ];;

let bind_keys () =
  bind_default_keys ();
  bind_control_x_keys ();;

bind_keys ();;

let main_loop mastername clients =
  let st = init true mastername in
  begin match clients with
  | duplexname :: _ ->
      let st' = init false duplexname in
      if compatible st st' then begin
        st.duplex <- Master st';
        st'.duplex <- Client st;
      end else
        Misc.warning
          (Printf.sprintf
             "Ignoring client %s incompatible with master file %s"
             duplexname st.filename)
  | [] -> ()
  end;
  (* Check if white run *)
  if Launch.white_run () then begin
    Driver.scan_special_pages st.cdvi (st.num_pages - 1);
    Launch.dump_white_run_commands ()
  end else begin
    Grdev.set_title ("Advi: " ^ st.filename);
    let x, y = Grdev.open_dev (" " ^ Ageometry.to_string attr.geom) in
    attr.geom.Ageometry.width <- x;
    attr.geom.Ageometry.height <- y;
    update_dvi_size true st;
    set_bbox st;
    let rec duplex idraw st =
      Graphics.set_window_title  ("Advi: " ^ st.filename);
      if st.page_number > 0 && Gs.get_do_ps () then
        Driver.scan_special_pages st.cdvi st.page_number
      else set_page_number st (fst (page_start 0 st));
      if changed st then reload false st;
      idraw st;
      (* num is the current number entered by keyboard *)
      (* Printf.eprintf "%s\n%!"
         (match st.duplex with
         | Client _ -> "Client"
         | Master _ -> "Master "
         | Alone -> "Alone"); *)
      try while true do
        st.num <- st.next_num;
        st.next_num <- 0;
        let last_command = st.last_command in
        st.last_command <- Other;
        if changed st then reload true st else
        match Grdev.wait_event () with
        | Grdev.Refreshed ->
            begin match st.duplex with
            | Master _ | Alone ->
                B.reload st
            | Client st' ->
                if !autoswitch || last_command = Position
                then raise (Duplex (B.reload, st'))
                else Grdev.clear_usr1 ();
            end
        | Grdev.Resized (x, y) -> resize st x y
        | Grdev.Key c | Grdev.Stdin c ->
            let keymap =
              match get_keymap () with
              | Default_keymap -> default_keymap
              | Control_x_keymap -> control_x_keymap in
            keymap.(Char.code c) st
        | Grdev.Href h -> goto_href h st
        | Grdev.Advi (s, a) -> a ()
        | Grdev.Move (w, h) ->
            st.orig_x <- st.orig_x + w;
            st.orig_y <- st.orig_y + h;
            set_bbox st;
            redraw st
        | Grdev.Edit (p, a) ->
            print_endline (Grdev.E.tostring p a);
            flush stdout;
            redraw st
        | Grdev.Region (x, y, w, h) -> ()
        | Grdev.Selection s -> selection s
        | Grdev.Position (x, y) ->
            st.last_command <- Position;
            position st x y
        | Grdev.Click (_, Grdev.Button4, _, _)  -> B.scroll_up st
        | Grdev.Click (_, Grdev.Button5, _, _)  -> B.scroll_down st
        | Grdev.Click (pos, but, _, _) when Grdev.E.editing () ->
            begin match pos, but with
            | Grdev.Top_left, Grdev.Button1 -> B.previous_slice st
            | Grdev.Top_left, Grdev.Button2 -> B.reload st
            | Grdev.Top_left, Grdev.Button3 -> B.next_slice st
            | Grdev.Top_right, Grdev.Button1 -> B.previous_page st
            | Grdev.Top_right, Grdev.Button2 -> B.last_page st
            | Grdev.Top_right, Grdev.Button3 -> B.next_page st
            | _, _ -> ()
            end
        | Grdev.Click (Grdev.Top_left, _, _, _) ->
            if !click_turn_page then B.pop_page st
        | Grdev.Click (_, Grdev.Button1, _, _) ->
            if !click_turn_page then B.previous_pause st
        | Grdev.Click (_, Grdev.Button2, _, _) ->
            if !click_turn_page then B.pop_previous_page st
        | Grdev.Click (_, Grdev.Button3, _, _) ->
            if !click_turn_page then B.next_pause st
        | Grdev.Nil -> ()
        | Grdev.Click (_, Grdev.NoButton, _, _)  -> ()
      done with
      | Exit -> Grdev.close_dev ()
      | Duplex (action, st') -> duplex action st' in
    duplex redraw st
  end;;
