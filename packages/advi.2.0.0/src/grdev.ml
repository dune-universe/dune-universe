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

open Misc;;

let ignore_background =
    Options.flag false
    "-ignore-background"
    "  ignore background for antialiasing,\
    \n\t (the default is to look at the background during antialiasing).";;

let glyph_gamma = ref 1.0;;

Options.add
  "-gamma"
  (Arg.Float (fun x ->
    if x <= 0.0 then Misc.warning "gamma value must be positive"
    else glyph_gamma := 1.0 /. x))
  (Printf.sprintf
     "<float> (>0)  set to <float> the gamma correction of glyphs,\
     \n\t (the default correction is %f)." !glyph_gamma)
;;

type color = Graphics.color;;

let href_frame_color = 0x00ff00;;
let advi_frame_color = 0xaaaaff;;
let rect_emphasize_color = Graphics.blue;;
let href_emphasize_color = 0xffff00;;
let name_emphasize_color = 0xffaaaa;;
let cut_emphasize_color = Graphics.cyan;;

(*** Device configuration ***)

let opened = ref false;;

let size_x = ref 0;;
let size_y = ref 0;;

let xmin = ref 0;;
let xmax = ref 0;;
let ymin = ref 0;;
let ymax = ref 0;;

let get_initial_window_device_geometry, set_initial_window_device_geometry =
  let initial_window_device_geometry = ref "" in
  (fun () -> !initial_window_device_geometry),
  (fun geom -> initial_window_device_geometry := geom)
;;

let update_device_geometry () =
  size_x := Graphics.size_x ();
  size_y := Graphics.size_y ();
  xmin := 0; xmax := !size_x;
  ymin := 0; ymax := !size_y;;

(* Communication with GS *)
exception Stop;;

let dvi = true;;
let ps = false;;
let psused = ref false;;
let last_is_dvi = ref true;;

let flush_ps () =
  psused := true;
  Gs.flush ();;

let flush_dvi () =
  GraphicsY11.flush ();;

type syncing = Always | Page | Now | Later | Never
let syncing_status = ref Never (* Immediately *)
let reset_syncing () =
  match !syncing_status with
  | Page -> syncing_status := Never
  | Now -> syncing_status := Later
  | _ -> ()

let toggle_syncing () =
  match !syncing_status with 
  | Always -> syncing_status := Never
  | Never -> syncing_status := Always 
  | _ -> ()

let try_finalize f x finally y =
  let res = try f x with exn -> finally y; raise exn in 
  finally y; 
  res

let with_syncing f x =
  let old = !syncing_status in
  let cur = 
  match old with
  | Always | Page | Now as x -> x
  | Later -> Now
  | Never -> Page in
  syncing_status := cur;
  let res = try f x with exn -> syncing_status := old; raise exn in
  syncing_status := old;
  res 

let syncing() =
  match !syncing_status with
  | Always | Page | Now -> true
  | Later | Never -> false

let flush_last () =
  if syncing() then 
    if  !last_is_dvi then flush_dvi () else flush_ps ();;

let sync b =
  if syncing() then
    if !last_is_dvi <> b then begin flush_last (); last_is_dvi := b end;;

let dosync_ps() =
  if !last_is_dvi <> true then begin flush_ps (); last_is_dvi := true end;;

let title = ref "Advi";;
let set_title s = title := s;;

let synchronize () =
  if syncing() then () else dosync_ps ();
  Gs.flush ();
  Transimpl.synchronize_transition ();
  GraphicsY11.synchronize ();
  Launch.launch_embedded_apps ();;

(* For refreshed signal on usr1 *)
exception Usr1;;
exception Usr2;;

let waiting = ref false;;
let usr1 = Sys.sigusr1;;
let usr2 = Sys.sigusr2;;
let usr1_status = ref false;;
let usr2_status = ref false;;
let clear_usr1 () = usr1_status := false;;
let clear_usr2 () = usr2_status := false;;

exception Watch_file;;
exception Input;;
let watch_file_interval = ref 0;;
Options.add
  "-watch-file"
  (Arg.Int (fun x -> watch_file_interval := x))
  "<int> watch for newer file every <int> second. \
  \n\t 0 means do not watch (default value)";;

let inputp fdins = 
  match Unix.select fdins [] [] 0.0 with
  | [], _, _ -> false
  | _, _, _ -> true

let get_input () =
  let buf = Bytes.create 1 in
  if inputp [ Unix.stdin ] then
    let _ = Unix.read Unix.stdin buf 0 1 in 
    if inputp [ Unix.stdin ] then () 
    else clear_usr2();
    Bytes.get buf 0
  else assert false;;

let watch_file_check () = 
  if !waiting then raise Watch_file;;

let set_usr1 () =
  Sys.set_signal usr1
    (Sys.Signal_handle
       (fun _ -> usr1_status := true; if !waiting then raise Watch_file
       ));;
set_usr1 ();;
let set_usr2 () =
  Sys.set_signal usr2
    (Sys.Signal_handle
       (fun _ -> usr2_status := true; if !waiting then raise Usr2
       ));;
set_usr2 ();;

let buttons45 = GraphicsY11.button4 lor GraphicsY11.button5
let preemptive_click() =
  GraphicsY11.button_enqueued buttons45

let sleep_broken = ref false;;
let clear_sleep () =
  sleep_broken := GraphicsY11.key_pressed () || preemptive_click ()
;;

(* returns false if sleep is fully performed. returns true if interrupted *)
let sleep_watch breakable sync n =
  let start = Unix.gettimeofday () in
  let interrupted () =
    if breakable &&
      begin
        !usr1_status || !sleep_broken ||
        GraphicsY11.key_pressed () || preemptive_click() 
      end
    then begin
      if GraphicsY11.key_pressed () then ignore (GraphicsY11.read_key ());
      sleep_broken := true;
      true
    end else false
  in
  let rec delay t =
    if interrupted () then raise Exit (* if there is a sig or key press, exit*)
    else begin
      try ignore (Unix.select [] [] [] t)
      with Unix.Unix_error(Unix.EINTR, _, _) -> ()
    end;
    let now = Unix.gettimeofday () in
    let remaining = start +. n -. now in
    if remaining > 0.0 then delay remaining else false
  in
  interrupted () || (* if it is interrupted, synchronization is not done *)
  begin
    if sync then synchronize ();
    try delay n with Exit -> true
  end;;

let sleep = sleep_watch true true;;

(* trans *)

(* if -nopauses is specified, Transimpl.sleep function is overridden by
   (fun _ -> true) (look at dviview.ml) *)
Transimpl.sleep := sleep_watch true false;;

let set_transition trans =
  Misc.debug_endline
    (Printf.sprintf "Setting transition mode to : %s"
       (Transimpl.string_of_transmode trans));
  Transimpl.current_transition := trans;;

let transbox_save x y width height =
  synchronize ();
  let x' = x and y' = !size_y - y in
  Transimpl.transbox_save x' y' width height;;

let transbox_go trans =
  Gs.flush ();
  Transimpl.transbox_go trans;
  synchronize ();;

(*** Private glyphs ***)

type cache =
   | No_cache
   | Cached of (color * color) * Graphics.image;;

module Glyph =
  struct
    type t = {
        glyph : Glyph.t;
        mutable cache : cache;
        mutable img_list : ((color * color) * Graphics.image) list
      }

    let width g = g.glyph.Glyph.width
    let height g = g.glyph.Glyph.height
    let hoffset g = g.glyph.Glyph.hoffset
    let voffset g = g.glyph.Glyph.voffset
    let graymap g = g.glyph.Glyph.graymap
  end;;

type glyph = Glyph.t;;
open Glyph;;

let make_glyph g =
  { glyph = g;
    cache = No_cache;
    img_list = [] };;

let get_glyph g = g.glyph;;

(* Alpha channel. *)
let alpha = ref 1.0;;
let set_alpha a = alpha := a;;

(* Blending *)
type blend =
   | Normal | Multiply | Screen | Overlay (* | SoftLight | HardLight *)
   | ColorDodge | ColorBurn | Darken | Lighten | Difference
   | Exclusion (* | Luminosity | Color | Saturation | Hue *);;

let blend = ref Drawimage.Normal;;
let set_blend b = blend := b;;

let epstransparent = ref true;;
let set_epstransparent s = epstransparent := s;;

let epsbygs = ref true;;
let set_epsbygs s = epsbygs := s;;

let epswithantialiasing = ref true;;
let set_epswithantialiasing a = epswithantialiasing := a;;

(* Background implementation *)

(* Viewport type definition *)

type x = GraphicsY11.x
and y = GraphicsY11.y
and w = GraphicsY11.w
and h = GraphicsY11.h;;

type ratio = float;;
type xratio = ratio
and yratio = ratio;;

type viewport = {vx : x; vy : y; vw : w; vh : h};;
(** Viewports: x, y, size_x, size_y, in advi coordinates. *)

(* The Background preferences                    *)
type bkgd_prefs = {
  mutable bgcolor : color;
  mutable bgcolorstart : color option;
  mutable bgcolorstop : color option;
  mutable bgimg : string option;
  mutable bgratiopt : Drawimage.ratiopt;
  mutable bgwhitetransp : bool;
  mutable bgalpha : Drawimage.alpha;
  mutable bgblend : Drawimage.blend;
  mutable bgxstart : xratio;
  mutable bgystart : yratio;
  mutable bgwidth : xratio;
  mutable bgheight : yratio;
  mutable bgxcenter : xratio option;
  mutable bgycenter : yratio option;
  mutable bgviewport: viewport option;
  (* hook for sophisticated programmed graphics backgrounds *)
  mutable bggradient: (bgfunarg -> unit) option;
}

(* Argument to a function that operates on the background.
   Suitable either for predefined gradient colors, or
   (in the future) for user's defined dynamically loadable functions.  *)
and bgfunarg = {
 argcolor : color;
 argcolorstart : color option;
 argcolorstop : color option;
 argxcenter : x;
 argycenter : y;
 argfunviewport : viewport;
 argviewport : viewport;
};;

(* The type of the options that drive the background drawing. *)
type bgoption =
   | BgColor of color
   | BgColorStart of color
   | BgColorStop of color
   | BgImg of Misc.file_name
   | BgAlpha of Drawimage.alpha
   | BgBlend of Drawimage.blend
   | BgRatio of Drawimage.ratiopt
   | BgViewport of viewport option
   | BgXStart of xratio
   | BgYStart of yratio
   | BgWidth of xratio
   | BgHeight of yratio
   | BgXCenter of xratio
   | BgYCenter of yratio
   | BgGradient of (bgfunarg -> unit) option;;

let full_ratio = 1.0
and null_ratio = 0.0;;

let full_screen_view () = {vx = !xmin; vy = !ymin; vw = !size_x; vh = !size_y};;

let default_bkgd_prefs c = {
    bgcolor = c;
    bgcolorstart = None;
    bgcolorstop = None;
    bgimg = None;
    bgratiopt = Drawimage.ScaleAuto;
    bgwhitetransp = false;
    bgalpha = full_ratio;
    bgblend = Drawimage.Normal;
    bgviewport = None;
    bgxstart = null_ratio;
    bgystart = null_ratio;
    bgwidth = full_ratio;
    bgheight = full_ratio;
    bgxcenter = None;
    bgycenter = None;
    bggradient = None;
};;

let bkgd_data = default_bkgd_prefs Graphics.white;;

let get_bgcolor, set_bgcolor =
  let advi_bgcolor = ref Graphics.white in
  (fun () -> !advi_bgcolor),
  (fun c ->
     advi_bgcolor := c;
     bkgd_data.bgcolor <- c);;

let get_fgcolor, set_fgcolor =
  let advi_fgcolor = ref Graphics.black in
  (fun () -> !advi_fgcolor),
  (fun c -> advi_fgcolor := c);;

let set_bgcolor_string s =
  set_bgcolor (Dvicolor.parse_color (String.lowercase_ascii s));;

let set_fgcolor_string s =
  set_fgcolor (Dvicolor.parse_color (String.lowercase_ascii s));;

Options.add
  "-bgcolor"
  (Arg.String set_bgcolor_string)
  "<string>  set the default background color (named, RGB, or X),\
  \n\t (the default background is \"white\").";;

Options.add
  "-fgcolor"
  (Arg.String set_fgcolor_string)
  "<string>  set the default foreground color (named, RGB, or X),\
  \n\t (the default foreground is \"black\").";;

let reverse_colors () =
  let fc = get_fgcolor () in
  let bc = get_bgcolor () in
  set_fgcolor bc;
  set_bgcolor fc;;

Options.add
  "-rv"
  (Arg.Unit reverse_colors)
  "  ask for reverse video\
  \n\t (simulated by exchanging the foreground and background colors).";;

let default_bkgd_data () = default_bkgd_prefs (get_bgcolor ());;

let blit_bkgd_data s d =
  d.bgcolor <- s.bgcolor;
  d.bgcolorstart <- s.bgcolorstart;
  d.bgcolorstop <- s.bgcolorstop;
  d.bgimg <- s.bgimg;
  d.bgratiopt <- s.bgratiopt;
  d.bgwhitetransp <- s.bgwhitetransp;
  d.bgalpha <- s.bgalpha;
  d.bgblend <- s.bgblend;
  d.bgviewport <- s.bgviewport;
  d.bgxstart <- s.bgxstart;
  d.bgystart <- s.bgystart;
  d.bgwidth <- s.bgwidth;
  d.bgheight <- s.bgheight;
  d.bgxcenter <- s.bgxcenter;
  d.bgycenter <- s.bgycenter;
  d.bggradient <- s.bggradient;;

let copy_of_bkgd_data () =
  let c = default_bkgd_data () in
  blit_bkgd_data bkgd_data c;
  c;;

let bg_color = ref bkgd_data.bgcolor;;
let bg_colors = ref [];;

let draw_img file whitetransp alpha blend
      psbbox ratiopt antialias (w, h) x0 y0 =
  if not !opened then failwith "Grdev.draw_img: no window";
  Drawimage.f file whitetransp alpha blend
    psbbox ratiopt antialias (w, h) (x0, !size_y - y0);;

let round_dim r t = Misc.round (r *. float t);;
let round_dim_x r = round_dim r !size_x;;
let round_dim_y r = round_dim r !size_y;;

let make_funviewport bkgd_data viewport =
  let fx = round_dim_x bkgd_data.bgxstart
  and fy = round_dim_y bkgd_data.bgystart
  and fw = round_dim_x bkgd_data.bgwidth
  and fh = round_dim_y bkgd_data.bgheight in
  Misc.debug_endline (Printf.sprintf
    "The parsed function viewport is {vx = %d; vy = %d; vw = %d; vh = %d}."
    fx fy fw fh);
  let {vx = vx; vy = vy; vw = vw; vh = vh} = viewport in
  (* Clipping of the function view to the current viewport *)
  let fx = max fx vx
  and fy = max fy vy
  and fw = min fw vw
  and fh = min fh vh in
  Misc.debug_endline (Printf.sprintf
    "The viewport is {vx = %d; vy = %d; vw = %d; vh = %d}."
    vx vy vw vh);
  Misc.debug_endline (Printf.sprintf
    "The function viewport is {vx = %d; vy = %d; vw = %d; vh = %d}."
    fx fy fw fh);
  {vx = fx; vy = fy; vw = fw; vh = fh};;

(* Try to figure out what can be used as a center:
   if the specified center in the funviewport is
   within the background viewport we choose it;
   otherwise, we choose the center of the background viewport. *)
let make_center xcr ycr
    ({vx = x; vy = y; vw = w; vh = h} as _bgviewport)
    ({vx = fx; vy = fy; vw = fw; vh = fh} as _funviewport) =
  let xc = match xcr with
  | Some xcr ->
      (* Get the center coordinates integer values in the funviewport. *)
      let fxc = fx + round_dim xcr fw in
      (* Check the center coordinates wrt the background viewport. *)
      if fxc < x then x else
      if fxc < x + w then fxc else x + w - 1
  | None -> fx + (fw + 1) / 2 in
  let yc = match ycr with
  | Some ycr ->
      (* Get the center coordinates integer values in the funviewport. *)
      let fyc = fy + round_dim ycr fh in
      (* Check the center coordinates wrt the background viewport. *)
      if fyc < y then y else
      if fyc < y + h then fyc else y + h - 1
  | None -> fy + (fh + 1) / 2 in
  Misc.debug_endline (Printf.sprintf
    "The gradient center is xc = %d; yc = %d." xc yc);
  xc, yc;;

let make_funarg bkgd_data bgviewport funviewport =
  (* Get the center coordinates ratios. *)
  let xcr = bkgd_data.bgxcenter
  and ycr = bkgd_data.bgycenter in
  let xc, yc = make_center xcr ycr bgviewport funviewport in
  {
    argcolor = bkgd_data.bgcolor;
    argcolorstart = bkgd_data.bgcolorstart;
    argcolorstop = bkgd_data.bgcolorstop;
    argxcenter = xc;
    argycenter = yc;
    argfunviewport = funviewport;
    argviewport = bgviewport;
  };;

let draw_bkgd () =
  (* Find the viewport *)
  let bgviewport =
    match bkgd_data.bgviewport with
    | None -> full_screen_view ()
    | Some v -> v in

  (* Background: paint with the solid color. *)
  bg_color := bkgd_data.bgcolor;
  Graphics.set_color !bg_color;
  let {vx = x; vy = y; vw = w; vh = h} = bgviewport in
  (* Background: solid color.
     Fix me: why this test ? could have a white bg, no ?
     -> Yes, but then it is useless to draw the rectangle.
     -> Mmm is it worth the burden to test ? *)
  if !bg_color <> Graphics.white then Graphics.fill_rect x y w h;

  (* Background: apply the gradient function if any. *)
  begin match bkgd_data.bggradient with
  | None -> ()
  | Some bggradient ->
      let funviewport = make_funviewport bkgd_data bgviewport in
      let bgfunarg = make_funarg bkgd_data bgviewport funviewport in
      let string_of_color_opt = function 
        | None -> "None"
        | Some c -> Printf.sprintf "Some %d" c in
      Misc.debug_endline (Printf.sprintf
        "colors are {argcolor = %d; argcolorstart = %s; argcolorstop = %s}."
        bkgd_data.bgcolor (string_of_color_opt bkgd_data.bgcolorstart)
                          (string_of_color_opt bkgd_data.bgcolorstop));
      bggradient bgfunarg end;

  (* Background: now draw the image. *)
  let draw_bg file =
    Drawimage.f file bkgd_data.bgwhitetransp bkgd_data.bgalpha
      bkgd_data.bgblend None
      bkgd_data.bgratiopt
      true (* antialias *)
      (w, h) (x, y) in
  lift draw_bg bkgd_data.bgimg;;

let set_bg_option = function
  | BgColor c -> bkgd_data.bgcolor <- c
  | BgColorStart c -> bkgd_data.bgcolorstart <- Some c
  | BgColorStop c -> bkgd_data.bgcolorstop <- Some c
  | BgImg file -> bkgd_data.bgimg <- Some file
  | BgAlpha a -> bkgd_data.bgalpha <- a
  | BgBlend b -> bkgd_data.bgblend <- b
  | BgRatio f -> bkgd_data.bgratiopt <- f
  | BgViewport v -> bkgd_data.bgviewport <- v
  | BgXStart x -> bkgd_data.bgxstart <- x
  | BgYStart y -> bkgd_data.bgystart <- y
  | BgWidth w -> bkgd_data.bgwidth <- w
  | BgHeight h -> bkgd_data.bgheight <- h
  | BgXCenter xc -> bkgd_data.bgxcenter <- Some xc
  | BgYCenter yc -> bkgd_data.bgycenter <- Some yc
  | BgGradient f -> bkgd_data.bggradient <- f;;

let set_bg_options l = List.iter set_bg_option l;;

let push_bg_color c =
  bg_colors := !bg_color :: !bg_colors;
  bg_color := c;;

let pop_bg_color () =
  match !bg_colors with
  | h :: t -> bg_color := h; bg_colors := t
  | [] -> bg_color := bkgd_data.bgcolor;;

let background_colors = ref [];;
let add_background_color x y w h c =
  background_colors := (x, y, w, h, c) :: !background_colors;;

let find_bg_color x y w h =
  let rec find_color = function
    | (x0, y0, w0, h0, c) :: t ->
        if x0 <= x && y0 <= y && x + w <= x0 + w0 && y + h <= y0 + h0
        then c
        else find_color t
    | [] -> !bg_color in
  find_color !background_colors;;

(* Forward to Driver.playing. *)
let forward_get_playing =
  ref (fun () -> (failwith "Undefined forward get_playing" : int));;
let set_forward_get_playing f = forward_get_playing := f;;

let get_playing () = !forward_get_playing ();;

let rgb_of_color c =
  let b = (c land 0x0000ff) in
  let g = (c land 0x00ff00) lsr 8 in
  let r = (c land 0xff0000) lsr 16 in
  r, g, b;;

let mean_color c c' =
  let r, g, b = rgb_of_color c in
  let r', g', b' = rgb_of_color c' in
  Graphics.rgb ((r + r' + 1) / 2) ((g + g' + 1) / 2) ((b + b' + 1) / 2);;

let point_color x y =
  let x' = min (!size_x - 1) x and y' = min (!size_y - 1) y in
  GraphicsY11.point_color x' y';;

let get_bg_color x y w h =
  if !ignore_background then Graphics.white else begin
    sync dvi;
    if !psused || bkgd_data.bgimg <> None ||
       bkgd_data.bggradient <> None ||
       bkgd_data.bgviewport <> None then
      let c = point_color (max 0 x + 1) (max 0 y + 1) in
      let c' = point_color (min !size_x (x + w) - 1)
                           (min !size_y (y + h) - 1) in
      if c = c' then c else 
      if get_playing () > 0 then find_bg_color x y w h else
      mean_color c c'
    else find_bg_color x y w h
  end;;

let get_color_table =
  let htable = Hashtbl.create 257 in
  function (bg, fg as col) ->
    try Hashtbl.find htable col
    with Not_found ->
      let table = Array.make 256 Graphics.transp in
      let r0 = (bg lsr 16) land 0xff
      and g0 = (bg lsr 8) land 0xff
      and b0 = bg land 0xff in
      let r1 = (fg lsr 16) land 0xff
      and g1 = (fg lsr 8) land 0xff
      and b1 = fg land 0xff in
      for i = 1 to 255 do
        let k = (255 - i) in
        let r = (k * r0 + i * r1) / 255
        and g = (k * g0 + i * g1) / 255
        and b = (k * b0 + i * b1) / 255 in
        table.(i) <- (r lsl 16) + (g lsl 8) + b
      done;
      Hashtbl.add htable col table;
      table;;

let get_glyph_image g col =
  match g.cache with
  | Cached (c, img) when c = col -> img
  | _ ->
      let img =
        try List.assoc col g.img_list
        with Not_found ->
          let gmap = graymap g
          and w = Glyph.width g
          and h = Glyph.height g in
          (* We enforce [h <> 0] and [w <> 0] because
             Caml graphics don't like zero-sized pixmaps. *)
          let dst = Array.make_matrix (max 1 h) (max 1 w) Graphics.transp
          and table = get_color_table col
          and p = ref 0 in
          for i = 0 to h - 1 do
            for j = 0 to w - 1 do
              let gamma_fix c =
                Misc.round (((float c /. 255.0) ** !glyph_gamma) *. 255.0) in
              dst.(i).(j) <- table.(gamma_fix (Char.code gmap.[!p]));
              incr p
            done
          done;
          let img = Graphics.make_image dst in
          g.img_list <- (col, img) :: g.img_list;
          img in
      g.cache <- Cached(col, img);
      img;;

(*** Device manipulation ***)
type 'a rect = { rx : 'a; ry : 'a; rh : 'a; rw : 'a; rd : 'a };;
let nobbox =  { rx = 0; ry = 0; rw = 10; rh = 10; rd = 0 };;
let bbox = ref nobbox;;

let set_bbox bb =
  if not !opened then failwith "Grdev.set_bbox: no window";
  match bb with
  | None ->
      bbox := nobbox;
  | Some (x0, y0, w, h) ->
      (* rd is always zero for bounding box *)
      bbox := {rx = x0; ry = !size_y - y0; rw = w; rh = -h; rd = 0};;

(*** Drawing ***)
let ps_set_color c = 
  if !psused then
  let r, g, b = rgb_of_color c in
  Gs.setrgbcolor r g b;;
  
  
let get_color, set_color =
 let color = ref (get_fgcolor ()) in
 (fun () -> !color),
 (fun c ->
   if not !opened then failwith "Grdev.set_color: no window";
   (*prerr_endline "set_color";*)
   color := c;
   Graphics.set_color c;
   ps_set_color c
 );;

let with_color c f x =
  let cur = get_color () in
  set_color c;
  try let y = f x in set_color cur; y
  with exn -> set_color cur; raise exn

let draw_glyph g x0 y0 =
  if not !opened then failwith "Grdev.draw_glyph: no window";
  let w = Glyph.width g
  and h = Glyph.height g in
  let x = x0 - hoffset g
  and y = !size_y - y0 + voffset g - h in
  if x + w > !xmin && x < !xmax && y + h > !ymin && y < !ymax then begin
    let bg = get_bg_color x y w h in
    let img = get_glyph_image g (bg, get_color ()) in
    sync dvi;
    Graphics.draw_image img x y;
  end;;

let old_fill_rect x y dx dy =
  Graphics.fill_rect x y (dx-1) (dy-1)

let fill_rect x0 y0 w h =
  if not !opened then failwith "Grdev.fill_rect: no window";
  let x = x0
  and y = !size_y - y0 - h in
  let x' = x + w
  and y' = y + h in
  (* clipping *)
  let x = max !xmin x
  and y = max !ymin y
  and x' = min x' !xmax
  and y' = min y' !ymax in
  let w = x' - x
  and h = y' - y in
  if w > 0 && h > 0 then begin
    sync dvi;
    old_fill_rect x y w h;
    add_background_color x y w h (get_color ());
  end;;
(* TODO: implement clipping in the following primitives ? *)

(* Convert a path to Caml Graphics coordinates. *)
let convert_path path =
  let newpath = Array.copy path in
  for i = 0 to Array.length newpath - 1 do
    let (x, y) = path.(i) in
    newpath.(i) <- (x, !size_y - y)
  done;
  newpath;;

let draw_path path ~pensize =
  if not !opened then failwith "Grdev.draw_path: no window";
  let path = convert_path path in
  Graphics.set_line_width pensize;
  Graphics.draw_poly_line path;
  Graphics.set_line_width 1;;

let shade_color shade =
  let c = get_color () in
  let r = 0xFF - (c lsr 16) land 0xFF
  and g = 0xFF - (c lsr 8) land 0xFF
  and b = 0xFF - c land 0xFF in
  let r = 0xFF - Misc.round (shade *. float r)
  and g = 0xFF - Misc.round (shade *. float g)
  and b = 0xFF - Misc.round (shade *. float b) in
  Graphics.rgb r g b;;

let fill_path path ~shade =
  if not !opened then failwith "Grdev.fill_path: no window";
  let path = convert_path path in
  Graphics.set_color (shade_color shade);
  Graphics.fill_poly path;
  Graphics.set_color (get_color ());;

let draw_arc ~x ~y ~rx ~ry ~start:a1 ~stop:a2 ~pensize =
  if not !opened then failwith "Grdev.draw_arc: no window";
  Graphics.set_line_width pensize;
  Graphics.draw_arc x (!size_y - y) rx ry (- a1) (- a2);
  Graphics.set_line_width 1;;

let fill_arc ~x ~y ~rx ~ry ~start:a1 ~stop:a2 ~shade =
  if not !opened then failwith "Grdev.fill_arc: no window";
  Graphics.set_color (shade_color shade);
  Graphics.fill_arc x (!size_y - y) rx ry (- a1) (- a2);
  Graphics.set_color (get_color ());;

let draw_ps_by_gs fname = Gs.draw_file fname;;

let draw_ps file bbox (w, h) x0 y0 =
  if not !opened then failwith "Grdev.draw_ps: no window";
  let x = x0
  and y = !size_y - y0 + h in
  try Drawimage.f file !epstransparent !alpha !blend
        (Some bbox) Drawimage.ScaleAuto !epswithantialiasing (w, h) (x, y - h)
  with
  | Not_found ->
      Misc.warning
        (Printf.sprintf "ps file %S was not found" file)
  | exn ->
      Misc.warning
        (Printf.sprintf "error happened while drawing ps file %S: %s"
           file (Printexc.to_string exn));;

let clean_ps_cache () = Drawimage.clean_cache ();;

(*** HTML interaction ***)

type rectangle_frame_image = {
    north : Graphics.image;
    south : Graphics.image;
    east : Graphics.image;
    west : Graphics.image;
  };;

(* To save a rectangular frame (4 images of 1 pixel width, each one
   for one side of the rectangle). *)
let save_rectangle x y dx dy =
  let x = min x (x + dx) in
  let y = min y (y + dy) in
  let dx = max 1 (abs dx) in
  let dy = max 1 (abs dy) in
  { south = Graphics.get_image x y dx 1;
    west = Graphics.get_image x y 1 dy;
    east = Graphics.get_image (x + dx) y 1 dy;
    north = Graphics.get_image x (y + dy) (succ dx) 1;
  };;

let restore_rectangle r x y dx dy =
  let x = min x (x + dx) in
  let y = min y (y + dy) in
  let dx = max 1 (abs dx) in
  let dy = max 1 (abs dy) in
  Graphics.draw_image r.south x y;
  Graphics.draw_image r.west x y;
  Graphics.draw_image r.east (x + dx) y;
  Graphics.draw_image r.north x (y + dy);;

let draw_rectangle x y dx dy =
  Graphics.moveto x y;
  Graphics.lineto (x + dx) y;
  Graphics.lineto (x + dx) (y + dy);
  Graphics.lineto x (y + dy);
  Graphics.lineto x y;;

let draw_line x y dx dy =
  Graphics.moveto x y;
  Graphics.lineto (x + dx) (y + dy);;

let draw_point x y =
  Graphics.draw_circle x y 3;;

(* Could be improved later on, using quad-tree or similar 2d structure. *)
module type ACTIVE =
  sig
    type 'a active =
        { x : int;
          y : int;
          w : int;
          h : int;
          action : 'a
        }

    type 'a t
    val empty : 'a t
    val add : 'a active -> 'a t -> 'a t
    val same_location : 'a active -> 'b active -> bool
    val find : int -> int -> 'a t -> 'a active
    val find_action : ('a -> bool) -> 'a t -> 'a active
    val find_with_action : ('a -> bool) -> int -> int -> 'a t -> 'a active
    val inside : int -> int -> 'a active -> bool
    val iter : ('a active -> unit) -> 'a t -> unit
  end;;

module A : ACTIVE =
  struct
    type 'a active =
        { x : int;
          y : int;
          w : int;
          h : int;
          action : 'a
        }
    type 'a t = 'a active list
    let empty = []
(*
    let crop z u l = 
      if z >= 0 && z + u <= l then z, u 
      else if z + u < 0 || z > l then 0, 1
      else if z <= 0 && z + u > l then 0, l
      else if z <= 0 then 0, u + z
      else z, l - z 
*)      
    let add a t = 
      (*
        let x, w = crop a.x a.w !size_x in
        let y, h = crop a.y a.h !size_y in
        { x = x; y = y; w = w; h = h; action = a.action } 
      *)
      a :: t
    let inside x y a  =
      a.x <= x && a.y <= y && x <= a.x + a.w && y <= a.y + a.h
    let find_with_action f x y t =
      List.find (fun a -> f a.action && inside x y a) t
    let find x y t = List.find (inside x y) t
    let find_action f l = List.find (fun a -> f a.action) l
    let iter = List.iter
    let same_location a b =
(*
      a.x = b.x && a.y = b.y && a.w = b.w && a.h = b.h
*)
      a.x <= b.x && a.y <= b.y &&
      a.w >= b.x - a.x + b.w && a.h >= b.y - a.y + b.h

  end;;

(* *)
let editing =
  Options.flag false "-edit"
  "  start Active-DVI in edit mode,\
  \n\t (the default is not to start in edit mode).";;

module H =
  struct
    type mode = Over | Click_down | Stick
    type draw_style = Draw | Fill
    type style = Box | Underline | Invisible
    type link = {
      link : string;
      action : (unit -> unit);
      mode : mode;
      style : style;
      color : color option;
      area : (int * int * int) option;
    }
    type tag =
       | Name of string
       | Href of string
       | Advi of link
       | Item of string

    type anchor = {
      tag : tag;
      draw : (int * int * glyph) list
    }

    let anchors = ref A.empty

    let add_corner x y w h s =
      let anchor = { tag = Item s; draw = []; } in
      let a =
        { A.x = x; A.y = y; A.w = w; A.h = h;
          A.action = anchor;
        } in
      anchors := A.add a !anchors

    let add_corners() =
      let w = !size_x / 10 in
      add_corner 0 0 w w "Bottom_left";
      add_corner 0 (!size_y - w) w w "Top_left";
      add_corner (!size_x - w) (!size_y - w) w w "Top_right";
      add_corner (!size_x - w) 0 w w "Bottom_right";
      ()

   let clear () = 
      anchors := A.empty;
      add_corners()

    let string_of_link {link = s} = s

    let string_of_tag = function
      | Name s -> Printf.sprintf "Name %s" s
      | Href s -> Printf.sprintf "Href %s" s
      | Advi l -> Printf.sprintf "Advi %s" (string_of_link l)
      | Item s -> assert false

    (* Draws a rectangle with border width bw if possible. *)
    let frame_rect bw x y w h =
      if bw > 0 && w > bw && h > bw then
        let draw_rect_with_line_width bw x y w h =
          old_fill_rect x y bw h;
          old_fill_rect x y w bw;
          old_fill_rect (x + w - bw) y bw h;
          old_fill_rect x (y + h - bw) w bw in
        draw_rect_with_line_width bw x y w h else
      Graphics.draw_rect x y w h

    let draw_anchor style c bw a =
      Graphics.set_color c;
      begin match style with
      | Box -> frame_rect bw a.A.x a.A.y a.A.w a.A.h
      | Underline -> frame_rect bw a.A.x a.A.y a.A.w bw
      | Invisible -> ()
      end;
      Graphics.set_color (get_color ())

    let make_anchors tag all_draw =
      let make_anchor draw (x, y as _orig) w h voff =
        let anchor = {tag = tag; draw = List.rev draw} in
        let bw =
          match tag with
          | Href _ -> 0
          | Advi _ -> 0
          | Item _ -> 0
          | Name _ -> 0 in
        let y' = y - voff - 1 in
        let h' = h + 2 in
        let a =
          let bw = bw + 1 in
          { A.x = x - bw;
            A.y = (!size_y - y' - h') - bw;
            A.w = w + bw + bw;
            A.h = h' + bw + bw;
            A.action = anchor;
          } in
        anchors := A.add a !anchors;
        match tag with
        | Item _ -> draw_anchor Box href_frame_color 1 a
        | Href _ -> draw_anchor Box href_frame_color 1 a
        | Advi link ->
            draw_anchor
              link.style
              (match link.color with
               | None -> advi_frame_color | Some c -> c)
              1 a
        | _ -> ()
      in
      let rec split draw (x, y as orig) w h voff = function
        | [] -> make_anchor draw orig w h voff
        | (x1, y1, g1 as d) :: rest as all ->
            if x1 + width g1 > x then
              split (d :: draw) orig
                (max w ((x1 - x) + width g1))
                (max h (height g1))
                (max voff (voffset g1)) rest
            else
              begin
                make_anchor draw orig w h voff;
                start all
              end
      and start = function
        | [] -> ()
        | (x, y, g as d) :: rest ->
            split [d] (x, y) (width g) (height g) (voffset g) rest in
      start all_draw

    let add anchor = make_anchors anchor.tag anchor.draw

    let area tag x y w h =
      let anchor = {tag = tag; draw = []} in
      let a = { A.x = x; A.y = y; A.w = w; A.h = h; A.action = anchor} in
      anchors := A.add a !anchors

    let find x y = A.find x y !anchors
    let find_with_tag t x y =
      A.find_with_action (fun x -> t x.tag) x y !anchors

    let find_tag t = A.find_action (fun x -> x.tag = t) !anchors


    type backup =
       | Nil
       | Rect of Graphics.image * anchor A.active *
             (Graphics.image * anchor A.active) list
       | Screen of Graphics.image * anchor A.active * anchor A.t

    let up_to_date act = function
      | Rect (_, a, l) -> A.same_location a act
      | Screen (_, a, _) -> A.same_location a act
      | Nil -> false


    let deemphasize ephemeral emph =
      match emph with
      | Rect (img, act, l) ->
          GraphicsY11.display_mode ephemeral;
          List.iter
            (function img, act -> Graphics.draw_image img act.A.x act.A.y) l;
          Graphics.draw_image img act.A.x act.A.y;
          Busy.set
            (if !editing then Busy.Selection else Busy.Free);
          GraphicsY11.display_mode false
      | Screen (img, act, all_anchors) ->
          GraphicsY11.display_mode true;
          anchors := all_anchors;
          Gs.flush ();
          (* long delay to be safe *)
          ignore (sleep_watch false false 0.1);
          Graphics.draw_image img 0 0;
          Busy.set
            (if !editing then Busy.Selection else Busy.Free);
          GraphicsY11.flush ();
          GraphicsY11.display_mode false
      | Nil -> 
          ()

    let emphasize fill c act =
      let img = Graphics.get_image act.A.x act.A.y act.A.w act.A.h in
      Graphics.set_color c;
      GraphicsY11.display_mode true;
      begin match fill with
      | Fill -> old_fill_rect act.A.x act.A.y act.A.w act.A.h
      | Draw -> Graphics.draw_rect (act.A.x + 1) (act.A.y + 1)
                  (act.A.w - 2) (act.A.h - 2)
      end;
      Graphics.set_color (get_color ());
      push_bg_color c;
      List.iter (function x, y, g -> draw_glyph g x y) act.A.action.draw;
      pop_bg_color ();
      GraphicsY11.set_cursor GraphicsY11.Cursor_hand2;
      GraphicsY11.display_mode false;
      Rect (img, act, [])

    let save_screen_exec act a =
      Gs.flush ();
      (* get image take the image from the backing store *)
      let img = Graphics.get_image 0 0 !size_x !size_y in
      GraphicsY11.sync ();
      (* wait until all events have been processed, flush should suffice *)
      (*
         Graphics.set_color (Graphics.point_color 0 0);
         (* it seems that the image is saved ``lazily'' and further instruction
            could be capture in the image *)
         sleep_watch false false 0.05;
       *)
      let all_anchors = !anchors in
      a ();
      flush_last ();
      GraphicsY11.synchronize ();
      Launch.launch_embedded_apps ();
      Screen (img, act, all_anchors)

    let nosave_screen_exec act a =
      a ();
      (* is the following needed? *)
      flush_last ();
      GraphicsY11.synchronize ();
      Launch.launch_embedded_apps ();
      Nil

    let light t =
      try
        match t with
        | Name n ->
            let t = Name (Misc.get_suffix "#" n) in
            emphasize Fill (* do not put Draw here -Didier *)
              name_emphasize_color (find_tag t)
        | _ -> Nil
      with
      | Not_found | Misc.Match -> Nil

    let flashlight t =
      deemphasize false (light t)

    let emphasize_and_flash color act =
      let fill, color =
        match act.A.action.tag with
        | Href s when  has_prefix "#/page." s -> Draw, rect_emphasize_color
        | _ -> Fill, color in
      let emph = emphasize fill color act in
      let m =
        match act.A.action.tag with
        | Href n -> light (Name n)
        | _ -> Nil in
      match emph, m with
      | Rect (img, a, l), Rect (img', a', l') ->
          Rect (img, a, (img', a') :: (l' @ l))
      | x, _ -> x

  end;;

module E =
  struct
    type info = {
        comm : string; name : string;
        first : (string * string) option; 
        line : string; file : string;
        origin : float rect; action : bool rect;
        xunit : float; yunit : float;
      }
    type figure = { rect : int rect; info : info; }
    type action = Move of int * int | Resize of bool * int * int

    let figures : figure list ref = ref []
    let screen = ref None
    let switch_edit_mode () =
      editing := not !editing;
      Busy.set
       (if !editing then Busy.Selection else Busy.Free)

    let clear () = figures := []; screen := None
        (*
           let save_screen cont =
           screen := Graphics.get_image 0 0 !size_x !size_y
           let restore_screen () = ()
         *)

    let add rect info =
      let r = { rect with ry = !size_y - rect.ry; } in
      figures := { rect = r; info = info} :: !figures;
      if !editing then
        begin
          Graphics.set_color Graphics.blue;
          draw_rectangle r.rx (r.ry - r.rd) r.rw (r.rh + r.rd);
          draw_line r.rx r.ry r.rw 0; 
          let cvx z = truncate (z *. info.xunit) in
          let cvy z = truncate (z *. info.yunit) in
          let ox = cvx info.origin.rx in
          let oy = cvy info.origin.ry in
          let x0 = r.rx - ox in
          let y0 = r.ry - oy in
          Graphics.set_color Graphics.green;
          draw_line x0 y0 ox oy;
          draw_point x0 y0;
          Graphics.set_color (get_color ());
        end

    let inside x y p  =
      let a = p.rect in
      let ax = a.rx in
      let aw = a.rw in
      let ay = a.ry - a.rd in
      let ah = a.rh + a.rd in
      (if aw > 0 then ax <= x && x <= ax + aw
      else ax + aw <= x && x <= ax) &&
      (if ah > 0 then ay <= y && y <= ay + ah
      else ay + ah <= y && y <= ay)
    let find x y = List.find (inside x y) !figures

    let tostring p a =
      (* should memorize the origin *)
      let deltax z dz =
        if dz = 0 then "*"
        else Printf.sprintf "%.4f" (z +. (float dz  /. p.info.xunit)) in
      let deltay z dz =
        if dz = 0 then "*"
        else Printf.sprintf "%.4f" (z +. (float dz  /. p.info.yunit)) in
      let origin = p.info.origin in
      let first =
        match p.info.first with
        | None -> ""
        | Some (f, v) -> Printf.sprintf "%s=%s" f v
      in
      let action, dx, dy =
        match a with
        | Move (dx, dy) ->
            "moveto", deltax origin.rx dx, deltay origin.ry dy
        | Resize (true, dx, dy) ->
            "resizetop", deltax origin.rw dx, deltay origin.rh dy
        | Resize (false, dx, dy) ->
            "resizebot", deltax origin.rw dx, deltay origin.rd dy
      in
      Printf.sprintf "<edit %s %s[%s] #%s @%s %s %s,%s>"
        p.info.comm p.info.name first p.info.line p.info.file action dx dy

    let editing () = !editing

  end;;

(*** Clearing device ***)

module Symbol = Symbol.Make (Glyph);;

let cut s =
  (* print_string s; print_newline (); *)
  (* cut does not work yet *)
  GraphicsY11.cut s;
;;

type window_geometry = string;;

let open_dev geom =
  if !opened then Graphics.close_graph ();
  Graphics.set_window_title !title;
  set_initial_window_device_geometry geom;
  Graphics.open_graph geom;
  opened := true;

  (* We disable Graphics's event retrieving *)
  GraphicsY11.init ();
  Timeout.init ();
  (* Fill the event queue *)
  Timeout.repeat 0.02 (* was 0.25s!! *) GraphicsY11.retrieve_events;
  (* Watch if the DVI file is modified *)
  if !watch_file_interval > 0 then
    Timeout.repeat (float !watch_file_interval) watch_file_check; 

  update_device_geometry ();
  GraphicsY11.set_remember_mode true;
  GraphicsY11.display_mode (Global_options.get_global_display_mode ());
  set_color (get_fgcolor ());
  !size_x, !size_y;;

let close_dev () =
  if !opened then begin
    Embed.kill_all_embedded_apps ();
    Graphics.close_graph ();
  end;
  opened := false;;

let clear_dev () =
  if not !opened then failwith "Grdev.clear_dev: no window";
  Embed.kill_ephemeral_apps ();
  Launch.unmap_persistent_apps ();
  Misc.debug_stop "subwindows of persistent apps unmapped";
  GraphicsY11.display_mode (Global_options.get_global_display_mode ());
  Graphics.clear_graph ();
  Misc.debug_stop "graphics cleared";
  H.clear ();
  E.clear ();
  bg_colors := [];
  background_colors := [];
  Symbol.clear_global_display_set ();
  (* update graphics size information *)
  update_device_geometry ();
  (* draw background *)
  draw_bkgd ();;

let resize_dev w h =
  if not !opened then failwith "Grdev.resize_dev: no window";
  Embed.kill_all_embedded_apps ();
  Graphics.resize_window w h;;

(*** Events ***)

type status = GraphicsY11.status = {
    mouse_x : int;
    mouse_y : int;
    button : bool;
    keypressed : bool;
    key : char;
    modifiers : int;
  };;

type area =
   | Bottom_right | Bottom_left | Top_right | Top_left | Middle;;
type button =
   | Button1 | Button2 | Button3 | Button4 | Button5 | NoButton 

type event =
   | Resized of int * int
   | Refreshed
   | Key of char
   | Stdin of char
   | Move of int * int
   | Edit of E.figure * E.action
   | Region of int * int * int * int
   | Selection of string
   | Position of int * int
   | Href of string
   | Advi of string * (unit -> unit)
   | Click of area * button * int * int
   | Nil;;

type option_event =
   | Final of event
   | Raw of status;;

let all_events = [
  GraphicsY11.Button_down;
  GraphicsY11.Button_up;
  GraphicsY11.Mouse_motion;
  GraphicsY11.Key_pressed;
];;

let button_up_motion = [
  GraphicsY11.Button_up;
  GraphicsY11.Mouse_motion;
];;

let button_up = [
  GraphicsY11.Button_up;
];;

let events = ref [];;

let event_waiting () = !events <> [];;

let rec pop_event () =
  match !events with
  | [] -> assert false
  | h :: t -> events := t; h;;

let push_event e = events := e :: !events;;
let push_back_event = push_event;;

let push_key_event c m =
  let status = {
    mouse_x = 0; mouse_y = 0;
    button = false;
    keypressed = true;
    key = c;
    modifiers = m;
  } in
  Misc.debug_endline (Printf.sprintf "Pushing the key %C" c);
  push_event status;;

let push_char_event c =
  Misc.debug_endline (Printf.sprintf "Pushing the key %C" c);
  match c with
  | '' .. '' as c -> push_key_event c GraphicsY11.control
  | c -> push_key_event c GraphicsY11.nomod;;

let push_mouse_event mx my b =
  let status = {
    mouse_x = mx; mouse_y = my;
    button = b;
    keypressed = false;
    key = '\000';
    modifiers = GraphicsY11.nomod;
  } in
  push_event status;;

let push_full_event c m kp mx my b =
  let status = {
    mouse_x = mx; mouse_y = my;
    button = b;
    keypressed = kp;
    key = c;
    modifiers = m;
  } in
  push_event status;;

(* Setting the forwards in Misc. *)
Misc.set_forward_push_char_event push_char_event;;
Misc.set_forward_push_key_event push_key_event;;
Misc.set_forward_push_mouse_event push_mouse_event;;
Misc.set_forward_push_full_event push_full_event;;

let reposition ~x ~y ~w ~h ~screen =
  Gs.flush ();
  Gs.kill ();
  (* screen is protected to 0 in the C layer *)
  GraphicsY11.reposition x y w h screen;
  update_device_geometry ();
  !size_x, !size_y;;

let resized () =
  let x = Graphics.size_x () and y = Graphics.size_y () in
  let b = x <> !size_x || y <> !size_y in
  if b then
    begin
      size_x := x;
      size_y := y;
      Gs.kill ();
      Some (x, y)
    end
  else None;;

let rec wait_signal_event events =
  match resized (), !usr1_status,  event_waiting (), !usr2_status with
  | Some (x, y), _, _, _ -> Final (Resized (x, y))
  | _, true, _, _ -> clear_usr1 (); Final (Refreshed)
  | _, _, true, _ -> Raw (pop_event ())
  | _, _, _, true -> Final (Stdin (get_input()))
  | _, _, _, _ -> 
      let rec wait () =
        try
          waiting := true;
          let ev = GraphicsY11.wait_next_event events in
          waiting := false;
          match resized () with
          | Some (x, y) ->
              push_back_event ev;
              Final (Resized (x, y))
          | None ->
              Raw ev
        with
        | Usr2 ->
            waiting := false;
            Final (Stdin (get_input()))
        | Watch_file ->
            waiting := false;
            if List.mem GraphicsY11.Key_pressed events then Final Nil
            else wait ()
        | exn ->
            waiting := false;
            raise exn in
      wait ();;

let wait_select_rectangle x y =
  let rec select dx dy =
    let buf = save_rectangle x y dx dy in
    draw_rectangle x y dx dy;
    let ev = wait_signal_event button_up_motion in
    restore_rectangle buf x y dx dy;
    match ev with
    | Raw e ->
        let dx' = e.GraphicsY11.mouse_x - x in
        let dy' = e.GraphicsY11.mouse_y - y in
        if e.GraphicsY11.button
        then select dx' dy'
        else Final (Region (x, y, dx', 0 - dy'))
    | x -> x in
  set_color (get_fgcolor ());
  GraphicsY11.display_mode true;
  Busy.set Busy.Selection;
  let restore () =
    GraphicsY11.display_mode false;
    set_color (get_color ());
    Busy.set
      (if !editing then Busy.Selection else Busy.Free) in
  try
    let e = select 0 0 in
    restore ();
    e
  with
  | exn -> restore (); raise exn;;

let wait_select_button_up m x y =
  let draw_color b =
    let draw s c x y =
      set_color (if b then c else cut_emphasize_color);
      draw_glyph s x y in
    Symbol.apply draw in
  let rec select r =
    let ev = wait_signal_event button_up_motion in
    match ev with
    | Raw e ->
        let x' = e.GraphicsY11.mouse_x in
        let y' = e.GraphicsY11.mouse_y in
        let r' = Symbol.new_region r x' (!size_y - y') in
        Symbol.iter_regions (draw_color true) (draw_color false) r r';
        if e.GraphicsY11.button then select r' else
        let m = GraphicsY11.get_modifiers () in
        if m land GraphicsY11.shift = 0 then begin
          Symbol.iter_region (draw_color true) r';
          Final Nil
        end else
          Final (Selection (Symbol.region_to_ascii r'))
    | x -> x in
  let color = (get_color ()) in
  GraphicsY11.synchronize ();
  Graphics.remember_mode false;
  GraphicsY11.display_mode true;
  Busy.temp_set Busy.Selection;
  let restore () =
    Graphics.remember_mode true;
    GraphicsY11.display_mode false;
    set_color color;
    Busy.restore_cursor () in
  try
    let e =
      if m land GraphicsY11.button2 = 0 then
        let r = Symbol.position x (!size_y - y) in
        select r
      else
        match Symbol.word x (!size_y - y) with
        | Some (r, w) ->
            Symbol.iter_region (draw_color false) r;
            Final (Selection w)
        | None -> Final Nil in
    restore ();
    e
  with
  | exn ->
      restore ();
      match exn with
      | Not_found -> Final Nil
      | _ -> raise exn;;

(* Graphical transformations to apply to rectangles. *)
let transform_rect action r dx dy =
  { rx = if action.rx then r.rx + dx else r.rx;
    ry = if action.ry then r.ry + dy else r.ry;
    rw = if action.rw then r.rw + dx else r.rw;
    rh = if action.rh then r.rh + dy else r.rh;
    rd = if action.rd then r.rd - dy else r.rd;
  }

let transform_cursor action =
  if action.rx || action.ry then Busy.Move
  else match action.rw, action.rh, action.rd with
  | true, false, false -> Busy.Resize_w
  | true, _, _ -> Busy.Resize
  | _, true, _ -> Busy.Resize_h
  | _, _, true -> Busy.Resize_d
  | _, _, _ -> Busy.Move


(* ?? *)
let filter trans event dx dy =
  let r = trans { rx = 0; ry = 0; rw = 0; rh = 0; rd = 0} dx dy in
  event (r.rx + r.rw) (0 - r.ry - r.rh - r.rd);;

let wait_move_button_up rect trans_type event x y =
  let trans = transform_rect trans_type in
  let cursor = transform_cursor trans_type in
  let rec move dx dy =
    let r = trans rect dx dy in
    let rx = r.rx in let ry = r.ry - r.rd in
    let rw = r.rw in let rh = r.rh + r.rd in
    let buf = save_rectangle rx ry rw rh in
    draw_rectangle rx ry rw rh;
    let ev = wait_signal_event button_up_motion in
    restore_rectangle buf rx ry rw rh;
    match ev with
    | Raw e ->
        let dx' = e.GraphicsY11.mouse_x - x in
        let dy' = e.GraphicsY11.mouse_y - y in
        if e.GraphicsY11.button then move dx' dy'
        else
          let m = GraphicsY11.get_modifiers() in
          (* to give up some motion, should rather be ^C *)
          if m land GraphicsY11.mod1 <> 0 then
            Final Nil
          else
            Final (filter trans event dx' (0 - dy'))
    | z -> z in
  let color = get_color () in
  set_color (get_fgcolor ());
  Busy.temp_set cursor;
  GraphicsY11.display_mode true;
  let restore () =
    GraphicsY11.display_mode false;
    set_color color;
    Busy.restore_cursor () in
  try let e = move 0 0 in restore (); e with
  | exn -> restore (); raise exn;;

let near x x' = abs (x - x') < !size_x / 4;;
let close x x' = abs (x - x') < !size_x / 10;;

let mouse_area near x y =
  if near x 0 then
    if near y 0 then Bottom_left else
    if near y !size_y then Top_left
    else Middle else
  if near x !size_x then
    if near y 0 then Bottom_right else
    if near y !size_y then Top_right
    else Middle
  else Middle;;

let modifier m b = m land b <> 0;;

module G = GraphicsY11;;

let pressed m b = m land b <> 0
let button123 = G.button1 lor G.button2 lor G.button3

let get_button b =
  if pressed b G.button1 then Button1 else
  if pressed b G.button2 then Button2 else
  if pressed b G.button3 then Button3 else
  if pressed b G.button4 then Button4 else
  if pressed b G.button5 then Button5 else
  NoButton
;;

let shift_or_control = G.shift lor G.control

let button_pressed m b = get_button m = b

let wait_button_up m x y =
  let wait_position () =
    match wait_signal_event button_up with
    | Raw e ->
        let m = e.modifiers in
        let () = Misc.debug_endline (Printf.sprintf "wait_position %x" m) in
        if modifier m G.button1 && not (modifier m shift_or_control) then begin
          match mouse_area close x y with
          | Middle -> Final (Position (x, !size_y - y))
          | c -> Final (Click (c, get_button e.modifiers, x, !size_y - y))
        end
        else
          let c = mouse_area near x y  in
          Final (Click (c, get_button e.modifiers, x, !size_y - y))
    | x -> x in
  if modifier m G.button1 && not (modifier m shift_or_control) then
    wait_position () else
  if modifier m G.control || modifier m G.shift then begin
    try
      let () = Misc.debug_endline (Printf.sprintf "Control?") in
      let p = E.find x y in
      let rect = p.E.rect in
      let info = p.E.info in
      let action = info.E.action in
      if button_pressed m Button2 && (action.rx || action.ry) then
        let event dx dy = Edit (p, E.Move (dx, dy)) in
        let action = { action with rw = false; rh = false; rd = false} in
        wait_move_button_up rect action event x y
      else if (button_pressed m Button3 || button_pressed m Button1)
          && (action.rh || action.rd || action.rw) then
        let b = modifier m G.shift in
        let action_h = action.rh && (not action.rd || not b) in
        let action_d = action.rd && (not action.rh || b) in
        let event dx dy = Edit (p, E.Resize (action_h, dx, dy)) in
        let action =
          { action with rx = false; ry = false;
            rh = action_h; rd = action_d } in
        wait_move_button_up rect action event x y
      else Final Nil
    with
    | Not_found ->
        let () = Misc.debug_endline (Printf.sprintf "Control not found?") in
        if modifier m G.control then
          let event dx dy = Move (dx, 0-dy) in
          let move =
            { rx = true; ry = true; rw = false; rh = false; rd = false } in
          wait_move_button_up !bbox move event x y
        else wait_position ()
  end else
    if modifier m G.shift && not (button_pressed m Button1)
    then wait_select_button_up m x y
    else wait_position ();;

(* Should map events of keypresses using control to control characters. *)
let find_key ev =
  let c = ev.GraphicsY11.key in
  let k =
    if ev.modifiers != G.control then c else
    let cc = 1 + int_of_char c - int_of_char 'A' in
    if cc >= 1 then char_of_int cc else c in
  Key k;;

let active_tag z = match z with H.Name _ -> false | _ -> true;;

let wait_event () =
  (* We reached a pause. Now we can reset the sleep break *)
  clear_sleep ();
  let rec event emph b =
    let send ev = H.deemphasize true emph; ev in
    let rescan () = H.deemphasize true emph; event H.Nil false in
    match wait_signal_event all_events with
    | Final e -> send e
    | Raw ev ->
        if ev.G.keypressed then send (find_key ev) else
        try match H.find_with_tag active_tag ev.mouse_x ev.mouse_y with
        | {A.action = {H.tag = H.Href h; H.draw = d}} as act ->
            if ev.button then
              let _ev' = GraphicsY11.wait_next_event button_up in
              if  pressed ev.modifiers button123
              then send (Href h)
              else send (Click (Middle, get_button _ev'.modifiers,
                                _ev'.mouse_x, _ev'.mouse_y))
            else if H.up_to_date act emph then event emph b else begin
                H.deemphasize true emph;
                event (H.emphasize_and_flash href_emphasize_color act) b end
        | {A.action = {H.tag = H.Item s; H.draw = d}} as act ->
            if ev.button then
              let _ev' = GraphicsY11.wait_next_event button_up in
              let area =
                match s with 
                | "Bottom_left" -> Bottom_left
                | "Bottom_right" -> Bottom_right
                | "Top_left" -> Top_left
                | "Top_right" -> Top_right 
                | _ -> Middle
              in
              (* This would not work with rolling button *)
              send (Click (area, 
                           get_button _ev'.modifiers, 
                           _ev'.mouse_x, 
                           _ev'.mouse_y))
            else
              if H.up_to_date act emph then event emph b else begin
                H.deemphasize true emph;
                event (H.emphasize_and_flash href_emphasize_color act) b end
        | {A.action =
           {H.tag = H.Advi {H.link = s; H.action = a; H.mode = H.Over};
            H.draw = d}} as act ->
              if H.up_to_date act emph then event emph b else begin
                H.deemphasize true emph;
                event (H.save_screen_exec act a) b end
        | {A.action =
           {H.tag = H.Advi {H.link = s; H.action = a; H.mode = H.Click_down};
            H.draw = d}} as act ->
              if ev.button && not b then begin
                H.deemphasize true emph;
                event (H.save_screen_exec act a) true end else
                if ev.button then event emph b else
                if H.up_to_date act emph then event emph b else begin
                  H.deemphasize true emph;
                  event (H.emphasize_and_flash href_emphasize_color act) b end
        | {A.action =
           {H.tag = H.Advi {H.link = s; H.action = a; H.mode = H.Stick};
            H.draw = d}} as act ->
              if ev.button && not b then begin
                H.deemphasize true emph;
                event (H.nosave_screen_exec act a) true end else
                if ev.button then event emph b else
                if H.up_to_date act emph then event emph b else begin
                  H.deemphasize true emph;
                  event (H.emphasize_and_flash href_emphasize_color act) b end
        | _ -> rescan ()
        with Not_found ->
          if ev.button then
            let m = ev.modifiers in
            match wait_button_up m ev.mouse_x ev.mouse_y with
            | Final (Region (x, y, dx, dy) as e) -> send e
            | Final (Selection s as e) -> send e
            | Final (Position (x, y) as e) -> send e
            | Final (Move (dx, dy) as e) -> send e
            | Final (Click (_, _, _, _) as e) -> send e
            | Final (Edit (_, _) as e) -> send e
            | Final Nil -> send Nil
            | Final
                (Resized (_, _) | Refreshed |
                Key _ | Stdin _ | Href _ | Advi (_, _) as e) ->
                  push_back_event ev;
                  send e
            | Raw _ -> rescan ()
          else
            let m = GraphicsY11.get_modifiers () in
            if not (modifier m G.shift)
            then Busy.temp_set Busy.Selection
            else Busy.restore_cursor ();
            rescan () in
  event H.Nil false;;

(* To be changed *)
exception GS = Gs.Terminated;;

let resized () =
  Graphics.size_x () <> !size_x || Graphics.size_y () <> !size_y;;

let continue () =
  if
    resized () || (*  !usr1_status || *)
    GraphicsY11.key_pressed () ||
    (* (\* button 4 and 5 down and up is on the queue of events *\) *)
    (* preemptive_buttons() || *)
    (* Some button is currently pressed *)
    GraphicsY11.button_pressed () ||
    !usr2_status (* input from stdin *)
  then
    begin Gs.flush (); raise Stop end;;

(* Calling GS *)

let current_pos () =
  if syncing() && not !last_is_dvi then flush_ps ();
  let x, y = Gs.current_point() in
  x, !size_y - y;;

let clearps () =
  last_is_dvi := true;
  psused := false;;

let newpage x y z t w =
  Gs.newpage x y z t w;
  Gs.flush ();
  last_is_dvi := false;
  psused := false;;

let add_headers l =
  Gs.add_headers l;;

let exec_ps s x0 y0 =
(*prerr_endline (Printf.sprintf "Calling exec_ps with %s" s);*)
  sync ps;
  if not !opened then failwith "Grdev.exec_ps: no window";
  Gs.draw s x0 y0;;

let embed_app app_command app_mode app_name width_pixel height_pixel x y =
  Embed.embed_app
   app_command app_mode app_name width_pixel height_pixel x (!size_y - y);;

let help_screen screen_name =
  ignore
    (Launch.fork_me
      (Printf.sprintf "-g %s" (get_initial_window_device_geometry ()))
      screen_name);;

let wait_button_up () =
  if GraphicsY11.button_down ()
  then ignore (GraphicsY11.wait_next_event [GraphicsY11.Button_up]);;
