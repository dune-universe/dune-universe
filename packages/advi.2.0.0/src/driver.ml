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

let active =
  Options.flag true "-passive"
  "  cancel all Active-DVI effects,\
  \n\t (the default is to play all effects).";;
let toggle_active () = active := not !active;;

let with_active b f x =
  let restore_delays () =
    if !active then Transimpl.sleep := (fun _ -> false) in
  let a = !active in
  try
    let v = active := b; f x in
    active := a; restore_delays (); v with
  | exc -> active := a; restore_delays (); raise exc;;

(* Number of steps before checking for user interruptions *)
let checkpoint_frequency = 10;;

(*** Some utilities for specials ***)

let split_string s start =
  Misc.split_string s (function ' ' -> true | _ -> false) start;;

(* "hello world" is one word *)
let rec split_string_quoted s start =
  let len = String.length s
  and i = ref start in
  (* find a space *)
  while !i < len && s.[!i] = ' ' do incr i done;
  if !i >= len then [] else begin
    let i0 = !i in
    while !i < len && s.[!i] <> ' ' do
      if s.[!i] = '"' (* '"' *) then begin
        incr i;
        while !i < len && s.[!i] <> '"' do incr i done;
        if !i >= len || s.[!i] <> '"' then
          failwith ("parse error (split_string_quoted): " ^ s);
        incr i
      end else incr i
    done;
    let i1 = !i in
    String.sub s i0 (i1 - i0) :: split_string_quoted s i1
  end;;

(* "\"hello world\"" -> "hello world" *)
let unquote s =
  let len = String.length s in
  if len = 0 then s else
  let b = if s.[0] = '"' then 1 else 0 in
  if len - b = 0 then "" else
  let e = if s.[len - 1] = '"' then 1 else 0 in
  let len = len - b - e in
  if len = 0 then "" else
  String.sub s b len;;

let split_record s =
  let tokens = split_string_quoted s 0 in
  List.map (fun token ->
    try
      let i = String.index token '=' in
      String.sub token 0 i,
      String.sub token (i + 1) (String.length token - i - 1)
    with
    | Not_found -> token, "") tokens;;

module Dev = Grdev;;
module Symbol = Dev.Symbol;;
module DFont = Devfont.Make(Dev);;

let base_dpi = 600;;

(*** Cooked fonts ***)

exception Pause;;
exception Wait of float;;

type cooked_font = {
    name : string;
    ratio : float;
    mtable : (int * int) Table.t;
    mutable gtables : (int * Dev.glyph Table.t) list
  };;

let dummy_mtable = Table.make (fun _ -> raise Not_found);;
let dummy_gtable = Table.make (fun _ -> raise Not_found);;
let dummy_font =
  { name = "--nofont--"; ratio = 1.0; mtable = dummy_mtable; gtables = [] };;

let cook_font fdef dvi_res =
  let name = fdef.Dvicommands.name
  and sf = fdef.Dvicommands.scale_factor
  and ds = fdef.Dvicommands.design_size in
  let ratio = float sf /. float ds in
  let mtable =
    try DFont.find_metrics name (dvi_res *. ratio)
    with Not_found -> dummy_mtable in
  { name = name;
    ratio = ratio;
    mtable = mtable;
    gtables = [] };;

let get_gtable cfont sdpi =
  try List.assoc sdpi cfont.gtables
  with Not_found ->
    let dpi = ldexp (float sdpi) (-16) in
    let table =
      try DFont.find_glyphs cfont.name (dpi *. cfont.ratio)
      with Not_found -> dummy_gtable in
    cfont.gtables <- (sdpi, table) :: cfont.gtables;
    table;;

(*** Cooked DVI's ***)

type cooked_dvi = {
    base_dvi : Cdvi.t;
    dvi_res : float;
    font_table : cooked_font Table.t
  };;

let base_dpi = 600 
let prefetch_fonts dvi = 
  let font_map = dvi.Cdvi.font_map in
  let cfont n = let f = List.assoc n font_map in f.Dvicommands.name in
  let fontnames = List.map (fun (n, _) -> (cfont n)) font_map in
  Search.prefetch fontnames base_dpi
;;

let cook_dvi dvi =
  let dvi_res = 72.27 in
  let build n =
    cook_font (List.assoc n dvi.Cdvi.font_map) dvi_res in
  let () = prefetch_fonts dvi in
  { base_dvi = dvi;
    dvi_res = dvi_res;
    font_table = Table.make build }
;;

(*** The rendering state ***)

type reg_set = {
    reg_h : int;
    reg_v : int;
    reg_w : int;
    reg_x : int;
    reg_y : int;
    reg_z : int;
  };;

type state = {
    cdvi : cooked_dvi;
    sdpi : int;
    conv : float;
    x_origin : int;
    y_origin : int;
    (* Current font attributes *)
    mutable cur_font : cooked_font;
    mutable cur_mtable : (int * int) Table.t;
    mutable cur_gtable : Dev.glyph Table.t;
    (* Registers *)
    mutable h : int;
    mutable v : int;
    mutable w : int;
    mutable x : int;
    mutable y : int;
    mutable z : int;
    mutable put : (int * int) list;
    (* Register stack *)
    mutable stack : reg_set list;
    (* Color & Color stack *)
    mutable color : Dvicolor.color;
    mutable color_stack : Dvicolor.color list;
    (* Other attributes *)
    mutable alpha : Drawimage.alpha;
    mutable alpha_stack : Drawimage.alpha list;
    mutable blend : Drawimage.blend;
    mutable blend_stack : Drawimage.blend list;
    mutable epstransparent : bool;
    mutable epstransparent_stack : bool list;
    mutable epsbygs : bool;
    mutable epsbygs_stack : bool list;
    mutable epswithantialiasing : bool;
    mutable epswithantialiasing_stack : bool list;
    mutable direction : Transitions.direction option;
    mutable transition : Transitions.t;
    mutable transition_stack : Transitions.t list;
    (* TPIC specials state *)
    mutable tpic_pensize : float;
    mutable tpic_path : (float * float) list;
    mutable tpic_shading : float;
    (* PS specials page state *)
    mutable status : Cdvi.known_status;
    mutable headers : (bool * string) list;
    mutable html : (Dev.H.tag * (int * int * Dev.glyph) list ref) list;
    mutable checkpoint : int;
  };;

type proc_unit = {
    escaped_register : reg_set;
    escaped_stack : reg_set list;
    escaped_cur_font : cooked_font;
    escaped_cur_mtable : (int * int) Table.t;
    escaped_cur_gtable : Dev.glyph Table.t;
    mutable escaped_commands : Dvicommands.command list
  };;

let procs = Hashtbl.create 107;;
type recording = { tag : string; unit : proc_unit}
let current_recording_proc = ref [];;

let visible = ref true;;
let is_recording () = !current_recording_proc <> [];;

(*** Rendering primitives ***)

let last_height = ref 0;;
let clear_symbols () = last_height := 2;;

let add_char st x y code glyph =
  let g : Symbol.g =
    { Symbol.fontname = st.cur_font.name;
      Symbol.fontratio = st.cur_font.ratio;
      Symbol.glyph = glyph
    } in
  last_height := (Dev.get_glyph glyph).Glyph.voffset;
  let s : Symbol.symbol = Symbol.Glyph g in
  Symbol.add_to_global_display_set st.color x y code s;;

let add_line st (line, file) =
  let x = st.x_origin + Misc.round (st.conv *. float st.h)
  and y = st.y_origin + Misc.round (st.conv *. float st.v) in
  Symbol.add_to_global_display_set st.color x y 0
    (Symbol.Line (line, file));;

let add_blank nn st width =
  let x = st.x_origin + Misc.round (st.conv *. float st.h)
  and y = st.y_origin + Misc.round (st.conv *. float st.v)
  and w = Misc.round (st.conv *. float width) in
  Symbol.add_to_global_display_set st.color x y nn
    (Symbol.Space (w, !last_height));;

let add_rule st x y w h =
  Symbol.add_to_global_display_set st.color x y 0
    (Symbol.Rule (w, h));;

let get_register_set st =
  { reg_h = st.h; reg_v = st.v;
    reg_w = st.w; reg_x = st.x;
    reg_y = st.y; reg_z = st.z };;

let set_register_set st rset =
  st.h <- rset.reg_h;
  st.v <- rset.reg_v;
  st.w <- rset.reg_w;
  st.x <- rset.reg_x;
  st.y <- rset.reg_y;
  st.z <- rset.reg_z;;

let push st =
  st.stack <- (get_register_set st) :: st.stack;;

let pop st =
  match st.stack with
  | [] -> ()
  | rset :: rest ->
      set_register_set st rset;
      st.stack <- rest;;

let color_push st col =
  st.color_stack <- st.color :: st.color_stack;
  st.color <- col;
  if !visible then Dev.set_color col;;

let color_pop st =
  match st.color_stack with
  | [] -> ()
  | col :: rest ->
      st.color <- col;
      if !visible then Dev.set_color col;
      st.color_stack <- rest;;

let alpha_push st v =
  st.alpha_stack <- st.alpha :: st.alpha_stack;
  st.alpha <- v;
  if !visible then Dev.set_alpha v;;

let alpha_pop st =
  match st.alpha_stack with
  | [] -> ()
  | v :: rest ->
      st.alpha <- v;
      if !visible then Dev.set_alpha v;
      st.alpha_stack <- rest;;

let blend_push st v =
  st.blend_stack <- st.blend :: st.blend_stack;
  st.blend <- v;
  if !visible then Dev.set_blend v;;

let blend_pop st =
  match st.blend_stack with
  | [] -> ()
  | v :: rest ->
      st.blend <- v;
      if !visible then Dev.set_blend v;
      st.blend_stack <- rest;;

let epstransparent_push st v =
  st.epstransparent_stack <- st.epstransparent :: st.epstransparent_stack;
  st.epstransparent <- v;
  if !visible then Dev.set_epstransparent v;;

let epstransparent_pop st =
  match st.epstransparent_stack with
  | [] -> ()
  | v :: rest ->
      st.epstransparent <- v;
      if !visible then Dev.set_epstransparent v;
      st.epstransparent_stack <- rest;;

let epsbygs_push st v =
  st.epsbygs_stack <- st.epsbygs :: st.epsbygs_stack;
  st.epsbygs <- v;
  if !visible then Dev.set_epsbygs v;;

let epsbygs_pop st =
  match st.epsbygs_stack with
  | [] -> ()
  | v :: rest ->
      st.epsbygs <- v;
      if !visible then Dev.set_epsbygs v;
      st.epsbygs_stack <- rest;;

let epswithantialiasing_push st v =
  st.epswithantialiasing_stack <-
    st.epswithantialiasing :: st.epswithantialiasing_stack;
  st.epswithantialiasing <- v;
  if !visible then Dev.set_epswithantialiasing v;;

let epswithantialiasing_pop st =
  match st.epswithantialiasing_stack with
  | [] -> ()
  | v :: rest ->
      st.epswithantialiasing <- v;
      if !visible then Dev.set_epswithantialiasing v;
      st.epswithantialiasing_stack <- rest;;

let transition_push st v =
  st.transition <- v;
  if !visible then Dev.set_transition v;;

let fnt st n =
  let (mtable, gtable, cfont) =
    try
      let cfont = Table.get st.cdvi.font_table n in
      (cfont.mtable, get_gtable cfont st.sdpi, cfont)
    with Not_found -> (dummy_mtable, dummy_gtable, dummy_font) in
  st.cur_mtable <- mtable;
  st.cur_gtable <- gtable;
  st.cur_font <- cfont;;

let put st code =
  try
    let x = st.x_origin + Misc.round (st.conv *. float st.h)
    and y = st.y_origin + Misc.round (st.conv *. float st.v)
    and glyph = Table.get st.cur_gtable code in
    if !visible then
      begin
        begin match st.html with
        | (tag, draw) :: _ -> draw := (x, y, glyph) :: !draw
        | [] -> ()
        end;
        Dev.draw_glyph (glyph : Dev.glyph) x y;
        add_char st x y code glyph
      end
  with _ -> ();;

let set st code =
  put st code;
  try
    let (dx, dy) = Table.get st.cur_mtable code in
    st.h <- st.h + dx;
    st.v <- st.v + dy
  with _ -> ();;

let put_rule st a b =
  let x = st.x_origin + Misc.round (st.conv *. float st.h)
  and y = st.y_origin + Misc.round (st.conv *. float st.v)
  and w = int_of_float (ceil (st.conv *. float b))
  and h = int_of_float (ceil (st.conv *. float a)) in
  add_rule st x (y - h) w h;
  if !visible then Dev.fill_rect x (y - h) w h;;

let set_rule st a b =
  put_rule st a b;
  st.h <- st.h + b;;

(*** Specials ***)

let ill_formed_special s =
  Misc.warning (Printf.sprintf "Ill formed special <<%s>>" s);;

exception Ill_formed_special of string;;

let line_of_special s k =
  match split_string s k with
  | line :: rest ->
      (* Printf.eprintf "%s @ %s\n%!" line
         (match rest with h :: _ -> h | _ -> ""); *)
      begin try
        let l = int_of_string line in
        let f = match rest with | file :: _ -> Some file | _ -> None in
        (l, f)
      with
      | Failure _ -> raise (Ill_formed_special s)
      end
  | _ -> raise (Ill_formed_special s);;

let line_special st s k =
  try add_line st (line_of_special s k)
  with Ill_formed_special s -> ill_formed_special s;;

let color_special st s =
  match split_string s 0 with
  | "color" :: "push" :: args ->
     color_push st (Dvicolor.parse_color_args args)
  | "color" :: "pop" :: [] ->
     color_pop st
  | "color" :: args ->
     let _c = Dvicolor.parse_color_args args in
     Misc.warning "global color special is not supported"
  | _ -> ill_formed_special s;;

let parse_float s =
 try float_of_string s with
 | _ ->
   failwith (Printf.sprintf "advi: cannot read a floating number in %S" s);;

let parse_quoted_float s =
 parse_float (unquote s);;

let parse_float_option s r =
  try Some (parse_float (List.assoc s r)) with _ -> None;;

let alpha_special st s =
  match split_string s 0 with
  | ["advi:"; "alpha"; "push"; arg] ->
      alpha_push st (parse_float arg)
  | ["advi:"; "alpha"; "pop"] ->
      alpha_pop st
  | _ -> ill_formed_special s;;

let parse_blend s =
  match String.lowercase_ascii s with
  | "none" -> Drawimage.Normal
  | "normal" -> Drawimage.Normal
  | "multiply" -> Drawimage.Multiply
  | "screen" -> Drawimage.Screen
  | "overlay" -> Drawimage.Overlay
  | "dodge" -> Drawimage.ColorDodge
  | "burn" -> Drawimage.ColorBurn
  | "darken" -> Drawimage.Darken
  | "lighten" -> Drawimage.Lighten
  | "difference" -> Drawimage.Difference
  | "exclusion" -> Drawimage.Exclusion
  | _ ->
      Misc.warning (Printf.sprintf "blend: invalid blend mode %s" s);
      Drawimage.Normal;;

let blend_special st s =
  match split_string s 0 with
  | ["advi:"; "blend"; "push"; arg] ->
      blend_push st (parse_blend arg)
  | "advi:" :: "blend" :: "pop" :: [] ->
      blend_pop st
  | _ -> ill_formed_special s;;

let parse_bool s =
  match String.lowercase_ascii s with
  | "true" -> true
  | "false" -> false
  | _ -> failwith "invalid boolean";;

let epstransparent_special st s =
  match split_string s 0 with
  | ["advi:"; "epstransparent"; "push"; arg] ->
      epstransparent_push st (parse_bool arg)
  | "advi:" :: "epstransparent" :: "pop" :: [] ->
      epstransparent_pop st
  | _ -> ill_formed_special s;;

let epsbygs_special st s =
  match split_string s 0 with
  | ["advi:"; "epsbygs"; "push"; arg] ->
      epsbygs_push st (parse_bool arg)
  | "advi:" :: "epsbygs" :: "pop" :: [] ->
      epsbygs_pop st
  | _ -> ill_formed_special s;;

let epswithantialiasing_special st s =
  match split_string s 0 with
  | ["advi:"; "epswithantialiasing"; "push"; arg] ->
      epswithantialiasing_push st (parse_bool arg)
  | "advi:" :: "epswithantialiasing" :: "pop" :: [] ->
      epswithantialiasing_pop st
  | _ -> ill_formed_special s;;

let get_records s =
  List.map (fun (k, v) -> String.lowercase_ascii k, v) (split_record s);;

let psfile_special st s =
  try
    Misc.debug_endline (Printf.sprintf "psfile_special %S" s);
    let records = get_records s in
    let file =
      try unquote (List.assoc "psfile" records)
      with Not_found -> failwith "psfile: invalid special" in
    Misc.debug_endline ("PSFILE=" ^ file);
    (* bbox *)
    let llx, lly, urx, ury as bbox =
      try
        let llx = int_of_float_of_string (List.assoc "llx" records)
        and lly = int_of_float_of_string (List.assoc "lly" records)
        and urx = int_of_float_of_string (List.assoc "urx" records)
        and ury = int_of_float_of_string (List.assoc "ury" records) in
        Misc.debug_endline
          (Printf.sprintf "BBOX=%d %d %d %d" llx lly urx ury);
        llx, lly, urx, ury
      with
      | _ -> failwith "psfile: no bbox" in
    let rwi = try int_of_string (List.assoc "rwi" records) with _ -> 0 in
    let rhi = try int_of_string (List.assoc "rhi" records) with _ -> 0 in

    let file, drawbygs =
      let get_second_token s =
        Scanf.sscanf s "`%_s %s" (fun token -> token) in
      try
        if file.[0] = '`' then (* ex. `jpeg2ps world.jpg *)
          get_second_token file, false (* it must not be eps *)
        else file, st.epsbygs
      with
      | _ -> 
          (* file must be an eps *)
          file, st.epsbygs  in

    let x = st.x_origin + Misc.round (st.conv *. float st.h) in
    let y = st.y_origin + Misc.round (st.conv *. float st.v) in
    if !visible then
      if drawbygs then
        Dev.draw_ps_by_gs file bbox (rwi, rhi)
          (x - st.x_origin) (y - st.y_origin)
      else 
        let width, height = 
          match rwi, rhi with
          | 0, 0 -> float (urx - llx), float (ury - lly)
          | 0, _ ->
             let h = float rhi *. 0.1 in
             let w = float (urx - llx) *. (h /. float (ury - lly)) in
             w, h
          | _, 0 ->
             let w = float rwi *. 0.1 in
             let h = float (ury - lly) *. (w /. float (urx - llx)) in
             w, h
          | _, _ -> float rwi *. 0.1, float rhi *. 0.1 in
        let dpi = ldexp (float st.sdpi) (-16) in
        let width_pixel = truncate (width /. 72.0 *. dpi) in
        let height_pixel = truncate (height /. 72.0 *. dpi) in
        Dev.draw_img file st.epstransparent st.alpha st.blend (Some bbox)
          Drawimage.ScaleAuto st.epswithantialiasing
          (width_pixel, height_pixel) x y
  with
  | exc ->
      Misc.warning
        (Printf.sprintf "Failed to load psfile: %s" (Printexc.to_string exc));;

(* Killing embedded applications:
   (1) we parse Unix signals in specials. *)
let int_of_signal = function
  | "SIGABRT" | "sigabrt" -> Sys.sigabrt (* -1 *)
  | "SIGALRM" | "sigalrm" -> Sys.sigalrm (* -2 *)
  | "SIGFPE" | "sigfpe" -> Sys.sigfpe (* -3 *)
  | "SIGHUP" | "sighup" -> Sys.sighup (* -4 *)
  | "SIGILL" | "sigill" -> Sys.sigill (* -5 *)
  | "SIGINT" | "sigint" -> Sys.sigint (* -6 *)
  | "SIGKILL" | "sigkill" -> Sys.sigkill (* -7 *)
  | "SIGPIPE" | "sigpipe" -> Sys.sigpipe (* -8 *)
  | "SIGQUIT" | "sigquit" -> Sys.sigquit (* -9 *)
  | "SIGSEGV" | "sigsegv" -> Sys.sigsegv (* -10 *)
  | "SIGTERM" | "sigterm" -> Sys.sigterm (* -11 *)
  | "SIGUSR1" | "sigusr1" -> Sys.sigusr1 (* -12 *)
  | "SIGUSR2" | "sigusr2" -> Sys.sigusr2 (* -13 *)
  | "SIGCHLD" | "sigchld" -> Sys.sigchld (* -14 *)
  | "SIGCONT" | "sigcont" -> Sys.sigcont (* -15 *)
  | "SIGSTOP" | "sigstop" -> Sys.sigstop (* -16 *)
  | "SIGTSTP" | "sigtstp" -> Sys.sigtstp (* -17 *)
  | "SIGTTIN" | "sigttin" -> Sys.sigttin (* -18 *)
  | "SIGTTOU" | "sigttou" -> Sys.sigttou (* -19 *)
  | "SIGVTALRM" | "sigvtalrm" -> Sys.sigvtalrm (* -20 *)
  | "SIGPROF" | "sigprof" -> Sys.sigprof (* -21 *)
  | "" -> Sys.sigquit
  | s -> int_of_string s;;

(* Killing embedded applications:
   (2) finding the application and calling Embed.kill_* to kill it. *)
let kill_embed_special kill_fun st s =
  (* advi: kill[all]embed name=? signal=? *)
  let records = get_records s in
  let app_name =
    try unquote (List.assoc "name" records)
    with Not_found -> failwith ("No command to kill in " ^ s) in
  let signal = List.assoc "signal" records in
  (* prerr_endline (Printf.sprintf "Signal is ``%s''" signal); *)
  let sig_val =
    try int_of_signal (unquote signal) with
    | Not_found -> failwith ("No signal to kill command in " ^ s)
    | Failure _ -> failwith ("Cannot understand signal in " ^ s)  in
  Misc.debug_endline
    (Printf.sprintf "Killing %s, in special %s." app_name s);
  kill_fun sig_val app_name;;

(* Killing embedded applications:
   (3) partial applications to define the required primitives. *)
let kill_one_embed_special = kill_embed_special Embed.kill_embedded_app;;
let kill_all_embed_special = kill_embed_special Embed.kill_all_embedded_app;;

(* Hide or show the window containing the output of an embedded application. *)
let show_hide_embed_special show_fun st s =
  (* advi: [un]map[all]embed name=? *)
  let records = get_records s in
  let app_name =
    try unquote (List.assoc "name" records) with
    | Not_found ->
      failwith ("No application, hence no window to operate in " ^ s) in
  Misc.debug_endline
    (Printf.sprintf "Showing or hiding %s, in special %s." app_name s);
  show_fun app_name;;

(* Mapping or unmapping windows of embedded applications. *)
let unmap_one_embed_special st s =
  show_hide_embed_special Embed.unmap_embedded_app st s;;

let unmap_all_embed_special st s =
  show_hide_embed_special Embed.unmap_all_embedded_app st s;;

let map_one_embed_special st s =
  show_hide_embed_special Embed.map_embedded_app st s;;

let map_all_embed_special st s =
  show_hide_embed_special Embed.map_all_embedded_app st s;;

(* Parsing embedding modes in specials. *)
let app_mode_of_string = function
  | "fake" -> Embed.Fake
  | "raw" -> Embed.Raw
  | "sticky" -> Embed.Sticky
  | "persistent" -> Embed.Persistent
  | "ephemeral" -> Embed.Ephemeral
  | s -> failwith ("Unknown embedding mode " ^ s);;

(* Parsing embedding applications commands and calling embed_app. *)
let embed_special st s =
  (* advi: embed mode=? width=? height=? command="command string" *)
  let records = get_records s in
  let app_mode =
    try app_mode_of_string (List.assoc "mode" records) with
    | Not_found -> failwith ("embed: no embedding mode in special " ^ s) in
  let app_name =
    try unquote (List.assoc "name" records) with
    | Not_found -> "" in
  let command =
    try unquote (List.assoc "command" records) with
    | Not_found ->
        if app_mode = Embed.Fake then "" else
        failwith ("embed: no command to embed in special " ^ s) in
  (* prerr_endline ("embed command=" ^ command); *)
  let get_dim dim records =
    match Dimension.normalize
            (Dimension.dimen_of_string (List.assoc dim records)) with
    | Dimension.In d -> d
    | _ -> assert false in

  let width_pixel, height_pixel =
    let w, h =
      try
        let width = get_dim "width" records in
        let height = get_dim "height" records in
        width, height
      with
      | _ -> failwith ("embed: no size in special " ^ s) in
    let dpi = ldexp (float st.sdpi) (-16) in
    let width_pixel = truncate (w *. dpi) in
    let height_pixel = truncate (h *. dpi) in
  (* prerr_endline (Printf.sprintf "%d x %d pixel" width_pixel height_pixel);*)
    width_pixel, height_pixel in
  let x = st.x_origin + Misc.round (st.conv *. float st.h)
  and y = st.y_origin + Misc.round (st.conv *. float st.v) in
  Misc.debug_endline
    (Printf.sprintf
       "Embedding %s with command %S, in special %s." app_name command s);
  if !visible then
    Dev.embed_app command app_mode app_name width_pixel height_pixel x y;;

(* When scanning the page, we gather information on the embedded commands *)
let scan_embed_special st s =
  let records = get_records s in
  let command =
    try unquote (List.assoc "command" records) with
    | Not_found ->
      failwith ("advi embed: no command to embed in special " ^ s) in
  Launch.add_white_run_command command;;

let parse_transition dir mode record =
  let default_dir =
    match dir with Some d -> d | None -> Transitions.DirNone in
  let parse_genpath record =
    try List.assoc "genpath" record with
    | Not_found ->
        Misc.warning
          (Printf.sprintf "advi trans push: genpath function not found");
        "spiral" in
  let parse_pathelem s =
      (parse_float_option (s ^ "x") record,
       parse_float_option (s ^ "y") record,
       None,
       None) (* to complete with parsed scale and rotation *) in
  let parse_steps =
    try
      let stepsstr = List.assoc "steps" record in
      try Some (int_of_string stepsstr) with
      | _ ->
         Misc.warning
           (Printf.sprintf "advi trans push: steps parsing failed %S" stepsstr);
         None with
    | Not_found -> None
  in
  let parse_direction key default =
    try
      match String.lowercase_ascii (List.assoc key record) with
      | "left" -> Transitions.DirLeft
      | "right" -> Transitions.DirRight
      | "top" | "up" -> Transitions.DirTop
      | "bottom" | "down" -> Transitions.DirBottom
      | "topleft" | "upleft" -> Transitions.DirTopLeft
      | "topright" | "upright" -> Transitions.DirTopRight
      | "bottomleft" | "downleft" -> Transitions.DirBottomLeft
      | "bottomright" | "downright" -> Transitions.DirBottomRight
      | "center" -> Transitions.DirCenter
      | s ->
         Misc.warning
           (Printf.sprintf "advi trans push: direction parsing failed %S" s);
         raise Exit
    with _ -> default (* Transitions.DirNone *)
  in
  match String.lowercase_ascii mode with
  | "slide" ->
      Transitions.TransSlide (parse_steps, parse_direction "from" default_dir)
  | "wipe" ->
      Transitions.TransWipe (parse_steps, parse_direction "from" default_dir)
  | "block" ->
      Transitions.TransBlock
        (parse_steps, parse_direction "from" Transitions.DirNone)
  | "path" ->
      Transitions.TransPath
        (parse_steps,
         parse_genpath record,
         parse_pathelem "start",
         parse_pathelem "stop")
  | "none" ->
      Transitions.TransNone
  | _ ->
     Misc.warning
      (Printf.sprintf "advi trans push: mode parsing failed %S" mode);
     Transitions.TransNone;;

let transition_special st s =
  match split_string s 0 with
  | "advi:" :: "trans" :: mode :: args ->
      let record = split_record (String.concat " " args) in
      let trans = parse_transition st.direction mode record in
      transition_push st trans
  | _ -> ill_formed_special s;;

let transbox_save_special st s =
  match split_string s 0 with
  | "advi:" :: "transbox" :: "save" :: args ->
      let dpi = ldexp (float st.sdpi) (-16) in
      let record = split_record (String.concat " " args) in
      let width = Dimension.dimen_of_string (List.assoc "width" record) in
      let height = Dimension.dimen_of_string (List.assoc "height" record) in
      let depth = Dimension.dimen_of_string (List.assoc "depth" record) in
      let pixels_of_dimen dim =
        match Dimension.normalize dim with
        | Dimension.Px x -> x
        | Dimension.In x -> truncate (x *. dpi)
        | _ -> assert false in
      let width_pixel = pixels_of_dimen width
      and height_pixel = pixels_of_dimen height
      and depth_pixel = pixels_of_dimen depth in
      let x = st.x_origin + Misc.round (st.conv *. float st.h)
      and y = st.y_origin + Misc.round (st.conv *. float st.v) + depth_pixel
      in
      Dev.transbox_save x y width_pixel (height_pixel + depth_pixel)
  | _ -> failwith "advi: transbox save special failed";;

let transbox_go_special st s =
  match split_string s 0 with
  | "advi:" :: "transbox" :: "go" :: mode :: args ->
      let record = split_record (String.concat " " args) in
      let trans = parse_transition None mode record in
      Dev.transbox_go trans
  | _ -> failwith "advi: transbox go special failed";;

exception Ignore;;

let edit_special st s =
  try
  match split_string s 0 with
  | "advi:" :: "edit" :: args ->
      let record = split_record (String.concat " " args) in
      let first =
        try
          let assignable (x, v) = List.mem x [ "x"; "y"; "w"; "h"; "d"; ] in
          Some (List.find assignable record)
        with Not_found -> None in
      let field x =
        try List.assoc x record
        with Not_found -> 
            Misc.warning (Printf.sprintf "Field %s missing in special %s" x s);
          raise Ignore in
      let dpi = ldexp (float st.sdpi) (-16) in
      let pixels dim =
        match Dimension.normalize (Dimension.dimen_of_string dim) with
        | Dimension.Px x -> float x
        | Dimension.In x -> x *. dpi
        | _ -> assert false in
      let xunit,yunit = 
        try pixels (field "xunit"), pixels (field "yunit") 
        with Not_found -> let u = pixels (field "unit") in u, u in
      let float_field x x' =
        let b, fx =
          try true, List.assoc x record
          with Not_found -> false, field x' in
        try b, float_of_string fx
        with _  ->
          Misc.warning
            (Printf.sprintf "Field %s=%s not a float in special %s" x fx s);
            raise Ignore in
      let float_to_pixel f unit = truncate (f *. unit) in
      let raw_fields = {
        Dev.rx = float_field "x" "X"; Dev.ry = float_field "y" "Y";
        Dev.rw = float_field "w" "W"; Dev.rh = float_field "h" "H";
        Dev.rd = float_field "d" "D"; 
      } in
      let rmap f r = {
         Dev.rx = f r.Dev.rx; 
         Dev.ry = f r.Dev.ry; 
         Dev.rw = f r.Dev.rw; 
         Dev.rh = f r.Dev.rh; 
         Dev.rd = f r.Dev.rd;
      } in
      let r = rmap snd raw_fields in
      let m = rmap fst raw_fields in
      let rect = {
        Dev.rx = st.x_origin + Misc.round (st.conv *. float st.h)
          + float_to_pixel r.Dev.rx xunit;
        Dev.ry = st.y_origin + Misc.round (st.conv *. float st.v)
          - float_to_pixel r.Dev.ry yunit;
        Dev.rw = float_to_pixel r.Dev.rw xunit;
        Dev.rh = float_to_pixel r.Dev.rh yunit;
        Dev.rd = float_to_pixel r.Dev.rd yunit;
      } in
      let info =
        { Dev.E.comm = field "comm";
          Dev.E.name = field "name";
          Dev.E.line = field "line";
          Dev.E.file = field "file";
          Dev.E.first = first;
          Dev.E.xunit = xunit;
          Dev.E.yunit = yunit;
          Dev.E.origin = r;
          Dev.E.action = m;
        } in
      Dev.E.add rect info

  | _ -> ill_formed_special s
  with Ignore -> ()
;;

(* Defining the forward function eval_command. *)
let forward_eval_command =
  ref (fun (st : state) (c : Dvicommands.command) ->
        (failwith "Undefined forward eval_command" : unit));;

let set_forward_eval_command f = forward_eval_command := f;;

let eval_command st c = !forward_eval_command st c;;

let playing = ref 0;;
let get_playing () = !playing;;

(* Setting the forward function Dev.get_playing. *)
Dev.set_forward_get_playing get_playing;;

let visible_stack = ref [];;

let proc_clean () =
  current_recording_proc := [];
  playing := 0;
  visible_stack := [];
  visible := true;
  Hashtbl.clear procs;;

let proc_special st s =
  let records = get_records s in
  try
    let v = List.assoc "record" records in
    match v with
    | "start" ->
        let procname =
          try unquote (List.assoc "proc" records) with
          | Not_found -> failwith "proc: invalid special" in
        visible_stack := !visible :: !visible_stack;
        visible := List.mem_assoc "play" records;
        if !playing = 0 then
          let recording =
            { tag = procname;
              unit =
                { escaped_register = get_register_set st;
                  escaped_stack = st.stack;
                  escaped_cur_mtable = st.cur_mtable;
                  escaped_cur_gtable = st.cur_gtable;
                  escaped_cur_font = st.cur_font;
                  escaped_commands = [] }
            } in
          current_recording_proc := recording :: !current_recording_proc
    | "end" ->
        if !playing = 0 then begin
          match !current_recording_proc with
          | [] -> Misc.warning (Printf.sprintf "'xxx %s' not recording" s)
          | recording :: rest ->
              let procname = recording.tag in
              current_recording_proc := rest;
              let u = recording.unit in
              Hashtbl.add procs procname u;
              match u.escaped_commands with
              | h :: rest -> u.escaped_commands <- List.rev rest
              | [] -> assert false
        end;
        begin match !visible_stack with
        | h :: rest ->
            visible := h; visible_stack := rest;
        | [] ->
            (* Ill-formed DVI not recording error should have ben reported
               right above *)
            ();
        end;
    | _ -> ill_formed_special s
  with
  | Not_found ->
      let procname =
        try unquote (List.assoc "proc" records) with
        | Not_found -> failwith "proc: invalid special" in
      try
        ignore (List.assoc "play" records);
        if not (is_recording ()) then begin
          let us = Hashtbl.find_all procs procname in
          let escaped_cur_font = st.cur_font
          and escaped_cur_mtable = st.cur_mtable
          and escaped_cur_gtable = st.cur_gtable in
          let escaped_stack = push st; st.stack in
          incr playing;
          List.iter
            (fun u ->
               set_register_set st u.escaped_register;
               st.stack <- u.escaped_stack;
               st.cur_mtable <- u.escaped_cur_mtable;
               st.cur_gtable <- u.escaped_cur_gtable;
               st.cur_font <- u.escaped_cur_font;
               List.iter (fun com -> eval_command st com)
                 u.escaped_commands
            ) us;
          decr playing;
          st.stack <- escaped_stack; pop st;
          st.cur_mtable <- escaped_cur_mtable;
          st.cur_gtable <- escaped_cur_gtable;
          st.cur_font <- escaped_cur_font;
        end with
      | Not_found ->
          Misc.warning
            (Printf.sprintf "xxx '%s': %s not recorded" s procname);;

let wait_special st s =
  let records = get_records s in
  let second =
    try parse_float (List.assoc "sec" records) with
    | Not_found | Failure _ ->
        failwith (Printf.sprintf "wait: invalid special: [ %s ]" s) in
  (* Wait is treated like Pause, as an exception *)
  if !visible then raise (Wait second);
  st.checkpoint <- 0;;

(* Background object configuration. *)
let inherit_background_info =
  Options.flag false
    "-inherit-background"
    "  the background options are inherited\
    \n\t from the previous page,\
    \n\t (the default is not to inherit background settings).";;

let setup_bkgd st =
  (* Propagate bkgd preferences to graphics device
     storing the default/inherited prefs into Dev. *)
  Dev.blit_bkgd_data st.Cdvi.bkgd_prefs Dev.bkgd_data;
  (* Apply local modifications. *)
  Dev.set_bg_options st.Cdvi.bkgd_local_prefs;
  (* Recover modified preferences. *)
  Dev.blit_bkgd_data Dev.bkgd_data st.Cdvi.bkgd_prefs;;

let ratios_alist = [
  ("auto", Drawimage.ScaleAuto);
  ("center", Drawimage.ScaleCenter);
  ("top", Drawimage.ScaleTop);
  ("bottom", Drawimage.ScaleBottom);
  ("left", Drawimage.ScaleLeft);
  ("right", Drawimage.ScaleRight);
  ("topleft", Drawimage.ScaleTopLeft);
  ("bottomright", Drawimage.ScaleBottomRight);
  ("topright", Drawimage.ScaleTopRight);
  ("bottomleft", Drawimage.ScaleBottomLeft);
];;

(* The find_bgfun function should eventually handle dynamically
   loaded plugins *)
let bggradients_alist = [
  ("hgradient", Addons.hgradient);
  ("vgradient", Addons.vgradient);
  ("dgradient", Addons.dgradient);
  ("d1gradient", Addons.d1gradient);
  ("d2gradient", Addons.d2gradient);
  ("cgradient", Addons.cgradient);
  ("circgradient", Addons.circgradient);
];;

let find_bggradient s =
   try Some (List.assoc (unquote s) bggradients_alist) with _ -> None;;

let bkgd_alist = [
  ("color", fun s st ->
     let c = Dvicolor.parse_color_args (split_string (unquote s) 0) in
     [Dev.BgColor c]);
  ("image", fun s st ->
     [Dev.BgImg s]);
  ("reset", fun s st ->
     Dev.blit_bkgd_data (Dev.default_bkgd_data ()) st.Cdvi.bkgd_prefs;
     []);
  ("inherit", fun s st ->
     inherit_background_info := true;
     []);
  ("alpha", fun s st ->
     let a = parse_quoted_float s in
     [Dev.BgAlpha a]);
  ("blend", fun s st ->
     let b = parse_blend (unquote s) in
     [Dev.BgBlend b]);
  ("fit", fun s st ->
     let f =
       try List.assoc (unquote s) ratios_alist with _ -> Drawimage.ScaleAuto in
     [Dev.BgRatio f]);
  ("colorstart", fun s st ->
     let c = Dvicolor.parse_color_args (split_string (unquote s) 0) in
     [Dev.BgColorStart c]);
  ("colorstop", fun s st ->
     let c = Dvicolor.parse_color_args (split_string (unquote s) 0) in
     [Dev.BgColorStop c]);
  ("xstart", fun s st ->
     let x = parse_quoted_float s in
     [Dev.BgXStart x]);
  ("ystart", fun s st ->
     let y = parse_quoted_float s in
     [Dev.BgYStart y]);
  ("width", fun s st ->
     let w = parse_quoted_float s in
     [Dev.BgWidth w]);
  ("height", fun s st ->
     let h = parse_quoted_float s in
     [Dev.BgHeight h]);
  ("xcenter", fun s st ->
     let x = parse_quoted_float s in
     [Dev.BgXCenter x]);
  ("ycenter", fun s st ->
     let y = parse_quoted_float s in
     [Dev.BgYCenter y]);
  ("gradient", fun s st ->
     [Dev.BgGradient (find_bggradient (unquote s))]);
];;

let filter_alist alist falist =
  let aux k alist okalist =
    try (k, List.assoc k alist) :: okalist with
    | Not_found -> okalist in
  List.fold_left (fun l -> fun k -> aux k alist l) []
                 (List.map (fun (k, v) -> k) falist);;

(* When scanning the page, we just fill the info structure for backgrounds *)
let scan_bkgd_special st s =
  let records = get_records s in
  st.Cdvi.bkgd_local_prefs <-
    List.flatten
      (List.map (fun (k, v) -> (List.assoc k bkgd_alist) v st)
                (filter_alist records bkgd_alist)) @
    st.Cdvi.bkgd_local_prefs;;

(* When not scanning, we ignore the background information *)
let bkgd_special st s = ();;

(* Support for TPIC specials. *)

let milli_inch_to_sp = AdviUnits.from_to AdviUnits.IN AdviUnits.SP 1e-3;;

let tpic_milli_inches s = parse_float s *. milli_inch_to_sp;;

let no_shading = -1.0

let tpic_pen st =
  Misc.round (st.conv *. st.tpic_pensize);;

let tpic_x st x =
  st.x_origin + Misc.round (st.conv *. (float st.h +. x));;
let tpic_y st y =
  st.y_origin + Misc.round (st.conv *. (float st.v +. y));;

let tpic_flush_path st cntr =
  let path = Array.of_list (List.rev st.tpic_path) in
  (* Convert points in path to pixel coordinates *)
  let pixpath =
    Array.map (fun (x, y) -> (tpic_x st x, tpic_y st y)) path in
  (* If shading requested and path is closed, fill *)
  if st.tpic_shading >= 0.0 &&
     Array.length path >= 2 &&
     path.(0) = path.(Array.length path - 1) &&
     !visible then Dev.fill_path pixpath ~shade:st.tpic_shading;
  (* If requested, draw outline of path *)
  if cntr && !visible then Dev.draw_path pixpath ~pensize:(tpic_pen st);
  (* Reset path *)
  st.tpic_path <- [];
  st.tpic_shading <- no_shading;;

let dist (x0, y0) (x1, y1) = abs (x0 - x1) + abs (y0 - y1);;

let tpic_spline_path st =
  (* Code shamelessly stolen from xdvi *)
  let path =
    Array.of_list
      (List.map (fun (x, y) -> (tpic_x st x, tpic_y st y))
                (List.rev st.tpic_path)) in
  let p =
    Array.concat [[|path.(0)|]; path; [|path.(Array.length path - 1)|]] in
  let r = ref [] in
  for i = 0 to Array.length p - 3 do
    let steps = (dist p.(i) p.(i + 1) + dist p.(i + 1) p.(i + 2)) / 4 in
    let (x2, y2) = p.(i + 2)
    and (x1, y1) = p.(i + 1)
    and (x0, y0) = p.(i) in
    for j = 0 to steps - 1 do
      let w = (j * 1000 + 500) / steps in
      let t1 = w * w / 20 in
      let w = w - 500 in
      let t2 = (750000 - w * w) / 10 in
      let w = w - 500 in
      let t3 = w * w / 20 in
      let xp = (t1 * x2 + t2 * x1 + t3 * x0 + 50000) / 100000
      and yp = (t1 * y2 + t2 * y1 + t3 * y0 + 50000) / 100000 in
      r := (xp, yp) :: !r
    done
  done;
  if !visible then
    Dev.draw_path (Array.of_list (List.rev !r)) ~pensize:(tpic_pen st);
  st.tpic_path <- [];
  st.tpic_shading <- no_shading;;

let rad_to_deg = 45.0 /. atan 1.0;;

let tpic_arc st x y rx ry s e cntr =
  let x = tpic_x st x
  and y = tpic_y st y
  and rx = Misc.round (st.conv *. rx)
  and ry = Misc.round (st.conv *. ry)
  and s = Misc.round (s *. rad_to_deg)
  and e = Misc.round (e *. rad_to_deg) in
  (* If shading requested, fill the arc *)
  if st.tpic_shading >= 0.0 && !visible then
    Dev.fill_arc ~x ~y ~rx ~ry ~start:s ~stop:e ~shade:st.tpic_shading;
  (* If requested, draw outline of arc *)
  if cntr && !visible then
    Dev.draw_arc ~x ~y ~rx ~ry ~start:s ~stop:e ~pensize:(tpic_pen st);
  (* Reset shading *)
  st.tpic_shading <- no_shading;;

let tpic_specials st s =
  match split_string s 0 with
  | "pn" :: size :: _ ->
      st.tpic_pensize <- tpic_milli_inches size
  | "pa" :: x :: y :: _ ->
      st.tpic_path <-
        (tpic_milli_inches x, tpic_milli_inches y) :: st.tpic_path
  | "fp" :: _ ->
      tpic_flush_path st true
  | "ip" :: _ ->
      tpic_flush_path st false
  | "da" :: _ -> (* TODO: dashed lines *)
      tpic_flush_path st true
  | "dt" :: _ -> (* TODO: dotted lines *)
      tpic_flush_path st true
  | "sp" :: _ -> (* TODO: dashed/dotted splines *)
      tpic_spline_path st
  | "ar" :: x :: y :: rx :: ry :: s :: e :: _ ->
      tpic_arc st (tpic_milli_inches x) (tpic_milli_inches y)
               (tpic_milli_inches rx) (tpic_milli_inches ry)
               (parse_float s) (parse_float e)
               true
  | "ia" :: x :: y :: rx :: ry :: s :: e :: _ ->
      tpic_arc st (tpic_milli_inches x) (tpic_milli_inches y)
               (tpic_milli_inches rx) (tpic_milli_inches ry)
               (parse_float s) (parse_float e)
               true
  | "sh" :: s :: _ ->
      st.tpic_shading <- parse_float s
  | "wh" :: _ ->
      st.tpic_shading <- 0.0
  | "bk" :: _ ->
      st.tpic_shading <- 1.0
  | s :: _ ->
      Misc.warning (Printf.sprintf "Unknown pic command: %s" s)
  | _ -> ();;
(* End of TPIC hacks *)

let put_special st s =
  if Gs.get_do_ps () && st.status.Cdvi.hasps then
    if s = "begin" then
      let x, y = Dev.current_pos () in
      (* we get absolute coordinates and convert them *)
      let h' = Misc.round (float (x - st.x_origin) /. st.conv) in
      let v' = Misc.round (float (y - st.y_origin) /. st.conv) in
      st.put <- (st.h - h', st.v - v') :: st.put;
      st.h <- h'; st.v <- v' else
    if s = "end" then
      match st.put with
      |  _ :: tail -> st.put <- tail
      | _ -> ();;

let rec put_coor x y = function
  | (dx, dy) :: tail -> put_coor (x + dx) (y + dy) tail
  | [] -> x, y;;

let ps_special st s =
  if Gs.get_do_ps () && st.status.Cdvi.hasps then
     let h', v' = put_coor st.h st.v st.put in
     let x = Misc.round (st.conv *. float h') in
     let y = Misc.round (st.conv *. float v') in
     if !visible then
       begin try
         Dev.exec_ps s x y
       with Dev.GS ->
         st.status.Cdvi.hasps <- false
       end;;

(* header are not "rendered", only stored during scan *)
let header_special st s = ();;

(* For html specials *)

(* Should check that a pause is not in the middle of some html code *)
let open_html st link tag =
  st.html <- (tag (unquote link), ref []) :: st.html
(*   match st.html with *)
(*   | Some (t, k) -> st.html <- Some (t, succ k) *)
(*   | None -> st.html <- Some (tag (unquote link), 0);; *)
                                    

let close_html st =
  match st.html with
  | [] -> Misc.warning ("Closing html tag that was not open")
  |  (tag, draw) :: rest ->
      Dev.H.add {Dev.H.tag = tag; Dev.H.draw = List.rev !draw};
      st.html <- rest;
      match rest with
      | (_, draw') :: _ -> draw' := !draw @ !draw'
      | [] -> ()
;;

(*   match st.html with *)
(*   | Some (tag, k) when k > 0 -> *)
(*       (\* Just added that line *\) *)
(*       Dev.H.add {Dev.H.tag = tag; Dev.H.draw = List.rev st.draw_html}; *)
(*       st.html <- Some (tag, k - 1) *)
(*   | Some (tag, 0) -> *)
(*       Dev.H.add {Dev.H.tag = tag; Dev.H.draw = List.rev st.draw_html}; *)
(*       st.html <- None; *)
(*       st.draw_html <- [] *)
(*   | Some (_, k) -> assert false *)
(*   | None -> Misc.warning ("Closing html tag that was not open");; *)

let html_special st html =
  if has_prefix "<A " html || has_prefix "<a " html then
    let stripped = String.sub html 3 (String.length html - 4) in
    let fields = split_record stripped in
    begin match fields with
    | ("name", link) :: _ ->
        open_html st link (fun x -> Dev.H.Name x)
    | ("href", link) :: _ ->
        open_html st link (fun x -> Dev.H.Href x)
    | (("advi" | "hdvi" | "pdvi" as kind), link) :: rest ->
        let mode =
          if kind = "advi" then Dev.H.Over
          else if kind = "hdvi" then Dev.H.Click_down
          else Dev.H.Stick
        in
        let style =
          try
            match List.assoc "style" rest with
            | "invisible" -> Dev.H.Invisible
            | "underline" -> Dev.H.Underline
            | "box" -> Dev.H.Box
            | _ ->
                Misc.warning
                  (Printf.sprintf "Incorrect style in html suffix %s" html);
                Dev.H.Box
          with Not_found -> Dev.H.Box in
        let color =
          try Some (Dvicolor.parse_color_args [List.assoc "color" rest])
          with Not_found -> None in
        let advi x =
          let play () = proc_special st ("advi: proc=" ^ x ^ " play") in
          Dev.H.Advi
            {Dev.H.link = x;
             Dev.H.action = play;
             Dev.H.mode = mode;
             Dev.H.style = style;
             Dev.H.color = color;
             Dev.H.area = None} in
        open_html st link advi
    | _ -> Misc.warning (Printf.sprintf "Unknown html suffix %s" html)
    end else
  if has_prefix "</A>" html || has_prefix "</a>" html then close_html st else
  Misc.warning (Printf.sprintf "Unknown html suffix %s" html);;

let scan_special_html (_, xrefs, _) page s =
  let name = String.sub s 14 (String.length s - 16) in
  Hashtbl.add xrefs name page;;

let scan_special_line (_, _, lastline) s k =
  try lastline := Some (line_of_special s k)
  with Ill_formed_special s -> ill_formed_special s;;

let save_page_image_special st = Shot.save_page_image ();;

let get_file_name records =
  try unquote (List.assoc "file" records) with
  | Not_found -> failwith "advi_save_page: invalid special (no file name)";;

let save_page_image_file_special st s =
  let records = get_records s in
  Shot.save_page_image_file (get_file_name records);;

let save_page_area_image_special st s =
  let records = get_records s in
  try
    let x = int_of_string (List.assoc "x" records)
    and y = int_of_string (List.assoc "y" records)
    and w = int_of_string (List.assoc "w" records)
    and h = int_of_string (List.assoc "h" records) in
    Shot.save_page_area_image x y w h with
  | Not_found | Failure _ ->
    failwith (Printf.sprintf "advi_save_page: invalid special %s" s);;

let save_page_area_image_file_special st s =
  let records = get_records s in
  try
    let fname = get_file_name records in
    let x = int_of_string (List.assoc "x" records)
    and y = int_of_string (List.assoc "y" records)
    and w = int_of_string (List.assoc "w" records)
    and h = int_of_string (List.assoc "h" records) in
    Shot.save_page_area_image_file fname x y w h with
  | Not_found | Failure _ ->
    failwith (Printf.sprintf "advi_save_page_area: invalid special %s" s);;

let push_keys_special st s =
  Misc.debug_endline (Printf.sprintf "push_keys_special %S" s);
  match split_string_quoted s 0 with
  | ["advi:"; "pushkeys"; keys] ->
     let push_keys keys =
       let push pc =
         if pc >= 0
         then Misc.push_key_event (char_of_int pc) GraphicsY11.nomod in
       let push_control pc =
         if pc >= 0
         then Misc.push_key_event (char_of_int pc) GraphicsY11.control in
       let rec loop pc i =
         if i >= 0 then
           let k = keys.[i] in
           match k with
           | '^' when pc >= 0 -> push_control pc; loop (-1) (i - 1)
           | '^' -> loop (int_of_char k) (i - 1)
           | k when pc >= 0 -> push pc; loop (int_of_char k) (i - 1)
           | k -> loop (int_of_char k) (i - 1)
         else push pc in
       loop (-1) (String.length keys - 1) in
     (* We scan the string as a Caml token to handle properly \ddd
        chars if any. In case of error (i.e. when keys is not properly
        enclosed between double quotes), we simply push back the
        characters verbatim. *)
     Misc.debug_endline (Printf.sprintf "advi_push_keys %S" keys);
     Scanf.kscanf
       (Scanf.Scanning.from_string keys)
       (fun _ _ ->
          let keys = unquote keys in
          push_keys keys)
       "%S" push_keys
  | _ ->
     failwith (Printf.sprintf "advi_push_keys: invalid special ``%s''" s);;

(* This function is iterated on the current DVI page BEFORE
   rendering it, to gather the information contained in some
   "asynchronous" specials (typically, PS headers, background 
   commands, html references) *)

let scan_special status (headers, xrefs, lastline as args) pagenum s =
  try
    if Launch.white_run () &&
       has_prefix "advi: embed " s then scan_embed_special status s else
    (* Embedded Postscript, better be first for speed when scanning *)
    let do_ps = Gs.get_do_ps () in
    if has_prefix "\" " s || has_prefix "ps:" s 
    || has_prefix "psfile=" s || has_prefix "PSfile=" s then
      status.Cdvi.hasps <- do_ps else
    if has_prefix "!" s then
      (if do_ps then headers := (true, get_suffix "!" s) :: !headers) else
    if has_prefix "header=" s then
      (if do_ps
       then headers := (false, get_suffix "header=" s) :: !headers) else
    if has_prefix "advi: setbg " s then scan_bkgd_special status s else
    if has_prefix "line: " s then scan_special_line args s 6 else
  (*
    if has_prefix "src:" s then scan_special_line args s 4 else
  *)
    (* We must test both case <A and <a, since some latex packages
       output the upper case variant of the anchor indication,
       as opposed to package hyperref, and all the Active-DVI
       defined anchor generating macros. *)
    if has_prefix "html:<A name=\"" s || has_prefix "html:<a name=\"" s
    then scan_special_html args pagenum s
  with
  | Failure s -> ill_formed_special s
  | Ill_formed_special s -> ill_formed_special s;;

(* Scan a page calling function scan_special when seeing a special and
  the function otherwise for other DVI stuff. *)

let scan_special_page otherwise cdvi globals pagenum =
  Misc.debug_stop "Scanning specials";
  let page = cdvi.base_dvi.Cdvi.pages.(pagenum) in
  match page.Cdvi.page_status with
  | Cdvi.Unknown ->
      let status =
        {Cdvi.hasps = false;
         Cdvi.bkgd_local_prefs = [];
         Cdvi.bkgd_prefs =
         (if !inherit_background_info
         then Dev.copy_of_bkgd_data ()
         else Dev.default_bkgd_data ())} in
      let lastline = ref None in
      let eval = function
        | Dvicommands.C_xxx s ->
            let globals = (fst globals, snd globals, lastline) in
            scan_special status globals pagenum s
        | x -> otherwise x in
      Cdvi.page_iter eval cdvi.base_dvi.Cdvi.pages.(pagenum);
      page.Cdvi.line <- !lastline;
      page.Cdvi.page_status <- Cdvi.Known status;
      status
  | Cdvi.Known stored_status -> stored_status;;

let scan_find_location cdvi page (line, filename) =
  let intervals = ref [] in
  let last = ref 0 in
  let eval = function
    | Dvicommands.C_xxx s when has_prefix "line: " s ->
        let (l, f as _location) = line_of_special s 6 in
        if f = filename then
          begin
            if !last <= line && line < l then
              intervals := !last :: !intervals;
            last := l
          end;
    | _ -> () in
  Cdvi.page_iter eval cdvi.base_dvi.Cdvi.pages.(page);
  match List.sort (fun l1 l2 -> compare (line - l1) (line - l2)) !intervals
  with
  | l :: _ -> l
  | [] -> 0;;

exception Found of (int * string option) option;;
exception Position of int * int * int * int * int;;

let scan_find_anchor_position cdvi dpi page anchor =
  let dvi = cdvi.base_dvi in
  let mag = float dvi.Cdvi.preamble.Dvicommands.pre_mag /. 1000.0 in
  let conv = mag *. dpi /. cdvi.dvi_res /. 65536.0 in
  let st =
    { cdvi = cdvi;
      sdpi = 0;
      conv = conv;
      x_origin = 0; y_origin = 0;
      cur_mtable = dummy_mtable;
      cur_gtable = dummy_gtable;
      cur_font = dummy_font;
      h = 0; v = 0; w = 0; x = 0; y = 0; z = 0; put = [];
      stack = []; color = Dev.get_fgcolor (); color_stack = [];
      alpha = 1.0; alpha_stack = [];
      blend = Drawimage.Normal; blend_stack = [];
      epstransparent = true; epstransparent_stack = [];
      epsbygs = false;
      epsbygs_stack = [];
      epswithantialiasing = true; epswithantialiasing_stack = [];
      direction = None;
      transition = Transitions.TransNone;
      transition_stack = [];
      tpic_pensize = 0.0; tpic_path = []; tpic_shading = no_shading;
      status = Obj.magic (); 
      headers = [];
      html = []; 
      checkpoint = 0;
    } in
   let eval =
     function
   | Dvicommands.C_xxx s when 
        has_prefix "html:<A name=\"" s 
          && String.sub s 14 (String.length s - 16) = anchor -> 
            raise (Position (st.h, st.v, 0, 0, 0))
  | Dvicommands.C_set code -> 
      begin try
        let (dx, dy) = Table.get st.cur_mtable code in
        st.h <- st.h + dx;
        st.v <- st.v + dy
      with _ -> ()
      end;
  | Dvicommands.C_set_rule (a, b) -> st.h <- st.h + b
  | Dvicommands.C_push -> push st
  | Dvicommands.C_pop -> pop st
  | Dvicommands.C_right k -> st.h <- st.h + k
  | Dvicommands.C_w0 ->  st.h <- st.h + st.w
  | Dvicommands.C_w k -> st.w <- k; st.h <- st.h + st.w
  | Dvicommands.C_x0 ->  st.h <- st.h + st.x
  | Dvicommands.C_x k -> st.x <- k; st.h <- st.h + st.x
  | Dvicommands.C_down k -> st.v <- st.v + k
  | Dvicommands.C_y0 -> st.v <- st.v + st.y
  | Dvicommands.C_y k -> st.y <- k; st.v <- st.v + st.y
  | Dvicommands.C_z0 -> st.v <- st.v + st.z
  | Dvicommands.C_z k -> st.z <- k; st.v <- st.v + st.z
  | Dvicommands.C_fnt n -> fnt st n
  | Dvicommands.C_xxx s -> () (* Should catch displacements *)
  | _ -> () in
  let page_dvi =  cdvi.base_dvi.Cdvi.pages.(page) in
  try Cdvi.page_iter eval page_dvi; raise Not_found
  with Position (h, v, _, _, _) ->
    let x = Misc.round (st.conv *. float h) in
    let y = Misc.round (st.conv *. float v) in
    (x, y, 0, 0, 0);;

let scan_find_anchor_location cdvi page anchor =
   let last = ref None in
   let eval = function
   | Dvicommands.C_xxx s when has_prefix "line: " s ->
   last := Some (line_of_special s 6)
   | Dvicommands.C_xxx s when 
        has_prefix "html:<A name=\"" s 
          && String.sub s 14 (String.length s - 16) = anchor -> 
            raise (Found !last)
    | _ -> () in
  try Cdvi.page_iter eval cdvi.base_dvi.Cdvi.pages.(page); raise Not_found
  with Found l -> l;;

let special st s =
 try
  if has_prefix "\" " s || has_prefix "ps:" s
  || has_prefix "! " s then ps_special st s else
  if has_prefix "advi: put" s then
    put_special st (get_suffix "advi: put" s) else

  (* Other specials *)
  if has_prefix "color " s then color_special st s else
  if has_prefix "html:" s then html_special st (get_suffix "html:" s) else
  if has_prefix "PSfile=" s
  || has_prefix "psfile=" s then psfile_special st s else
  if has_prefix "advi: " s then begin
    if has_prefix "advi: edit" s then edit_special st s else
    if has_prefix "advi: alpha" s then alpha_special st s else
    if has_prefix "advi: blend" s then blend_special st s else
    if has_prefix "advi: epstransparent" s then
      epstransparent_special st s else
    if has_prefix "advi: epsbygs" s then epsbygs_special st s else
    if has_prefix "advi: epswithantialiasing" s then
      epswithantialiasing_special st s else
    if has_prefix "advi: pause" s then raise Pause else
    if has_prefix "advi: proc" s then proc_special st s else
    if has_prefix "advi: setbg " s then bkgd_special st s else
    (* all the following have effects,
       and should be ignored if active is false *)
    if !active then begin
      if has_prefix "advi: wait " s then wait_special st s else
      if has_prefix "advi: embed " s then
        (if !visible then embed_special st s) else
      if has_prefix "advi: trans " s then transition_special st s else
      if has_prefix "advi: transbox save " s then
        transbox_save_special st s else
      if has_prefix "advi: transbox go " s then
        transbox_go_special st s else
      if has_prefix "advi: killembed " s then
        (if !visible then kill_one_embed_special st s) else
      if has_prefix "advi: killallembed " s then
        (if !visible then kill_all_embed_special st s) else
      if has_prefix "advi: mapembed " s then
        (if !visible then map_one_embed_special st s) else
      if has_prefix "advi: mapallembed " s then
        (if !visible then map_all_embed_special st s) else
      if has_prefix "advi: unmapembed " s then
        (if !visible then unmap_one_embed_special st s) else
      if has_prefix "advi: unmapallembed " s then
        (if !visible then unmap_all_embed_special st s) else
      if has_prefix "advi: savepageimage" s then
        (if !visible then save_page_image_special st) else
      if has_prefix "advi: savepageimagefile " s then
        (if !visible then save_page_image_file_special st s) else
      if has_prefix "advi: savepageareaimage " s then
        (if !visible then save_page_area_image_special st s) else
      if has_prefix "advi: savepageareaimagefile " s then
        (if !visible then save_page_area_image_file_special st s) else
      if has_prefix "advi: pushkeys " s then
        (if !visible then push_keys_special st s) else
      Misc.warning (Printf.sprintf "unknown special: %s" s) end
    (* else we ignore it, whether well-formed or ill-formed *)
  end else
  if has_prefix "line: " s then line_special st s 6 else
(*
  if has_prefix "src:" s then line_special st s 4 else
*)
  if has_prefix "pn " s || has_prefix "pa " s || s = "fp" || s = "ip"
  || has_prefix "da " s || has_prefix "dt " s || s = "sp"
  || has_prefix "sp " s || has_prefix "ar " s || has_prefix "ia " s
  || has_prefix "sh " s || s = "wh" || s = "bk"
  then tpic_specials st s
 with
   Pause | Wait _ as exn -> raise exn
 | _ ->
   Misc.warning (Printf.sprintf "Unknown or ill formed special <<%s>>" s);;

(*** Page rendering ***)
let eval_dvi_command st = function
  | Dvicommands.C_set code -> set st code
  | Dvicommands.C_set_rule(a, b) -> set_rule st a b
  | Dvicommands.C_put code -> put st code
  | Dvicommands.C_put_rule(a, b) -> put_rule st a b
  | Dvicommands.C_nop
  | Dvicommands.C_bop _
  | Dvicommands.C_eop -> ()
  | Dvicommands.C_push -> push st
  | Dvicommands.C_pop -> pop st
  | Dvicommands.C_right k -> add_blank 1 st k; st.h <- st.h + k
  | Dvicommands.C_w0 ->  add_blank 2 st st.w; st.h <- st.h + st.w
  | Dvicommands.C_w k -> st.w <- k; add_blank 3 st st.w; st.h <- st.h + st.w
  | Dvicommands.C_x0 ->  add_blank 4 st st.x; st.h <- st.h + st.x
  | Dvicommands.C_x k -> st.x <- k; add_blank 5 st st.x; st.h <- st.h + st.x
  | Dvicommands.C_down k -> st.v <- st.v + k
  | Dvicommands.C_y0 -> st.v <- st.v + st.y
  | Dvicommands.C_y k -> st.y <- k; st.v <- st.v + st.y
  | Dvicommands.C_z0 -> st.v <- st.v + st.z
  | Dvicommands.C_z k -> st.z <- k; st.v <- st.v + st.z
  | Dvicommands.C_fnt n -> fnt st n
  | Dvicommands.C_xxx s -> special st s
  | Dvicommands.C_fnt_def (_, _)
  | Dvicommands.C_pre _
  | Dvicommands.C_post (_, _)
  | Dvicommands.C_post_post _ -> ();;

let eval_command st c =
  let record r =
    let u = r.unit in
    match c with
    (* The advi: proc specials are not recorded *)
    (* | Dvicommands.C_xxx s when has_prefix "advi: proc" s -> () *)
    | _ -> u.escaped_commands <- c :: u.escaped_commands in
  List.iter record !current_recording_proc;
  eval_dvi_command st c;;

(* Setting the forward function eval_command. *)
set_forward_eval_command eval_command;;

let newpage h st magdpi x y =
  if st.status.Cdvi.hasps then 
    try Dev.newpage h st.sdpi magdpi x y
    with Dev.GS -> st.status.Cdvi.hasps <- false
  else Dev.clearps ();;

let find_prologues l =
  let l = List.rev l in
  let h = List.map snd (List.filter (function b, _ -> not b) l)  in
  try
    let h' = Search.true_file_names [] h in
    let table = List.combine h h' in
    try
      List.map
        (function b, s as p -> if b then p else b, List.assoc s table) l
    with
    | Not_found -> assert false
  with
  | Invalid_argument _ | Not_found ->
      Misc.warning
        "Cannot find postscript prologue. Continuing without Postscript";
      Gs.set_do_ps false;
      [];;

let render_step cdvi num ?trans ?chst dpi xorig yorig =
  proc_clean ();
  if num < 0 || num >= Array.length cdvi.base_dvi.Cdvi.pages then
    invalid_arg "Driver.render_step";
  let dvi = cdvi.base_dvi in
  let mag = float dvi.Cdvi.preamble.Dvicommands.pre_mag /. 1000.0
  and page = dvi.Cdvi.pages.(num) in
  let otherwise = ignore in
  let status =
    let headers = ref []
    and xrefs = dvi.Cdvi.xrefs in
    let s = scan_special_page otherwise cdvi (headers, xrefs) num in
    if !headers <> [] then Dev.add_headers (find_prologues !headers);
    s in
 (* Didier: should it be ``Gs.get_do_ps ()'' instead of ``false''? 
    and why has Dvi been changed to Cdvi ?
    --Why should we forget about the status? 
  status.Cdvi.hasps <- Gs.get_do_ps ();
  *)
  let orid = function Some f -> f | None -> fun x->x in
  let st =
    { cdvi = cdvi;
      sdpi = Misc.round (mag *. ldexp dpi 16);
      conv = mag *. dpi /. cdvi.dvi_res /. 65536.0;
      x_origin = xorig; y_origin = yorig;
      cur_mtable = dummy_mtable;
      cur_gtable = dummy_gtable;
      cur_font = dummy_font;
      h = 0; v = 0; w = 0; x = 0; y = 0; z = 0; put = [];
      stack = []; color = Dev.get_fgcolor (); color_stack = [];
      alpha = 1.0; alpha_stack = [];
      blend = Drawimage.Normal; blend_stack = [];
      epstransparent = true; epstransparent_stack = [];
      epsbygs = Gs.get_do_ps();
      epsbygs_stack = [];
      epswithantialiasing = true; epswithantialiasing_stack = [];
      direction = trans;
      transition = Transitions.TransNone;
      transition_stack = [];
      tpic_pensize = 0.0; tpic_path = []; tpic_shading = no_shading;
      status = (orid chst) status;
      headers = [];
      html = []; 
      checkpoint = 0;
    } in
  newpage [] st (mag *. dpi) xorig yorig;
  setup_bkgd st.status; (* Apply the background preferences in Dev, *)
  Dev.clear_dev ();     (* and redraw the background.               *)
  Dev.set_color st.color;
  Dev.set_transition st.transition;
  st.checkpoint <- 0;
  let check () =
    begin try Dev.continue () with
    | Dev.Stop as exn -> raise exn
    end;
    st.checkpoint <- checkpoint_frequency in
  let eval st x =
    st.checkpoint <- st.checkpoint - 1;
    let b = eval_command st x in
    if st.checkpoint < 0 then check ();
    b in
  Cdvi.page_step (eval st) page;;

let unfreeze_font cdvi n =
  try
    let cfont = Table.get cdvi.font_table n in
    ignore (Table.get cfont.mtable (Char.code 'A'))
  with _ -> ();;


let unfreeze_fonts cdvi =
  let font_map = cdvi.base_dvi.Cdvi.font_map in
  List.iter (fun (n, _) -> unfreeze_font cdvi n) font_map
    

let scan_special_pages cdvi lastpage =
  let headers = ref []
  and xrefs = cdvi.base_dvi.Cdvi.xrefs in
  let otherwise = ignore in
  for n = 0 to min lastpage (Array.length cdvi.base_dvi.Cdvi.pages) - 1 do
    ignore (scan_special_page otherwise cdvi (headers, xrefs) n);
  done;
  if !headers <> [] then Dev.add_headers (find_prologues !headers);;

let unfreeze_glyphs cdvi dpi =
  let mag = float cdvi.base_dvi.Cdvi.preamble.Dvicommands.pre_mag /. 1000.0 in
  let sdpi = Misc.round (mag *. ldexp dpi 16)
  and mtable = ref dummy_mtable
  and gtable = ref dummy_gtable in
  let otherwise = function
    | Dvicommands.C_fnt n ->
        let (mt, gt) =
          try
            let cfont = Table.get cdvi.font_table n in
            (cfont.mtable, get_gtable cfont sdpi)
          with Not_found -> (dummy_mtable, dummy_gtable) in
        mtable := mt;
        gtable := gt
    | Dvicommands.C_set code ->
        begin try ignore (Table.get !mtable code) with _ -> () end;
        begin try ignore (Table.get !gtable code) with _ -> () end
    | _ -> () in

  let headers = ref []
  and xrefs = cdvi.base_dvi.Cdvi.xrefs in
  let globals = headers, xrefs in
  for n = 0 to Array.length cdvi.base_dvi.Cdvi.pages - 1 do
    mtable := dummy_mtable;
    gtable := dummy_gtable;
    ignore (scan_special_page otherwise cdvi globals n);
  done;
  Dev.add_headers (find_prologues !headers);;
