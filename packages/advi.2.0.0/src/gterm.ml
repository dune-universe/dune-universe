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

(* A simple terminal to handle simple editions and user's interaction. *)

type x = GraphicsY11.x
and y = GraphicsY11.y
and w = GraphicsY11.w
and h = GraphicsY11.h;;

type rectangle = GraphicsY11.rectangle = {x : x; y : y; w : w; h : h};;

type term = {
  (* Character contents of the terminal. *)
  mutable lines : bytes array;
  (* Height in lines. *)
  mutable height : int;
  (* Width in characters. *)
  mutable width : int;
  (* Height in pixels. *)
  mutable gheight : int;
  (* Width in pixels. *)
  mutable gwidth : int;
  (* Coordinates of the lower left corner of the terminal screen. *)
  mutable gx : int;
  mutable gy : int;
  (* Coordinates of the cursor in the char array. *)
  mutable cursor_x : int;
  mutable cursor_y : int;
  (* Colors *)
  mutable cursor_color : Graphics.color;
  mutable foreground_color : Graphics.color;
  mutable background_color : Graphics.color;
  mutable border_width_color : Graphics.color;
  mutable title_color : Graphics.color;
  (* Decorations *)
  mutable border_width : int;
  mutable title : string;
  (* Font *)
  mutable font : string;
  mutable font_size_x : int;
  mutable font_size_y : int;
  (* Excursion *)
  mutable excursion : GraphicsY11.rectangle;
};;

let get_gx t x = t.font_size_x * x + t.gx;;
let get_gy t y = t.font_size_y * y + t.gy;;

let cursor_gx t = get_gx t t.cursor_x;;
let cursor_gy t = get_gy t t.cursor_y;;

let draw_cursor t cc cf =
  Graphics.set_color cc;
  let gx = cursor_gx t
  and gy = cursor_gy t in 
  Graphics.fill_rect gx gy t.font_size_x t.font_size_y;
  let c = Bytes.get (t.lines.(t.cursor_y)) t.cursor_x in
  Graphics.moveto gx gy;
  Graphics.set_color cf;
  Graphics.draw_char c;
  Graphics.set_color t.foreground_color;;

let show_cursor t =
  draw_cursor t t.cursor_color t.background_color;;

let hide_cursor t =
  draw_cursor t t.background_color t.foreground_color;;

let htab t h =
  let e = t.excursion in
  let x0 = e.x in
  let xm = x0 + e.w - 1 in
  let h = min (max x0 h) xm in
  hide_cursor t;
  t.cursor_x <- h;
  show_cursor t;;
 
let vtab t v =
  let e = t.excursion in
  let y0 = e.y in
  let ym = y0 + e.h - 1 in
  let v = min (max y0 v) ym in
  hide_cursor t;
  t.cursor_y <- v;
  show_cursor t;;

let draw_border t =
  let w = t.border_width
  and h = t.border_width
  and gh = t.gheight
  and gw = t.gwidth
  and x = t.gx
  and y = t.gy in

  let draw_frame t =
    if t.border_width <> 0 then begin
      Graphics.set_color t.border_width_color;

      Graphics.fill_rect x (y - h) (gw + w) w;
      Graphics.fill_rect (x + gw) y w (gh + h);

      Graphics.fill_rect (x - w) (y - h) w (gh + h);
      Graphics.fill_rect (x - w) (y + gh) (gw + w) w
    end in

  let draw_title title =
    if title <> "" then begin
      let tx, ty = Graphics.text_size title in
      let hmargin = ((gw + 2 * w) - tx) / 2 in
      let vmargin = max 0 ((h - ty) / 2) in
      let title =
        if hmargin >= 0 then title
        else String.sub title 0 t.width in
      Graphics.moveto (x - w + max 0 hmargin) (y + gh + vmargin);
      Graphics.set_color t.title_color;
      Graphics.draw_string title
    end in

  draw_frame t;
  draw_title t.title;;

let draw_term t =
  Graphics.set_color t.background_color;
  Graphics.fill_rect t.gx t.gy t.gwidth t.gheight; 
  draw_border t;
  show_cursor t;;

let redraw t =
  draw_term t;
  Array.iteri
   (fun i s ->
      Graphics.moveto t.gx (t.gy + t.font_size_y * i);
      Graphics.draw_string (Bytes.to_string s))
   t.lines;
  show_cursor t;;

let clear t =
  Graphics.set_color t.background_color;
  Graphics.fill_rect
    t.gx t.gy (t.width * t.font_size_x) (t.height * t.font_size_y);
  Graphics.set_color t.foreground_color;
  Array.iter (fun s -> Bytes.fill s 0 (Bytes.length s) ' ') t.lines;
  htab t 0;
  vtab t (t.height - 1);;

let print_nl t = htab t 0; vtab t (t.cursor_y - 1);;

let rec print_str t s =
  hide_cursor t;
  let l = String.length s in
  let n =
   try String.index s '\n'
   with Not_found -> l in

  let ncursor_x = t.cursor_x + l in

  let print_it s =
    let gx = cursor_gx t 
    and gy = cursor_gy t in
    let l = String.length s in
    Graphics.moveto gx gy;
    Graphics.set_color t.background_color;
    Graphics.fill_rect gx gy (l * t.font_size_x) t.font_size_y;
    Graphics.set_color t.foreground_color;
    Graphics.draw_string s;
    String.blit s 0 t.lines.(t.cursor_y) t.cursor_x l;
    t.cursor_x <- t.cursor_x + l in

  let print_cont t scont =
    print_it scont;
    t.cursor_x <- 0;
    let v = (max 0 (t.cursor_y - 1)) mod t.height in
    t.cursor_y <- v in

  let print_endline =
    let endline = String.make 1 ' ' in
    fun t -> print_cont t endline
  and print_continue =
    let endline = String.make 1 '\\' in
    fun t -> print_cont t endline in

  begin
   if n < l then begin (* if we have newline character in s *)
     print_str t (String.sub s 0 n);
     print_endline t;
     if l - n > 1 then print_str t (String.sub s (n + 1) (l - n - 1));
   end else begin
     if ncursor_x < t.width then print_it s else
     begin
      let nbc = t.width - 1 - t.cursor_x in
      print_str t (String.sub s 0 nbc);
      print_continue t;
      print_str t (String.sub s nbc (l - nbc));
     end
   end
  end;
  show_cursor t
;;

let print_chr t c =
  match c with
  | '\n' -> print_nl t
  | _ -> print_str t (String.make 1 c);;

let make_term_gen fg bg bw bwc tc cc xc yc w h =
  if h < 1 || w < 1 || bw < 0 then failwith "make_term_gen" else
  let font_size_x, font_size_y = Graphics.text_size "M" in
  let t = {
    lines = Array.init h (fun i -> Bytes.make w ' ');
    height = h;
    width = w;
    gheight = h * font_size_y;
    gwidth = w * font_size_x;
    gx = xc;
    gy = yc;
    cursor_x = 0;
    cursor_y = h - 1;

    cursor_color = cc;
    foreground_color = fg;
    background_color = bg;
    border_width_color = bwc;
    border_width = bw;
    title = ""; (*Printf.sprintf "Gterm %ix%i" w h;*)
    title_color = tc;

    font = Printf.sprintf "%ix%i" font_size_x font_size_y;
    font_size_x = font_size_x;
    font_size_y = font_size_y;
    excursion = {x = 0; y = 0; w = w; h = h} 
   } in
  t;;

let get_excursion t = t.excursion;;
let reset_excursion t e x y =
  t.excursion <- e;
  htab t x;
  vtab t y;;

let set_excursion t x y w h =
  (* Validity verifications. *)
  (* x, y should be a valid character on the terminal. *)
  (* Rectangle x y w h is clipped to the terminal. *)
  if x < 0 || x > t.width ||
     y < 0 || y > t.height ||
     w < 0 || w > t.width ||
     h < 0 || h > t.height
  then failwith "set_excursion" else
  let w = min w (t.width - x)
  and h = min h (t.height - y) in
  let excursion = {x = x; y = y; w = w; h = h} in
  t.excursion <- excursion;
  htab t x;
  vtab t y;;

let save_excursion t x y w h f =
  let e0 = get_excursion t in
  let x0, y0 = t.cursor_x, t.cursor_y in
  try
   set_excursion t x y w h;
   let res = f t in
   reset_excursion t e0 x0 y0;
   res
  with exn -> reset_excursion t e0 x0 y0; raise exn;;

let rec set_title t s =
  let tx, ty = Graphics.text_size s in
  if tx <= t.gwidth then t.title <- s else
  (* String s is too long: truncate it to fit it in the terminal. *)
  let l = String.length s in
  let b = Buffer.create 30 in
  let rec loop i rest =
    if i < l then
    let c = s.[i] in
    let tx, ty = Graphics.text_size (String.make 1 c) in
    if tx <= rest then begin
      Buffer.add_char b c; loop (i + 1) (rest - tx)
    end in
  loop 0 t.gwidth;
  set_title t (Buffer.contents b);;

let make_term =
  make_term_gen
  Graphics.black Graphics.white
  10 Graphics.black Graphics.white
  0x6FFFFF;;

(* Basic functions to edit. *)
let end_of_line t =
  let line = t.lines.(t.cursor_y) in
  let l = Bytes.length line in
  htab t l;;

let beginning_of_line t = htab t 0;;
let forward_char t = htab t (t.cursor_x + 1);;
let backward_char t = htab t (t.cursor_x - 1);;
let next_line t = vtab t (t.cursor_y - 1);;
let previous_line t = vtab t (t.cursor_y + 1);;

(* To be improved : does not work in the midle of a line. *)
let backspace t =
 backward_char t;
 print_chr t ' ';
 backward_char t;;

(* Still to implement
    | '' -> delete_char t
let delete_char t =
  let line = t.lines.(t.cursor_y) in

    | '' -> kill_line t
let kill_line t =
 print_str t " ";;
    | '' -> scroll_one_line_down t
let scroll_one_line_down t =
    | 'z' -> scroll_one_line_down t
let scroll_one_line_up t =
    | 'v' -> scroll_one_window_down t
let scroll_one_window_up t =
    | '' -> scroll_one_window_down t
let scroll_one_window_down t =
*)

let rec get_next_key t =
  GraphicsY11.synchronize ();
  let c = GraphicsY11.read_key () in
  match c with
  | '' -> beginning_of_line t; get_next_key t
  | '' -> backward_char t; get_next_key t
  | '' -> end_of_line t; get_next_key t
  | '' -> forward_char t; get_next_key t
  | '' -> redraw t; get_next_key t
  | c -> c;;

let rec edit t =
  begin match get_next_key t with
  | '' | '' -> backspace t
  | '' -> next_line t
  | '' -> previous_line t
  | '\n' | '\r' -> print_nl t
  | c -> print_chr t c end;
  edit t;;

(* Basic functions to input strings. *)
let rec flush_keys () =
  if GraphicsY11.key_pressed () then
    let _c = GraphicsY11.read_key () in
    flush_keys ();;

type prompt = string;;
type prefill = string;;

let get_line_prefill =
  let b = Buffer.create 11 in
  let get t =
    flush_keys ();
    let limx = t.cursor_x in
    let rec read t =
      let c = get_next_key t in
      match c with
      | '\n' | '\r' | '' ->
         print_nl t;
         let s = Buffer.contents b in
         Buffer.clear b;
         s
      | '' | '' ->
          if t.cursor_x = limx then () else
           begin
             let s = Buffer.contents b in
             Buffer.clear b;
             let nl = String.length s - 1 in
             if nl > 0 then Buffer.add_string b (String.sub s 0 nl);
             backspace t;
           end;
          read t
      | '' ->
          Buffer.clear b;
          while t.cursor_x > limx do backspace t done;
          read t
      | '' | '' | '' ->
         Buffer.clear b;
         ""
      | c ->
         Buffer.add_char b c;
         print_chr t c;
         read t in
    read in
  (fun t prefill ->
     Buffer.clear b;
     Buffer.add_string b prefill;
     print_str t prefill;
     let read = get t in
     save_excursion t t.cursor_x t.cursor_y (t.width - t.cursor_x) 1
       read);;

let ask_prefill t prompt prefill =
  print_str t prompt;
  let answer = get_line_prefill t prefill in
  flush_keys ();
  answer;;

let get_line t = get_line_prefill t "";;

let ask t prompt = ask_prefill t prompt "";;
