(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Johannes Kanig, Stephane Lescuyer                       *)
(*  Jean-Christophe Filliatre, Romain Bardou and Francois Bobot           *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

open Format
open Dvi_util
open Fonts

type color =
  | RGB of float * float * float
  | CMYK of float * float * float * float
  | HSB of float * float * float
  | Gray of float

(* a state can be pushed/ popped *)
type state = {
  h : int32;
  v : int32;
  w : int32;
  x : int32;
  y : int32;
  z : int32;
}

(* text *)

type env_info = { ei_conv : float; ei_pos : int32 * int32 }

type info = { color : color }

type text = {
  tex_font : Fonts.t;
  tex_string : Int32.t list;
  tex_pos : float * float;
  tex_info : info;
  tex_env : env_info;
}

type text_type1 = {
  c_glyph : Int32.t;
  c_font : Fonts.type1;
  c_pos : float * float;
  c_info : info;
}

type command =
  | Fill_rect of info * float * float * float * float
      (** [Fill_rect info x y w h] should draw a rectangle at [(x,y)] of
      width [w] and height [h]. *)
  | Draw_text of text
  | Specials of info * string * float * float
      (** [Specials info s x y] should draw special [s], encoded as a string,
      at pos. [(x,y)]. [info] can contain additional information
      such as color. *)
  | Draw_text_type1 of text_type1
      (** Can appear only after a decomposition of text *)

type page = command list

type env = {
  mutable ecolor : color;
  color_stack : color Stack.t;
  conv : float;
  mutable font : Int32.t;
  stack : state Stack.t;
  mutable s : state;
  use_type1 : bool;
}

let info_of_env env = { color = env.ecolor }

let new_env conv use_type1 =
  {
    ecolor = Gray 0.;
    conv;
    font = Int32.zero;
    stack = Stack.create ();
    s =
      {
        h = Int32.zero;
        v = Int32.zero;
        w = Int32.zero;
        x = Int32.zero;
        y = Int32.zero;
        z = Int32.zero;
      };
    color_stack = Stack.create ();
    use_type1;
  }

let rec scanf_with s def = function
  | [] -> def s
  | a :: l -> (
      try a s
      with Scanf.Scan_failure _ | Failure _ | End_of_file ->
        scanf_with s def l )

(* let print_state fmt s =
 *   fprintf fmt "{h = %ld; v = %ld; w = %ld; x = %ld; y = %ld; z= %ld}@."
 *     s.h s.v s.w s.x s.y s.z *)

let put_rule env cmds a b =
  let x = env.conv *. Int32.to_float env.s.h
  and y = env.conv *. Int32.to_float env.s.v
  and w = env.conv *. Int32.to_float b
  and h = env.conv *. Int32.to_float a in
  Fill_rect (info_of_env env, x, y -. h, w, h) :: cmds

let load_fonts font_map conv =
  if Defaults.get_debug () then printf "conv : %f@." conv;
  Int32Map.fold
    (fun k fdef -> Int32Map.add k (Fonts.load_font fdef conv))
    font_map Int32Map.empty

let set_env_char (_, font, _) (h, v) c =
  if Defaults.get_debug () then
    printf "Setting character %ld at (%ld,%ld).@." c h v;
  let fwidth = Fonts.char_width font (Int32.to_int c) in
  let width = Int32.of_float fwidth in
  (Int32.add h width, v)

let rec put_char_type1 (info, font, conv) char (h, v) cmds =
  match glyphs font with
  | VirtualFont vf ->
      (* FixMe how to use vf_design_size?
         Hack use the one of the first dvi, wrong!!*)
      if Defaults.get_debug () then
        printf "VirtualFont : %s ds=%f conv=%f@." (Fonts.tex_name font)
          vf.vf_design_size conv;
      let conv = (* vf.vf_design_size *) conv in
      let env = new_env conv true in
      env.s <- { env.s with h; v };
      (* FIXME design size, font_scale,... All is messed up... :( *)
      let fm =
        load_fonts vf.vf_font_map
          (conv (* for palatino (vf?)*) *. 0.65 (* experimental :( *))
      in
      let l =
        try Int32H.find vf.vf_chars char
        with Not_found -> failwith "virtual font not found"
      in
      interp_command fm env cmds l
  | Type1 font1 ->
      let x = conv *. Int32.to_float h and y = conv *. Int32.to_float v in
      if Defaults.get_debug () then
        printf "Type1 : %s,%ld,%f,%f@." (Fonts.tex_name font) char x y;
      let text_type1 =
        { c_glyph = char; c_font = font1; c_pos = (x, y); c_info = info }
      in
      Draw_text_type1 text_type1 :: cmds

and put_text (info, font, conv) scl ((h, v) as pos) cmds =
  let x = conv *. Int32.to_float h and y = conv *. Int32.to_float v in
  let text =
    {
      tex_font = font;
      tex_string = List.rev scl;
      tex_pos = (x, y);
      tex_info = info;
      tex_env = { ei_conv = conv; ei_pos = pos };
    }
  in
  Draw_text text :: cmds

and set_char fm env cmds l =
  let info = info_of_env env in
  let font = Int32Map.find env.font fm in
  let conv = env.conv in
  let ifc = (info, font, conv) in
  let pos = (env.s.h, env.s.v) in
  let cmds, (h, v), l =
    if env.use_type1 then set_char_type1 ifc pos cmds l
    else set_char_tex ifc [] pos pos cmds l
  in
  env.s <- { env.s with h; v };
  interp_command fm env cmds l

and set_char_type1 ifc pos cmds l =
  match l with
  | Dvi.SetChar c :: l ->
      let cmds = put_char_type1 ifc c pos cmds in
      let pos = set_env_char ifc pos c in
      (cmds, pos, l)
  | Dvi.PutChar c :: l ->
      if Defaults.get_debug () then printf "Putting character %ld.@." c;
      let cmds = put_char_type1 ifc c pos cmds in
      (cmds, pos, l)
  | _ -> assert false

and set_char_tex ifc scl pos_start pos_end cmds (l : Dvi.command list) =
  match l with
  | Dvi.SetChar c :: l ->
      let pos_end = set_env_char ifc pos_end c in
      set_char_tex ifc (c :: scl) pos_start pos_end cmds l
  | Dvi.PutChar c :: l ->
      if Defaults.get_debug () then printf "Putting character %ld.@." c;
      close_string ifc (c :: scl) pos_start pos_end cmds l
  | l -> close_string ifc scl pos_start pos_end cmds l

and close_string ifc scl pos_start pos_end cmds l =
  let cmds = put_text ifc scl pos_start cmds in
  (cmds, pos_end, l)

and interp_command fm env cmds l =
  match l with
  | [] -> cmds
  | (Dvi.SetChar _ | Dvi.PutChar _) :: _ -> set_char fm env cmds l
  | Dvi.SetRule (a, b) :: l ->
      if Defaults.get_debug () then printf "Setting rule (w=%ld, h=%ld).@." a b;
      let cmds = put_rule env cmds a b in
      env.s <- { env.s with h = Int32.add env.s.h b };
      interp_command fm env cmds l
  | Dvi.PutRule (a, b) :: l ->
      if Defaults.get_debug () then printf "Putting rule (w=%ld, h=%ld).@." a b;
      let cmds = put_rule env cmds a b in
      interp_command fm env cmds l
  | Dvi.Push :: l ->
      if Defaults.get_debug () then printf "Push current state.@.";
      Stack.push env.s env.stack;
      interp_command fm env cmds l
  | Dvi.Pop :: l ->
      ( try
          if Defaults.get_debug () then printf "Pop current state.@.";
          env.s <- Stack.pop env.stack
        with Stack.Empty -> failwith "Empty color stack !" );
      interp_command fm env cmds l
  | Dvi.Right b :: l ->
      if Defaults.get_debug () then printf "Moving right %ld.@." b;
      env.s <- { env.s with h = Int32.add env.s.h b };
      interp_command fm env cmds l
  | Dvi.Wdefault :: l ->
      if Defaults.get_debug () then printf "Moving right by the default W.@.";
      env.s <- { env.s with h = Int32.add env.s.h env.s.w };
      interp_command fm env cmds l
  | Dvi.W b :: l ->
      if Defaults.get_debug () then
        printf "Moving right and changing W to %ld.@." b;
      env.s <- { env.s with h = Int32.add env.s.h b; w = b };
      interp_command fm env cmds l
  | Dvi.Xdefault :: l ->
      if Defaults.get_debug () then printf "Moving right by the default X.@.";
      env.s <- { env.s with h = Int32.add env.s.h env.s.x };
      interp_command fm env cmds l
  | Dvi.X b :: l ->
      if Defaults.get_debug () then
        printf "Moving right and changing X to %ld.@." b;
      env.s <- { env.s with h = Int32.add env.s.h b; x = b };
      interp_command fm env cmds l
  | Dvi.Down a :: l ->
      if Defaults.get_debug () then printf "Moving down %ld.@." a;
      env.s <- { env.s with v = Int32.add env.s.v a };
      interp_command fm env cmds l
  | Dvi.Ydefault :: l ->
      if Defaults.get_debug () then printf "Moving down by the default Y.@.";
      env.s <- { env.s with v = Int32.add env.s.v env.s.y };
      interp_command fm env cmds l
  | Dvi.Y a :: l ->
      if Defaults.get_debug () then
        printf "Moving down and changing Y to %ld.@." a;
      env.s <- { env.s with v = Int32.add env.s.v a; y = a };
      interp_command fm env cmds l
  | Dvi.Zdefault :: l ->
      if Defaults.get_debug () then printf "Moving down by the default Z.@.";
      env.s <- { env.s with v = Int32.add env.s.v env.s.z };
      interp_command fm env cmds l
  | Dvi.Z a :: l ->
      if Defaults.get_debug () then
        printf "Moving down and changing Z to %ld.@." a;
      env.s <- { env.s with v = Int32.add env.s.v a; z = a };
      interp_command fm env cmds l
  | Dvi.FontNum f :: l ->
      env.font <- f;
      if Defaults.get_debug () then printf "Font is now set to %ld@." f;
      interp_command fm env cmds l
  | Dvi.Special xxx :: l ->
      if Defaults.get_debug () then printf "Special command : %s@." xxx;
      let x = env.conv *. Int32.to_float env.s.h
      and y = env.conv *. Int32.to_float env.s.v in
      let push color =
        Stack.push env.ecolor env.color_stack;
        env.ecolor <- color;
        cmds
      in
      let cmds =
        scanf_with xxx
          (fun s -> Specials (info_of_env env, s, x, y) :: cmds)
          [
            (fun s ->
              Scanf.sscanf s "color push rgb %f %f %f" (fun r g b ->
                  push (RGB (r, g, b))));
            (fun s ->
              Scanf.sscanf s "color push cmyk %f %f %f %f" (fun c m y k ->
                  push (CMYK (c, m, y, k))));
            (fun s ->
              Scanf.sscanf s "color push gray %f" (fun g -> push (Gray g)));
            (fun s ->
              Scanf.sscanf s "color push hsb %f %f %f" (fun h s b ->
                  push (HSB (h, s, b))));
            (fun s ->
              Scanf.sscanf s "color pop%n" (fun _ ->
                  env.ecolor <-
                    (* todo : color stack seems to traverse pages and so pop before push in one page *)
                    ( try Stack.pop env.color_stack
                      with Stack.Empty -> failwith "Empty color stack !" ));
              cmds);
          ]
      in
      interp_command fm env cmds l

let interp_page conv fm p =
  interp_command fm (new_env conv false) [] (Dvi.commands p)

let load_doc doc =
  let conv = Dvi.get_conv doc in
  let fonts = load_fonts (Dvi.fontmap doc) conv in
  let pages =
    List.fold_left
      (fun acc p ->
        if Defaults.get_debug () then printf "#### Starting New Page ####@."
        else if Defaults.get_verbosity () then printf ".";
        interp_page conv fonts p :: acc)
      [] (Dvi.pages doc)
  in
  List.rev pages

let load_file file =
  let doc = Dvi.read_file (File.to_string file) in
  if Defaults.get_verbosity () then
    printf "Dvi file parsing and interpretation :@.@?";
  let res = load_doc doc in
  if Defaults.get_verbosity () then printf " done@.@?";
  res

let decompose_text text =
  let font = text.tex_font in
  let pos = text.tex_env.ei_pos in
  if Defaults.get_debug () then
    printf "decompose text at (%ld,%ld)@." (fst pos) (snd pos);
  let info = text.tex_info in
  let ifc = (info, font, text.tex_env.ei_conv) in
  let fold (pos, cmds) c =
    let cmds = put_char_type1 ifc c pos cmds in
    (set_env_char ifc pos c, cmds)
  in
  let _, cmds = List.fold_left fold (pos, []) text.tex_string in
  cmds

module Incremental = struct
  let load_page i p =
    let conv = Dvi.Incremental.get_conv i in
    let fonts = load_fonts (Dvi.Incremental.font_map i) conv in
    interp_page conv fonts p
end

module Print = struct
  (* debug printing *)
  open Format

  let command fmt c =
    match c with
    | Fill_rect (_, x, y, w, h) -> fprintf fmt "rect(%f,%f,%f,%f)" x y w h
    | Draw_text text -> fprintf fmt "glyph (%s)" (Fonts.tex_name text.tex_font)
    | Specials _ -> assert false
    | Draw_text_type1 _ -> assert false

  let page fmt p =
    fprintf fmt "[< %a >]" (Misc.print_list Misc.newline command) p

  let dvi fmt d =
    Misc.print_list (fun fmt () -> fprintf fmt "<newpage>@\n@\n") page fmt d
end
