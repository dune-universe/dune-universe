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

open Point_lib
open Matrix
module P = Picture_lib
module S = Spline_lib
open Dviinterp

let fprintf = Format.fprintf

let conversion = 0.3937 *. 72.

let point_of_cm cm = conversion *. cm

let float fmt f =
  (* PDF does not understand e notation, so we protect the printf which
     uses %g in the cases where this would use e notation; we do not need that
     much precision anyway*)
  let a = abs_float f in
  if classify_float f = FP_nan then
    (* should be an error there is a bug somewhere to track *)
    fprintf fmt "0"
  else if a < 0.0001 then fprintf fmt "0"
  else if a >= 1.e04 then fprintf fmt "%.4f" f
  else fprintf fmt "%.4g" f

let bp fmt f = Format.fprintf fmt "%a bp" float f

let rec list sep p fmt l =
  match l with
  | [] -> ()
  | [ x ] -> p fmt x
  | x :: xs ->
      p fmt x;
      sep fmt;
      list sep p fmt xs

let option sep p fmt o =
  match o with
  | None -> ()
  | Some x ->
      p fmt x;
      sep fmt

let nothing _ = ()

let space fmt = fprintf fmt "@ "

let braces p fmt x = fprintf fmt "{%a}" p x

module PGF = struct
  type line_cap = ButtCap | RoundCap | SquareCap

  let _ = ButtCap

  let _ = SquareCap

  type line_join = MiterJoin | RoundJoin | BevelJoin

  let _ = MiterJoin

  let _ = BevelJoin

  let point fmt p = fprintf fmt "\\pgfqpoint{%a}{%a}" bp p.x bp p.y

  let moveto fmt p = fprintf fmt "\\pgfpathmoveto{%a}" point p

  let lineto fmt p = fprintf fmt "\\pgfpathlineto{%a}" point p

  let curveto fmt p1 p2 p3 =
    fprintf fmt "\\pgfpathcurveto{%a}{%a}{%a}" point p1 point p2 point p3

  let close_path fmt = fprintf fmt "\\pgfpathclose"

  let stroke fmt = fprintf fmt "\\pgfusepath{stroke}"

  let fill fmt = fprintf fmt "\\pgfusepath{fill}"

  let clip fmt = fprintf fmt "\\pgfusepath{clip}"

  let gsave fmt = fprintf fmt "\\begin{pgfscope}"

  let grestore fmt = fprintf fmt "\\end{pgfscope}"

  let setlinewidth fmt f =
    (* handle strange treatment of linewidth of Metapost *)
    fprintf fmt "\\pgfsetlinewidth{%a}" bp f

  let setlinecap fmt c =
    let i =
      match c with
      | ButtCap -> "butt"
      | RoundCap -> "round"
      | SquareCap -> "rect"
    in
    fprintf fmt "\\pgfset%scap" i

  let setlinejoin fmt j =
    let i =
      match j with
      | MiterJoin -> "miter"
      | RoundJoin -> "round"
      | BevelJoin -> "bevel"
    in
    fprintf fmt "\\pgfset%sjoin" i

  let transform fmt t =
    if t = Matrix.identity then ()
    else
      fprintf fmt "\\pgflowlevel{\\pgftransformcm{%a}{%a}{%a}{%a}{%a}}" float
        t.xx float t.yx float t.xy float t.yy point { x = t.x0; y = t.y0 }

  let scolor_rgb fmt r g b =
    fprintf fmt "\\color[rgb]{%a,%a,%a}" float r float g float b

  let scolor_cmyk fmt c m y k =
    fprintf fmt "\\color[cmyk]{%a,%a,%a,%a}" float c float m float y float k

  let scolor_hsb fmt h s b =
    fprintf fmt "\\color[hsb]{%a,%a,%a}" float h float s float b

  let scolor_gray fmt c = fprintf fmt "\\color[gray]{%a}" float c

  let scolor fmt c =
    match c with
    | Concrete_types.RGB (r, g, b) -> scolor_rgb fmt r g b
    | Concrete_types.CMYK (c, m, y, k) -> scolor_cmyk fmt c m y k
    | Concrete_types.Gray c -> scolor_gray fmt c

  let color fmt c =
    match c with
    | Concrete_types.OPAQUE c -> scolor fmt c
    | Concrete_types.TRANSPARENT (f,c) ->
      fprintf fmt "\\pgfsetfillopacity{%a}\\pgfsetstrokeopacity{%a}%a" float f float f scolor c

  let dvi_color fmt c =
    match c with
    | Dviinterp.RGB (r, g, b) -> scolor_rgb fmt r g b
    | Dviinterp.CMYK (c, m, y, k) -> scolor_cmyk fmt c m y k
    | Dviinterp.HSB (h, s, b) -> scolor_hsb fmt h s b
    | Dviinterp.Gray g -> scolor_gray fmt g

  let dash fmt (offset, pattern) =
    fprintf fmt "\\pgfsetdash{%a}{%a}"
      (list nothing (braces bp))
      pattern bp offset

  let char_const fmt c = fprintf fmt "\\char'%03lo" c

  let glyph fmt p cl font =
    fprintf fmt
      "\\pgftext[base,left,at={%a}]{{\\font\\mlpostfont=%s at %apt \
       {\\mlpostfont %a}}}"
      point p (Fonts.tex_name font) float
      (Fonts.scale font conversion)
      (list nothing char_const) cl

  let glyphp p fmt (cl, font) = glyph fmt p cl font

  let rectangle fmt p w h =
    fprintf fmt "\\pgfpathrectangle{%a}{%a} %t" point p point { x = w; y = h }
      fill

  let rectanglep fmt (p, w, h) = rectangle fmt p w h
end

let in_context fmt f =
  fprintf fmt "@[<v>%t@, @[<v>%t@]@,%t@]" PGF.gsave f PGF.grestore

let fill_rect fmt trans i x y w h =
  let x = point_of_cm x
  and y = point_of_cm y
  and w = point_of_cm w
  and h = point_of_cm h in
  let p = { x; y } in
  in_context fmt (fun _ ->
      fprintf fmt "%a@ %a@ %a" PGF.transform trans PGF.dvi_color
        i.Dviinterp.color PGF.rectanglep (p, w, h))

let draw_char fmt trans text =
  (* FIXME why do we need to negate y coordinates? *)
  let f1, f2 = text.tex_pos in
  let f1 = point_of_cm f1 and f2 = point_of_cm f2 in
  let p = { x = f1; y = -. f2 } in
  in_context fmt (fun _ ->
      fprintf fmt "%a@ %a@ %a" PGF.transform trans PGF.dvi_color
        text.Dviinterp.tex_info.Dviinterp.color (PGF.glyphp p)
        (text.tex_string, text.tex_font))

(* FIXME why do we need to negate y coordinates? *)
let tex_cmd fmt trans c =
  match c with
  | Dviinterp.Fill_rect (i, x, y, w, h) -> fill_rect fmt trans i x (-.y) w h
  | Dviinterp.Draw_text text -> draw_char fmt trans text
  | Dviinterp.Specials _ -> ()
  | Dviinterp.Draw_text_type1 _ -> assert false

let draw_tex fmt t =
  list space (fun fmt x -> tex_cmd fmt t.Gentex.trans x) fmt t.Gentex.tex

let curveto fmt s =
  let sa, sb, sc, sd = Spline.explode s in
  if sa = sb && sc = sd then PGF.lineto fmt sd else PGF.curveto fmt sb sc sd

let path =
  let path fmt = function
    | S.Path p ->
        ( match p.S.pl with
        | [] -> assert false
        | x :: _ as l ->
            fprintf fmt "%a@ %a" PGF.moveto (Spline.left_point x)
              (list space curveto) l );
        if p.S.cycle then fprintf fmt " %t" PGF.close_path
    | S.Point p -> fprintf fmt "%a@ %a" PGF.moveto p PGF.lineto p
  in
  fun fmt p -> fprintf fmt "%a" path p

let pen fmt t =
  (* FIXME do something better *)
  (* for now assume that the pen is simply a scaled circle, so just grab the xx
   * value of the matrix and use that as linewidth *)
  PGF.setlinewidth fmt t.xx

let rec picture fmt p =
  match p with
  | P.Empty -> ()
  | P.OnTop l -> list space picture fmt l
  | P.Stroke_path (pa, clr, pe, da) ->
      in_context fmt (fun _ ->
          fprintf fmt "%a%a%a@ %a@ %t\n" (option space PGF.color) clr
            (option space PGF.dash) da pen pe path pa PGF.stroke)
  | P.Fill_path (p, clr) ->
      in_context fmt (fun _ ->
          fprintf fmt "%a%a@ %t\n" (option space PGF.color) clr path p PGF.fill)
  | P.Tex t -> draw_tex fmt t
  | P.Clip (com, p) ->
      in_context fmt (fun _ ->
          fprintf fmt "%a@ %t@ %a" path p PGF.clip picture com)
  | P.Transform (t, p) ->
      in_context fmt (fun _ -> fprintf fmt "%a@ %a" PGF.transform t picture p)
  | P.ExternalImage (f, _, t) ->
      in_context fmt (fun _ ->
          fprintf fmt "%a@ \\pgftext{\\includegraphics{%s}}" PGF.transform t f)

let draw fmt x =
  let fmt = Format.formatter_of_out_channel fmt in
  let p1, p2 = Picture_lib.bounding_box x in
  fprintf fmt "%%%%Creator: Mlpost %s@." Mlpost_version.version;
  fprintf fmt "\\begin{tikzpicture}@.";
  fprintf fmt "\\useasboundingbox (%a, %a) rectangle (%a, %a);@." bp p1.x bp
    p1.y bp p2.x bp p2.y;
  in_context fmt (fun _ ->
      fprintf fmt "%a@ %a@ %a@ %a" PGF.setlinewidth
        (P.default_line_size /. 2.)
        PGF.setlinecap PGF.RoundCap PGF.setlinejoin PGF.RoundJoin picture
        (P.content x));
  fprintf fmt "@.\\end{tikzpicture}@."

let generate_one fn fig =
  File.write_to fn (fun fmt ->
      let fig = Compute.commandpic fig in
      (*     Format.printf "picturelib code: \n %a@." P.Print.pic fig; *)
      draw fmt fig);
  fn

let mps figl =
  List.map
    (fun (fn, fig) ->
      let fn = File.mk fn "pgf" in
      (*     Format.printf "metapost code:\n %a@."Print.commandpic fig; *)
      generate_one fn fig)
    figl

let dump () = ignore (mps (Defaults.emited ()))

let generate figs = ignore (mps figs)
