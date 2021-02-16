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
open Concrete_types

let fprintf = Printf.fprintf

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
  else Printf.fprintf fmt "%.4g" f

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

let space fmt = fprintf fmt " "

type specials_env = {
  externalimages : (string * Matrix.t, int) Hashtbl.t;
  colors : (P.color, int) Hashtbl.t;
  count : int ref;
}

let new_specials_env () =
  {
    externalimages = Hashtbl.create 7;
    colors = Hashtbl.create 17;
    count = ref 0;
  }

module MPS = struct
  type line_cap = ButtCap | RoundCap | SquareCap

  let _ = ButtCap

  let _ = SquareCap

  type line_join = MiterJoin | RoundJoin | BevelJoin

  let _ = MiterJoin

  let _ = BevelJoin

  let moveto_float fmt x y = fprintf fmt "%a %a moveto" float x float y

  let lineto_float fmt x y = fprintf fmt "%a %a lineto" float x float y

  let rlineto_float fmt x y = fprintf fmt "%a %a rlineto" float x float y

  let lineto fmt p = lineto_float fmt p.x p.y

  let moveto fmt p = moveto_float fmt p.x p.y

  let rlineto fmt p = rlineto_float fmt p.x p.y

  let lineto_floatp fmt (x, y) = lineto_float fmt x y

  let curveto fmt p1 p2 p3 =
    fprintf fmt "%a %a %a %a %a %a curveto" float p1.x float p1.y float p2.x
      float p2.y float p3.x float p3.y

  let close_path fmt = fprintf fmt "close_path"

  let newpath fmt = fprintf fmt "newpath"

  let stroke fmt = fprintf fmt "stroke"

  let fill fmt = fprintf fmt "fill"

  let showpage fmt = fprintf fmt "showpage"

  let clip fmt = fprintf fmt "clip"

  let gsave fmt = fprintf fmt "gsave"

  let grestore fmt = fprintf fmt "grestore"

  let setlinewidth fmt f =
    (* handle strange treatment of linewidth of Metapost *)
    fprintf fmt "0 %a dtransform truncate idtransform setlinewidth pop" float f

  let setlinecap fmt c =
    let i = match c with ButtCap -> 0 | RoundCap -> 1 | SquareCap -> 2 in
    fprintf fmt "%d setlinecap" i

  let setlinejoin fmt j =
    let i = match j with MiterJoin -> 0 | RoundJoin -> 1 | BevelJoin -> 2 in
    fprintf fmt "%d setlinejoin" i

  let matrix fmt t =
    fprintf fmt "[%a %a %a %a %a %a]" float t.xx float t.yx float t.xy float
      t.yy float t.x0 float t.y0

  let transform fmt t =
    if t = Matrix.identity then () else fprintf fmt "%a concat" matrix t

  let scolor_rgb fmt r g b =
    fprintf fmt "%a %a %a setrgbcolor" float r float g float b

  let scolor_cmyk fmt c m y k =
    fprintf fmt "%a %a %a %a setcmykcolor" float c float m float y float k

  let scolor_gray fmt c = fprintf fmt "%a setgray" float c

  let scolor fmt c =
    match c with
    | Concrete_types.RGB (r, g, b) -> scolor_rgb fmt r g b
    | Concrete_types.CMYK (c, m, y, k) -> scolor_cmyk fmt c m y k
    | Concrete_types.Gray c -> scolor_gray fmt c

  let color fmt c =
    match c with
    | Concrete_types.OPAQUE c -> scolor fmt c
    | Concrete_types.TRANSPARENT _ ->
        (* harvest take care of that case *)
        assert false

  let dvi_color fmt c =
    match c with
    | Dviinterp.RGB (r, g, b) -> scolor_rgb fmt r g b
    | Dviinterp.CMYK (c, m, y, k) -> scolor_cmyk fmt c m y k
    | Dviinterp.HSB _ -> assert false
    | Dviinterp.Gray g -> scolor_gray fmt g

  let dash fmt (offset, pattern) =
    fprintf fmt "[%a ] %a setdash" (list space float) pattern float offset

  let char_const fmt c = fprintf fmt "\\%03lo" c

  let glyph fmt cl font =
    fprintf fmt "(%a) %s %a fshow" (list nothing char_const) cl
      (Fonts.tex_name font) float
      (Fonts.scale font conversion)

  let glyphp fmt (cl, font) = glyph fmt cl font

  let rectangle fmt p w h =
    fprintf fmt "%t %a %a %a %a %t %t" newpath moveto p lineto_floatp
      (p.x +. w, p.y)
      lineto_floatp
      (p.x +. w, p.y +. h)
      lineto_floatp
      (p.x, p.y +. h)
      close_path fill

  let rectanglep fmt (p, w, h) = rectangle fmt p w h
end

let in_context fmt f = fprintf fmt "%t %t %t" MPS.gsave f MPS.grestore

let fill_rect fmt trans i x y w h =
  let x = point_of_cm x
  and y = point_of_cm y
  and w = point_of_cm w
  and h = point_of_cm h in
  let p = { x; y } in
  in_context fmt (fun _ ->
      fprintf fmt "%a %a %a" MPS.transform trans MPS.dvi_color i.Dviinterp.color
        MPS.rectanglep (p, w, h))

let draw_char fmt trans text =
  (* FIXME why do we need to negate y coordinates? *)
  let f1, f2 = text.tex_pos in
  let f1 = point_of_cm f1 and f2 = point_of_cm f2 in
  let p = { x = f1; y = -.f2 } in
  in_context fmt (fun _ ->
      fprintf fmt "%a %a %a %a" MPS.transform trans MPS.dvi_color
        text.Dviinterp.tex_info.Dviinterp.color MPS.moveto p MPS.glyphp
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
  if sa = sb && sc = sd then MPS.lineto fmt sd else MPS.curveto fmt sb sc sd

let path =
  let path fmt = function
    | S.Path p ->
        ( match p.S.pl with
        | [] -> assert false
        | x :: _ as l ->
            fprintf fmt "%a %a" MPS.moveto (Spline.left_point x)
              (list space curveto) l );
        if p.S.cycle then fprintf fmt " %t" MPS.close_path
    | S.Point p -> fprintf fmt "%a %a" MPS.moveto p MPS.rlineto p
  in
  fun fmt p -> fprintf fmt "%t %a" MPS.newpath path p

let pen fmt t =
  (* FIXME do something better *)
  (* for now assume that the pen is simply a scaled circle, so just grab the xx
   * value of the matrix and use that as linewidth *)
  MPS.setlinewidth fmt t.xx

let specials_signal = 0.123

let specials_division = 1000.

(** map real color to encoded color :
    - identity for four first case
    - encoding for transparency
    - encoding specials rgb opaque color
*)
let add_color_se clr se =
  match clr with
  | None -> clr
  | Some (OPAQUE (Gray _)) -> clr
  | Some (OPAQUE (CMYK _)) -> clr
  | Some (OPAQUE (RGB (r, _, _))) when r <> specials_signal -> clr
  | Some clr ->
      let nb =
        try Hashtbl.find se.colors clr
        with Not_found ->
          incr se.count;
          let nb = !(se.count) in
          Hashtbl.add se.colors clr nb;
          nb
      in
      let nb = float_of_int nb /. specials_division in
      Some (OPAQUE (RGB (specials_signal, 0.003, nb)))

let add_image_se =
  let dumb_path =
    Spline_lib.create_lines
      [ Point_lib.zero; Point_lib.zero; Point_lib.zero; Point_lib.zero ]
  in
  let dumb_path = Spline_lib.close dumb_path in
  fun p se ->
    let nb =
      try Hashtbl.find se.externalimages p
      with Not_found ->
        incr se.count;
        let nb = !(se.count) in
        Hashtbl.add se.externalimages p nb;
        nb
    in
    (* 0.010? *)
    let nb = float_of_int nb /. specials_division in
    let c = Some (OPAQUE (RGB (specials_signal, 0.019, nb))) in
    P.Fill_path (dumb_path, c)

let rec harvest se = function
  | P.Empty as p -> p
  | P.OnTop l ->
      let add acc e =
        let p = harvest se e in
        if p = P.Empty then acc else p :: acc
      in
      let l = List.fold_left add [] l in
      if l = [] then P.Empty else P.OnTop (List.rev l)
  | P.Stroke_path (p, c, d, e) -> P.Stroke_path (p, add_color_se c se, d, e)
  | P.Fill_path (p, c) -> P.Fill_path (p, add_color_se c se)
  | P.Tex _ as p -> p
  | P.Transform (m, t) -> harvest se (P.apply_transform_cmds m t)
  | P.Clip (com, p) ->
      let com = harvest se com in
      if com = P.Empty then com else P.Clip (com, p)
  | P.ExternalImage (f, _, m) -> add_image_se (f, m) se

(*
For specials in mps
The specials are described at the bottom of the preamble
The first line describe the version, the special signal and the special_div
%%MetaPostSpecials: 2.0 123 1000
The next describe specials : length data special_number special_type
%%MetaPostSpecial: 7 1 0.5 1 0 0 1 3
Color cmyk  : 7 (cmyk_counter)                       c m y k special_number 1
Color spot  :                                                               2
Color rgba  : 7 mode_transparency value_transparency r g b   special_number 3
Color cmyka : 8 mode_transparency value_transparency c m y k special_number 4
Color spota : 8 mode_transparency value_transparency ? ? ? ? special_number 5

In the text they appear as color :
  special_signal (1 cmyk 2 spot 3 rgb) special_number
 0.123 0.003 0.001 setrgbcolor

*)

let print_specials_color =
  let pr_color fmt c =
    match c with
    | RGB (r, b, g) -> fprintf fmt "%f %f %f" r g b
    | Gray g -> fprintf fmt "%f %f %f" g g g
    | CMYK (c, m, y, k) -> fprintf fmt "%f %f %f %f" c m y k
  in
  fun fmt cl id ->
    let trans, c =
      match cl with OPAQUE c -> (1., c) | TRANSPARENT (a, c) -> (a, c)
    in
    let mode, special_type =
      match c with RGB _ | Gray _ -> (7, 3) | CMYK _ -> (8, 4)
    in
    fprintf fmt "%%%%MetaPostSpecial: ";
    fprintf fmt "%i 1 %f %a %i %i\n" mode trans pr_color c id special_type

let print_specials_extimg fmt (f, m) id =
  fprintf fmt "%%%%MetaPostSpecial: 9 %f %f %f %f %f %f %s %i 10\n" m.xx m.yx
    m.xy m.yy m.x0 m.y0 f id

let print_specials fmt cx =
  let se = new_specials_env () in
  let cx = harvest se cx in
  if Hashtbl.length se.colors <> 0 || Hashtbl.length se.externalimages <> 0 then (
    fprintf fmt "%%%%MetaPostSpecials: 2.0 %i %i\n"
      (int_of_float (specials_signal *. specials_division))
      (int_of_float specials_division);
    Hashtbl.iter (print_specials_color fmt) se.colors;
    Hashtbl.iter (print_specials_extimg fmt) se.externalimages );
  cx

let rec picture fmt p =
  match p with
  | P.Empty -> ()
  | P.OnTop l -> list space picture fmt l
  | P.Stroke_path (pa, clr, pe, da) ->
      in_context fmt (fun _ ->
          fprintf fmt "%a%a%a %a %t\n" (option space MPS.color) clr
            (option space MPS.dash) da pen pe path pa MPS.stroke)
  | P.Fill_path (p, clr) ->
      in_context fmt (fun _ ->
          fprintf fmt "%a %a %t\n" (option space MPS.color) clr path p MPS.fill)
  | P.Tex t -> draw_tex fmt t
  | P.Clip (com, p) ->
      in_context fmt (fun _ ->
          fprintf fmt "%a %t %a" path p MPS.clip picture com)
  | P.Transform _ | P.ExternalImage _ -> assert false

module BitMap = struct
  (* FIXME replace me by something more efficient *)

  (* encode our bitmap as a string (array of chars) *)
  type t = Bytes.t

  (* a char '0' corresponds to '0', a char '1' corresponds to '1' *)

  let mk n : t = Bytes.make n '0'

  let set t n = Bytes.set t n '1'

  let _get t n = t.[n]

  let min t = try Bytes.index t '1' with Not_found -> assert false

  let safe_sub_four s i =
    (* if the string does not have 4 remaining chars, pad with zeros *)
    let my_len = 4 in
    let l = Bytes.length s in
    if i + my_len <= l then Bytes.sub s i my_len
    else
      let buf = Bytes.make my_len '0' in
      for j = i to l - 1 do
        Bytes.set buf (j - i) (Bytes.get s j)
      done;
      buf

  let one_char t i =
    match Bytes.to_string (safe_sub_four t i) with
    | "0000" -> '0'
    | "0001" -> '1'
    | "0010" -> '2'
    | "0011" -> '3'
    | "0100" -> '4'
    | "0101" -> '5'
    | "0110" -> '6'
    | "0111" -> '7'
    | "1000" -> '8'
    | "1001" -> '9'
    | "1010" -> 'a'
    | "1011" -> 'b'
    | "1100" -> 'c'
    | "1101" -> 'd'
    | "1110" -> 'e'
    | "1111" -> 'f'
    | _ -> assert false

  let chars t =
    let b = Buffer.create 5 in
    let rec aux k =
      let c = one_char t k in
      if c = '0' then Buffer.contents b
      else (
        Buffer.add_char b c;
        aux (k + 4) )
    in
    let m = min t in
    Printf.sprintf "%x:%s" m (aux m)
end

(* FIXME do better than comparing font names *)
module FontCmp = struct
  type t = Fonts.t

  let compare a b = String.compare (Fonts.tex_name a) (Fonts.tex_name b)
end

(* module FS = Set.Make(FontCmp) *)
module FM = Map.Make (FontCmp)

let max_char f = (Fonts.metric f).Tfm.file_hdr.Tfm.ec

let fonts p =
  let x = ref FM.empty in
  Picture_lib.iter
    (fun p ->
      match p with
      | P.Tex g ->
          List.iter
            (fun c ->
              match c with
              | Draw_text text ->
                  let f = text.tex_font in
                  let map =
                    try FM.find f !x
                    with Not_found ->
                      let map = BitMap.mk (max_char f) in
                      x := FM.add f map !x;
                      map
                  in
                  List.iter
                    (fun x -> BitMap.set map (Int32.to_int x))
                    text.tex_string
              | _ -> ())
            g.Gentex.tex
      | _ -> ())
    p;
  !x

(* Following the dvips manual, for example on
 http://www.radicaleye.com/dvipsman/dvips.html#SEC34,
 the Font line looks as follows:
 %*Font: tfmname scaledbp designbp hex-start:hex-bitstring
 Here is the meaning of each of these elements:

tfmname
    The TeX TFM filename, e.g., `cmr10'. You can give the same tfmname on more
    than one `%*Font' line; this is useful when the number of characters from
    the font used needs a longer hex-bitstring (see item below) than
    conveniently fits on one line.
scaledbp
    The size at which you are using the font, in PostScript points (TeX big
    points). 72bp = 72.27pt = 1in.
designbp
    The designsize of the font, again in PostScript points. This should match
    the value in the TFM file tfmname. Thus, for `cmr10', it should be
    `9.96265'.
hex-start
    The character code of the first character used from the font, specified as
    two ASCII hexadecimal characters, e.g., `4b' or `4B' for `K'.
hex-bitstring
    An arbitrary number of ASCII hexadecimal digits specifying which characters
    following (and including) hex-start are used. This is treated as a bitmap.
    For example, if your figure used the single letter `K', you would use
    `4b:8' for hex-start and hex-bitstring. If it used `KLMNP', you would use
    `4b:f4'.
 *)
let fontdecl fmt f map =
  let n = Fonts.tex_name f in
  let d = Fonts.design_size f in
  let r = point_of_cm (Fonts.ratio_cm f) in
  let magic_string = BitMap.chars map in
  fprintf fmt "%%*Font: %s %f %f %s\n" n d r magic_string

let draw fmt x =
  let { x = minx; y = miny }, { x = maxx; y = maxy } =
    Picture_lib.bounding_box x
  in
  let minxt, minyt, maxxt, maxyt =
    (floor minx, floor miny, ceil maxx, ceil maxy)
  in
  fprintf fmt "%%!PS\n";
  fprintf fmt "%%%%BoundingBox: %f %f %f %f\n" minxt minyt maxxt maxyt;
  fprintf fmt "%%%%HiResBoundingBox: %f %f %f %f\n" minx miny maxx maxy;
  fprintf fmt "%%%%Creator: Mlpost %s\n" Mlpost_version.version;
  (* metapost adds a creation date but this breaks determinism *)
  (* fprintf fmt "%%%%CreationDate: %s\n" (Misc.date_string ()); *)
  fprintf fmt "%%%%Pages: 1\n";
  FM.iter (fontdecl fmt) (fonts x);
  fprintf fmt "%%%%BeginProlog\n";
  fprintf fmt "%%%%EndProlog\n";
  fprintf fmt "%%%%Page: 1 1\n";
  let cx = print_specials fmt (P.content x) in
  fprintf fmt "%a %a %a %a %t" MPS.setlinewidth
    (P.default_line_size /. 2.)
    MPS.setlinecap MPS.RoundCap MPS.setlinejoin MPS.RoundJoin picture cx
    MPS.showpage;
  fprintf fmt "\n%%%%EOF\n"

let generate_one fn fig =
  File.write_to fn (fun fmt ->
      let fig = Compute.commandpic fig in
      (*     Format.printf "picturelib code: \n %a@." P.Print.pic fig; *)
      draw fmt fig);
  fn

let mps figl =
  List.map
    (fun (fn, fig) ->
      let fn = File.mk fn "mps" in
      (*     Format.printf "metapost code:\n %a@."Print.commandpic fig; *)
      generate_one fn fig)
    figl

let dump () = ignore (mps (Defaults.emited ()))

let generate figs = ignore (mps figs)
